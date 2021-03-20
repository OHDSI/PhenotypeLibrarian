####################
# Create Cohorts from templates
##############################

todaysDate <- stringr::str_replace_all(string = lubridate::as_date(lubridate::now()), 
                                       pattern = stringr::fixed("-"), 
                                       replacement = "") 
library(PhenotypeLibrarian)
conceptSetSignature <- readRDS(
  file = file.path(
    projectFolder,
    "inst",
    "ConceptSets",
    "conceptSetSignature.rds"
  )
) %>%
  dplyr::filter(countConceptInDomainCondition > 0 |
                  countConceptInDomainObservation > 0 |
                  countConceptInDomainProcedure > 0) %>%
  dplyr::filter(numberOfUniqueDomains <= 3) %>%
  dplyr::rename(referentConceptId = referentConceptIdRecommended1,
                referentConceptName = referentConceptNameRecommended1) %>%
  dplyr::select(conceptSetUniqueId, conceptSetExpressionJson, conceptSetExpressionName, referentConceptId, referentConceptName) %>%
  dplyr::arrange(.data$conceptSetUniqueId)


## Set up -----------------------------
library(Capr)
library(CirceR)
library(DatabaseConnector)
library(magrittr)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                user = Sys.getenv("shinyDbUserGowtham"),
                                                                password = Sys.getenv("shinyDbPasswordGowtham"),
                                                                server = paste(Sys.getenv("shinydbServer"),
                                                                               Sys.getenv("shinydbDatabase"),
                                                                               sep = "/"),
                                                                port = "5432",
                                                                extraSettings = NULL,
                                                                oracleDriver = "thin")
vocabularyDatabaseSchema <- "vocabulary"
oracleTempSchema <- NULL


connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

#create Inpatient Visit Group
#either create it here or use an existing version of IP visit
IpOrIpErVisitQuery <- Capr::getConceptIdDetails(conceptIds = c(262,9201), # inpatient or Inpatient ER
                                                connection = connection,
                                                vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                mapToStandard = TRUE) %>%
  Capr::createConceptSetExpression(conceptSet = .,
                                   Name = "Inpatient Or ER & Inpatient Visit",
                                   includeDescendants = TRUE) %>%
  Capr::createVisitOccurrence(conceptSetExpression = .)

timelineOverlap <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = "All", StartCoeff = "Before",
                                                                         EndDays = 0, EndCoeff = "After"),
                                        EndWindow = Capr::createWindow(StartDays = 0, StartCoeff = "Before",
                                                                       EndDays = "All", EndCoeff = "After",
                                                                       EventStarts = FALSE))
IPVisitCount <- Capr::createCount(Query = IpOrIpErVisitQuery,
                                  Logic = "at_least",
                                  Count = 1,
                                  Timeline = timelineOverlap)
IPVisit <- Capr::createGroup(Name = "Inpatient Visit Occurrence",
                             type = "ALL",
                             criteriaList = list(IPVisitCount))

dateAttributeStartDateAfterICD10 <- Capr::createOccurrenceStartDateAttribute(Op = '>=', Value = '2015-10-01')
dateAttributeStartDateBeforeICD10 <- Capr::createOccurrenceStartDateAttribute(Op = '<=', Value = '2015-09-30')

dateRange <- c('all', 'icd9', 'icd10')

# generation options
####################################################
#Create Phenotypes
#######################################################
counter <- 0

########################
# Template 1
#all events of concept set with no such events in prior clean window
#period (365 days). No continuous observation period requirement.
#Persons exit the cohort on start_date + 1day
##################################
cohortTemplate1 <- list()
for (i in (1:nrow(conceptSetSignature))) {
  for (j in (1:length(dateRange))) {
    
    logicDescriptionTemplate <- paste0("All events of ", 
                                       "TEMPLATETEMPLATE",
                                       " with no such events in prior clean window period (365 days). No continuous observation period requirement. Persons exit the cohort on start_date + 1day.")
    cohortName <- "TEMPLATETEMPLATE all events with prior clean period"
    
    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else if (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD9CM period of on or before 2015-09-30.")
    } else if (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD10CM period of on or after 2015-10-01.")
    }
    
    templateType <- 'template1'
    templateTypeAbbreviation <- paste('T1', toupper(dateRange[[j]]), sep = " ")
    genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id", #change me
                                           cohortId = counter,
                                           cdmSchema = "@cdm_database_schema", #change me
                                           targetTable = "@target_cohort_table", #change me
                                           resultSchema = "@target_database_schema", #change me
                                           vocabularySchema = "@vocabulary_database_schema",
                                           generateStats = TRUE)
    rendered <- PhenotypeLibrarian::generateConditionObservationProcedurePhenotypes(genOp = genOp,
                                                                                    name = conceptSetSignature$conceptSetExpressionName[i],
                                                                                    cseJson = conceptSetSignature$conceptSetExpressionJson[i],
                                                                                    templateFn = templateType,
                                                                                    occurrenceStartDateAttribute = occurrenceStartDateAttribute)
    if (length(rendered) == 3) {
      counter <- counter + 1
      circeJson = rendered$circeJson %>% RJSONIO::fromJSON(digits = 23) %>% RJSONIO::toJSON(digits = 23, pretty = TRUE)
      logicDescription <- paste0(templateTypeAbbreviation, 
                                 " ",
                                 stringr::str_replace(string = logicDescriptionTemplate,
                                               pattern = 'TEMPLATETEMPLATE', 
                                               replacement = conceptSetSignature$conceptSetExpressionName[i]))
      cohortName <- paste0('[PL ',
                           counter,
                           ']',
                           '-',
                           templateTypeAbbreviation, 
                           ' ',
                           stringr::str_replace(string = cohortName,
                                                pattern = 'TEMPLATETEMPLATE', 
                                                replacement = conceptSetSignature$referentConceptName[i]))
      
      cohortTemplate1[[counter]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            templateTypeAbbreviation = !!templateTypeAbbreviation,
                                            dateRange = dateRange[[j]],
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i],
                                            logicDescription = !!logicDescription,
                                            cohortName = !!cohortName)
    } else {
      counter <- counter + 1
      ParallelLogger::logWarn(paste0("Skipping over cohort id: ", counter, " ", conceptSetSignature$referentConceptName[i]))
    }
  }
}
cohortTemplate1 <- dplyr::bind_rows(cohortTemplate1)


########################
# Template 2
#all events of concept set with overlapping inpatient visit
#with no such events in prior clean window period (365 days).
#no continuous observation period requirement Person exit the cohort
##################################
cohortTemplate2 <- list()
for (i in (1:nrow(conceptSetSignature))) {
  for (j in (1:length(dateRange))) {
    
    logicDescriptionTemplate <- paste0("All events of ", 
                                       "TEMPLATETEMPLATE",
                                       " with overlapping inpatient visit",
                                       " with no such events in prior clean window period (365 days). No continuous observation period requirement. Persons exit the cohort on start_date + 1day.")
    cohortName <- "TEMPLATETEMPLATE all events with overlapping inpatient visit and prior clean period"
    
    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else if (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD9CM period of on or before 2015-09-30.")
    } else if (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD10CM period of on or after 2015-10-01.")
    }
    
    templateType <- 'template2'
    templateTypeAbbreviation <- paste('T2', toupper(dateRange[[j]]), sep = " ")
    genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id", #change me
                                           cohortId = counter,
                                           cdmSchema = "@cdm_database_schema", #change me
                                           targetTable = "@target_cohort_table", #change me
                                           resultSchema = "@target_database_schema", #change me
                                           vocabularySchema = "@vocabulary_database_schema",
                                           generateStats = TRUE)
    rendered <- generateConditionObservationProcedurePhenotypes(genOp = genOp,
                                                                name = conceptSetSignature$conceptSetExpressionName[i],
                                                                cseJson = conceptSetSignature$conceptSetExpressionJson[i],
                                                                templateFn = templateType,
                                                                IPVisit = IPVisit,
                                                                occurrenceStartDateAttribute = occurrenceStartDateAttribute)
    if (length(rendered) == 3) {
      counter <- counter + 1
      circeJson = rendered$circeJson %>% RJSONIO::fromJSON(digits = 23) %>% RJSONIO::toJSON(digits = 23, pretty = TRUE)
      logicDescription <- paste0(templateTypeAbbreviation, 
                                 " ",
                                 stringr::str_replace(string = logicDescriptionTemplate,
                                                      pattern = 'TEMPLATETEMPLATE', 
                                                      replacement = conceptSetSignature$conceptSetExpressionName[i]))
      cohortName <- paste0('[PL ',
                           counter,
                           ']',
                           '-',
                           templateTypeAbbreviation, 
                           ' ',
                           stringr::str_replace(string = cohortName,
                                                pattern = 'TEMPLATETEMPLATE', 
                                                replacement = conceptSetSignature$referentConceptName[i]))
      
      cohortTemplate2[[counter]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            templateTypeAbbreviation = !!templateTypeAbbreviation,
                                            dateRange = dateRange[[j]],
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i],
                                            logicDescription = !!logicDescription,
                                            cohortName = !!cohortName)
    } else {
      counter <- counter + 1
      ParallelLogger::logWarn(paste0("Skipping over cohort id: ", counter, " ",  conceptSetSignature$referentConceptName[i]))
    }
  }
}
cohortTemplate2 <- dplyr::bind_rows(cohortTemplate2)


########################
# Template 3
#First occurrence of concept set event, first time in person history
#minimum prior observation period of 365 days, follow-up till end of observaiton period
##################################
cohortTemplate3 <- list()
for (i in (1:nrow(conceptSetSignature))) {
  for (j in (1:length(dateRange))) {
    
    logicDescriptionTemplate <- paste0("First occurrence of ", 
                                       "TEMPLATETEMPLATE",
                                       " event, first time in persons history",
                                       " with minimum prior observation period of 365 days.",
                                       " Persons are followed up till end of Observation period.")
    cohortName <- "TEMPLATETEMPLATE first time in history with 365 days minimum prior observation"
    
    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else if (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD9CM period of on or before 2015-09-30.")
    } else if (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD10CM period of on or after 2015-10-01.")
    }
    
    templateType <- 'template3'
    templateTypeAbbreviation <- paste('T3', toupper(dateRange[[j]]), sep = " ")
    genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id", #change me
                                           cohortId = counter,
                                           cdmSchema = "@cdm_database_schema", #change me
                                           targetTable = "@target_cohort_table", #change me
                                           resultSchema = "@target_database_schema", #change me
                                           vocabularySchema = "@vocabulary_database_schema",
                                           generateStats = TRUE)
    rendered <- generateConditionObservationProcedurePhenotypes(genOp = genOp,
                                                                name = conceptSetSignature$conceptSetExpressionName[i],
                                                                cseJson = conceptSetSignature$conceptSetExpressionJson[i],
                                                                templateFn = templateType,
                                                                occurrenceStartDateAttribute = occurrenceStartDateAttribute)
    if (length(rendered) == 3) {
      counter <- counter + 1
      circeJson = rendered$circeJson %>% RJSONIO::fromJSON(digits = 23) %>% RJSONIO::toJSON(digits = 23, pretty = TRUE)
      logicDescription <- paste0(templateTypeAbbreviation, 
                                 " ",
                                 stringr::str_replace(string = logicDescriptionTemplate,
                                                      pattern = 'TEMPLATETEMPLATE', 
                                                      replacement = conceptSetSignature$conceptSetExpressionName[i]))
      cohortName <- paste0('[PL ',
                           counter,
                           ']',
                           '-',
                           templateTypeAbbreviation, 
                           ' ',
                           stringr::str_replace(string = cohortName,
                                                pattern = 'TEMPLATETEMPLATE', 
                                                replacement = conceptSetSignature$referentConceptName[i]))
      
      cohortTemplate3[[counter]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            templateTypeAbbreviation = !!templateTypeAbbreviation,
                                            dateRange = dateRange[[j]],
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i],
                                            logicDescription = !!logicDescription,
                                            cohortName = !!cohortName)
    } else {
      counter <- counter + 1
      ParallelLogger::logWarn(paste0("Skipping over cohort id: ", counter, " ",  conceptSetSignature$referentConceptName[i]))
    }
  }
}
cohortTemplate3 <- dplyr::bind_rows(cohortTemplate3)

########################
# Template 4
#first occurrence of concept set event, first time in persons history
#no prior observation period requirement, follow up till end of observation period
##################################
cohortTemplate4 <- list()
for (i in (1:nrow(conceptSetSignature))) {
  for (j in (1:length(dateRange))) {
    
    logicDescriptionTemplate <- paste0("First occurrence of ", 
                                       "TEMPLATETEMPLATE",
                                       " event, first time in persons history",
                                       " no prior minimum observation period requirement.",
                                       " Persons are followed up till end of Observation period.")
    cohortName <- "TEMPLATETEMPLATE first time in history"
    
    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else if (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD9CM period of on or before 2015-09-30.")
    } else if (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
      logicDescriptionTemplate <- paste0(logicDescriptionTemplate, 
                                         " Index date is limited to ICD10CM period of on or after 2015-10-01.")
    }
    
    templateType <- 'template4'
    templateTypeAbbreviation <- paste('T4', toupper(dateRange[[j]]), sep = " ")
    genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id", #change me
                                           cohortId = counter,
                                           cdmSchema = "@cdm_database_schema", #change me
                                           targetTable = "@target_cohort_table", #change me
                                           resultSchema = "@target_database_schema", #change me
                                           vocabularySchema = "@vocabulary_database_schema",
                                           generateStats = TRUE)
    rendered <- generateConditionObservationProcedurePhenotypes(genOp = genOp,
                                                                name = conceptSetSignature$conceptSetExpressionName[i],
                                                                cseJson = conceptSetSignature$conceptSetExpressionJson[i],
                                                                templateFn = templateType,
                                                                occurrenceStartDateAttribute = occurrenceStartDateAttribute)
    if (length(rendered) == 3) {
      counter <- counter + 1
      circeJson = rendered$circeJson %>% RJSONIO::fromJSON(digits = 23) %>% RJSONIO::toJSON(digits = 23, pretty = TRUE)
      logicDescription <- paste0(templateTypeAbbreviation, 
                                 " ",
                                 stringr::str_replace(string = logicDescriptionTemplate,
                                                      pattern = 'TEMPLATETEMPLATE', 
                                                      replacement = conceptSetSignature$conceptSetExpressionName[i]))
      cohortName <- paste0('[PL ',
                           counter,
                           ']',
                           '-',
                           templateTypeAbbreviation, 
                           ' ',
                           stringr::str_replace(string = cohortName,
                                                pattern = 'TEMPLATETEMPLATE', 
                                                replacement = conceptSetSignature$referentConceptName[i]))
      
      cohortTemplate4[[counter]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            templateTypeAbbreviation = !!templateTypeAbbreviation,
                                            dateRange = dateRange[[j]],
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i],
                                            logicDescription = !!logicDescription,
                                            cohortName = !!cohortName)
    } else {
      counter <- counter + 1
      ParallelLogger::logWarn(paste0("Skipping over cohort id: ", counter, " ",  conceptSetSignature$referentConceptName[i]))
    }
  }
}
cohortTemplate4 <- dplyr::bind_rows(cohortTemplate4)

cohortTemplate <- dplyr::bind_rows(cohortTemplate1, cohortTemplate2, cohortTemplate3, cohortTemplate4) %>%
  dplyr::arrange(.data$cohortId)


saveRDS(object = cohortTemplate,
        file.path(rstudioapi::getActiveProject(), 'inst', 'CohortTemplates', paste0('Cohorts', todaysDate  , '.rds')))



baseUrl <- Sys.getenv('BaseUrl')



data <- list()
for (i in (1:nrow(cohortTemplate))) {
  row <- cohortTemplate[i,]
  cohortOhdsiSqlFromWebApi <-
    ROhdsiWebApi::getCohortSql(baseUrl = baseUrl,
                               cohortDefinition = row$cohortCirceJsonFromCapr %>% RJSONIO::fromJSON(digits = 23),
                               generateStats = TRUE)
  row$cohortOhdsiSqlFromWebApi <- cohortOhdsiSqlFromWebApi
  data[[i]] <- row
}
data <- dplyr::bind_rows(data) %>%
  dplyr::arrange(.data$cohortId)

saveRDS(object = data,
        file.path(rstudioapi::getActiveProject(), 'inst', 'Cohorts', paste0('Cohorts', todaysDate  , '.rds')))


