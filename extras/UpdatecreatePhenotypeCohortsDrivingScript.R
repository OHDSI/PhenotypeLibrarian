####################
# Create Cohorts from templates
##############################


conceptSetSignature <- readRDS(
  file = file.path(
    projectFolder,
    "preprocessed",
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
library(PhenotypeLibrary)

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

    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else if (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
    } else if (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
    }

    templateType <- 'template1'
    genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id", #change me
                                           cohortId = counter,
                                           cdmSchema = "@cdm_database_schema", #change me
                                           targetTable = "@target_cohort_table", #change me
                                           resultSchema = "@target_database_schema", #change me
                                           vocabularySchema = "@vocabulary_database_schema",
                                           generateStats = TRUE)
    rendered <- PhenotypeLibrary::generateConditionObservationProcedurePhenotypes(genOp = genOp,
                                                                                  name = conceptSetSignature$conceptSetExpressionName[i],
                                                                                  cseJson = conceptSetSignature$conceptSetExpressionJson[i],
                                                                                  templateFn = templateType,
                                                                                  occurrenceStartDateAttribute = occurrenceStartDateAttribute)
    if (length(rendered) == 3) {
      counter <- counter + 1
      circeJson = rendered$circeJson %>% RJSONIO::fromJSON(digits = 23) %>% RJSONIO::toJSON(digits = 23, pretty = TRUE)
      cohortTemplate1[[i]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i])
    } else {
      ParallelLogger::logWarn(paste0("Skipping over ", i, " ", conceptSetSignature$referentConceptName[i]))
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

    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
    } else (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
    }

    templateType <- 'template2'
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
      cohortTemplate2[[i]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i])
    } else {
      ParallelLogger::logWarn(paste0("Skipping over ", i, " ",  conceptSetSignature$referentConceptName[i]))
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

    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
    } else (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
    }

    templateType <- 'template3'
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
      cohortTemplate3[[i]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i])
    } else {
      ParallelLogger::logWarn(paste0("Skipping over ", i, " ",  conceptSetSignature$referentConceptName[i]))
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

    if (dateRange[[j]] == 'all') {
      occurrenceStartDateAttribute <- NULL
    } else (dateRange[[j]] == 'icd9') {
      occurrenceStartDateAttribute <- dateAttributeStartDateBeforeICD10
    } else (dateRange[[j]] == 'icd10') {
      occurrenceStartDateAttribute <- dateAttributeStartDateAfterICD10
    }

    templateType <- 'template4'
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
      cohortTemplate4[[4]] <- dplyr::tibble(cohortId = counter,
                                            conceptSetUniqueId = conceptSetSignature$conceptSetUniqueId[i],
                                            cohortCirceJsonFromCapr = circeJson,
                                            cohortHumanReadable = rendered$cohortRead,
                                            cohortOhdsiSqlFromCapr = rendered$ohdiSQL,
                                            templateType = !!templateType,
                                            conceptSetReferentConceptId = conceptSetSignature$referentConceptId[i],
                                            conceptSetReferentName = conceptSetSignature$referentConceptName[i],
                                            conceptSetName = conceptSetSignature$conceptSetExpressionName[i])
    } else {
      ParallelLogger::logWarn(paste0("Skipping over ", i, " ",  conceptSetSignature$referentConceptName[i]))
    }
  }
}
cohortTemplate4 <- dplyr::bind_rows(cohortTemplate4)

cohortTemplate <- dplyr::bind_rows(cohortTemplate1, cohortTemplate2, cohortTemplate3, cohortTemplate4) %>%
  dplyr::arrange(.data$cohortId)


saveRDS(object = cohortTemplate,
        file.path(rstudioapi::getActiveProject(), 'inst', 'CohortTemplates', 'Cohorts20210306.rds'))



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
        file.path(rstudioapi::getActiveProject(), 'inst', 'CohortTemplates', 'Cohorts20210315.rds'))


