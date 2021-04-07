library(magrittr)

assign(x = "appVersion",value = paste0("V 2.0", "  (",
                                       lubridate::now(tzone = "EST"),
                                       " EST)"), envir = .GlobalEnv)
assign(x = "appSignatureValue",
       value = Sys.getenv("PhenotypeLibrarianAppSignatureValue"), 
       envir = .GlobalEnv)

assign("username", Sys.getenv("charybdisdbUser"), envir = .GlobalEnv)
assign("password", Sys.getenv("charybdisdbPw"), envir = .GlobalEnv)
assign("dbms", 'postgres', envir = .GlobalEnv)
assign("server", paste(Sys.getenv("shinydbServer"),
                       Sys.getenv("shinydbDatabase"),
                       sep = "/"),
       envir = .GlobalEnv)
assign("vocabularyDatabaseSchema",
       'vocabulary',
       envir = .GlobalEnv)
assign("resultsDatabaseSchema",
       'aesi20210403',
       envir = .GlobalEnv)
# assign("resultsDatabaseSchema",
#        'eunomia',
#        envir = .GlobalEnv)


source("R/DisplayFunctions.R")
source("R/Tables.R")
source("R/Plots.R")
source("R/Results.R")
source("R/Connections.R")
source("R/HelperFunctions.R")
source("R/ModifyDataSource.R")
source("R/AnnotationService.R")

assign(x = "dbms", value = "postgresql", envir = .GlobalEnv)
assign(x = "port", value = 5432, envir = .GlobalEnv)

# default app titles and text
assign(x = "appTitle", value = "Phenotype Library", envir = .GlobalEnv)
source("html/defaultAboutTextPhenotypeLibrary.txt")
aboutText <- defaultAboutTextPhenotypeLibrary

# Cleaning up any tables in memory:
dataModelSpecifications <-
  readr::read_csv(
    file = "resultsDataModelSpecification.csv",
    col_types = readr::cols(),
    guess_max = min(1e7)
  )
suppressWarnings(rm(list = snakeCaseToCamelCase(dataModelSpecifications$tableName)))

userPriorityFile <- readr::read_csv(file = 'UserPriorityFile.csv',
                                col_types = readr::cols())


assign(x = "annotationPermission",
       value = Sys.getenv("PhenotypeLibrarianAnnoationPermission"), 
       envir = .GlobalEnv)
assign(x = "annotationService",
       value = Sys.getenv("PhenotypeLibrarianAnnoationServiceProvider"), 
       envir = .GlobalEnv)



if (annotationService == "GoogleSheets") {
  # get service account authorization token
  googlesheets4::gs4_auth(path = Sys.getenv('PhenotypeLibrarianAuthorizationJson'))
}


# connection to database
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    port = port,
    user = username,
    password = password
  )
connectionIsValid <- try(isConnectionValid(
  dbms = dbms,
  server = server,
  port = port,
  username = username,
  password = password
))
assign(x = "isValidConnection",
       value = connectionIsValid,
       envir = .GlobalEnv)
connectionPool <- NULL
writeLines(text = "Connecting to Pool.")
connectionPool <- pool::dbPool(
  drv = DatabaseConnector::DatabaseConnectorDriver(),
  dbms = dbms,
  server = server,
  port = port,
  user = username,
  password = password
)
writeLines("Connected to database.")


# Cleanup connection when the application stops
shiny::onStop(function() {
  if (isValidConnection) {
    writeLines(text = "Closing database connections")
    if (DBI::dbIsValid(dbObj = connectionPool)) {
      pool::poolClose(pool = connectionPool)
    }
  }
})


loadTimeStart <- Sys.time()
if (isValidConnection) {
  resultsTablesOnServer <-
    tolower(x = DatabaseConnector::dbListTables(conn = connectionPool,
                                                schema = resultsDatabaseSchema))
  
  # the code section below instantiates set of tables in R memory.
  # some tables are 'dummy' tables.
  writeLines("Loading Database Table")
  loadRequiredTables(tableName = "database",
                     databaseSchema = resultsDatabaseSchema,
                     required = TRUE,
                     connection = connectionPool)
  writeLines("Loading Cohort Table")
  loadRequiredTables(tableName = "cohort",
                     databaseSchema = resultsDatabaseSchema,
                     required = TRUE,
                     connection = connectionPool)
  
  # Downloading covariate_ref and temporal_covariate_ref in global R because
  # it is now a shared resource across multiple R sessions
  # temporariliy commenting this during app development - because it take a long time to load 
  writeLines("Loading Covariate reference Table")
  loadRequiredTables(tableName = "covariate_ref",
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  writeLines("Loading Temporal Covariate reference Table")
  loadRequiredTables(tableName = "temporal_covariate_ref",
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  
  
  writeLines("Loading Phenotype Description Table")
  loadRequiredTables(tableName = "phenotype_description", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  
  writeLines("Loading Temporal Time Reference Table")
  loadRequiredTables(tableName = "temporal_time_ref", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  
  writeLines("Loading analysis ref Table")
  loadRequiredTables(tableName = "analysis_ref", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  
  writeLines("Loading Temporal analysis reference Table")
  loadRequiredTables(tableName = "temporal_analysis_ref", 
                     databaseSchema = resultsDatabaseSchema,
                     connection = connectionPool)
  
  writeLines(paste("All Tables are loaded in", 
                   scales::comma(as.numeric(difftime(time1 = Sys.time(), 
                                                     time2 = loadTimeStart, 
                                                     units = "secs"))), 
                   "seconds."))
  
  for (table in c(dataModelSpecifications$tableName)) {
    if (table %in% resultsTablesOnServer &&
        !exists(x = snakeCaseToCamelCase(string = table)) &&
        !isEmpty(
          connection = connectionPool,
          tableName = table,
          resultsDatabaseSchema = resultsDatabaseSchema
        )) {
      assign(
        x = snakeCaseToCamelCase(table),
        value = dplyr::tibble(),
        envir = .GlobalEnv
      )
    }
  }
  
  for (table in c("recommender_set")) {
    if (table %in% resultsTablesOnServer &&
        !exists(x = snakeCaseToCamelCase(string = table)) &&
        !isEmpty(
          connection = connectionPool,
          tableName = table,
          resultsDatabaseSchema = 'concept_prevalence'
        )) {
      assign(
        x = snakeCaseToCamelCase(table),
        value = dplyr::tibble(),
        envir = .GlobalEnv
      )
    }
  }
  
  dataSource <-
    createDatabaseDataSource(
      connection = connectionPool,
      connectionDetails = connectionDetails,
      resultsDatabaseSchema = resultsDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
}


# create memory variables based on
if (exists("temporalTimeRef")) {
  temporalTimeRef <- get("temporalTimeRef") %>%
    dplyr::mutate(temporalChoices = paste0("Start ", .data$startDay, " to end ", .data$endDay))
  assign(x = "temporalTimeRef", value = temporalTimeRef, envir = .GlobalEnv)
}

if (exists("temporalCovariateRef")) {
  temporalCovariateRef <- temporalCovariateRef %>% 
    dplyr::mutate(conceptName = stringr::str_to_sentence(
      stringr::str_replace_all(
        string = .data$covariateName,
        pattern = "^.*: ",
        replacement = ""
      )
    ))
}

if (exists("covariateRef")) {
  covariateRef <- covariateRef %>% 
    dplyr::mutate(conceptName = stringr::str_to_sentence(
      stringr::str_replace_all(
        string = .data$covariateName,
        pattern = "^.*: ",
        replacement = ""
      )
    ))
  
  specifications <- readr::read_csv(
    file = "Table1Specs.csv",
    col_types = readr::cols(),
    guess_max = min(1e7)
  )
  
  assign(x = "prettyAnalysisIds",
         value = specifications$analysisId,
         envir = .GlobalEnv)
}


referentConceptIds <- c(0)
# modify tables in memory - process cohort table.
writeLines("Post processing downloaded tables.")
postProcessingStartTime <- Sys.time()
if (exists("cohort")) {
  # this table is required for app to work.
  cohort <- get("cohort") 
  if ('phenotypeId' %in% colnames(cohort)) {
    pId <- cohort %>% 
      dplyr::select(.data$phenotypeId) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(.data$phenotypeId) %>% 
      dplyr::filter(!is.na(.data$phenotypeId))
    
    cohort <- cohort %>%
      dplyr::left_join(pId, by = "phenotypeId")  %>% 
      dplyr::mutate(shortName = as.character(.data$cohortId)) %>% 
      dplyr::mutate(compoundName = paste(.data$shortName,":", .data$cohortName))
  } else {
    cohort <- cohort %>%
      dplyr::arrange(.data$cohortId)
  }

  
  if ('metadata' %in% colnames(cohort)) {
    cohortMetaData <- list()
    for (i in 1:nrow(cohort)) {
      x <- RJSONIO::fromJSON(cohort[i, ]$metadata)
      if (length(names(x)) > 0) {
        for (j in 1:length(x)) {
          if (!any(is.null(x[[j]]), is.na(x[[j]]), names(x[j]) == "sql")) {
            x[[j]] <- stringr::str_split(string = x[[j]], pattern = ";")[[1]]
          }
        }
        x <- dplyr::bind_rows(x)
        x$cohort_id <- cohort[i, ]$cohortId
        x$phenotype_id <- cohort[i, ]$phenotypeId
        cohortMetaData[[i]] <- x
      }
    }
    cohortMetaData <- dplyr::bind_rows(cohortMetaData) %>%
      readr::type_convert(col_types = readr::cols())
    if ('referentConceptId' %in% colnames(cohortMetaData)) {
      referentConceptIds <-
        c(referentConceptIds,
          cohortMetaData$referentConceptId) %>% unique()
    }
  }
} else {
  stop("Cohort table not found in data source")
}
if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
  cohort <- cohort %>% 
    dplyr::left_join(y = phenotypeDescription %>% 
                       dplyr::select(.data$phenotypeId, .data$phenotypeName),
                     by = "phenotypeId")
} else {
  cohort$phenotypeName <- ""
}


if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) { 
  phenotypeDescription <- phenotypeDescription %>%
    dplyr::mutate(clinicalDescription = 
                    stringr::str_squish(.data$clinicalDescription)) %>%
    dplyr::mutate(overview = (
      stringr::str_match(.data$clinicalDescription,
                         "Overview:(.*?)Presentation:")
    )[, 2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(presentation = (
      stringr::str_match(.data$clinicalDescription,
                         "Presentation:(.*?)Assessment:")
    )[, 2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(assessment = (
      stringr::str_match(.data$clinicalDescription,
                         "Assessment:(.*?)Plan:")
    )[, 2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(plan = (
      stringr::str_match(.data$clinicalDescription,
                         pattern = "Plan:(.*?)Prognosis:")
    )[,2] %>%
      stringr::str_trim()) %>%
    dplyr::mutate(prognosis = (
      stringr::str_match(.data$clinicalDescription,
                         pattern = "^.+Prognosis:(.*)")
    )[,2] %>%
      stringr::str_trim()) %>%
  dplyr::inner_join(
    cohort %>%
      dplyr::group_by(.data$phenotypeId) %>%
      dplyr::summarize(cohortDefinitions = dplyr::n()) %>%
      dplyr::ungroup(),
    by = "phenotypeId"
  ) %>% 
    dplyr::mutate(referentConceptId = .data$phenotypeId/1000) %>% 
    dplyr::select(.data$phenotypeId, .data$phenotypeName,
                  .data$clinicalDescription, .data$overview,
                  .data$presentation, .data$assessment, .data$plan,
                  .data$prognosis,
                  .data$cohortDefinitions, .data$referentConceptId)
  
  referentConceptIds <-
    c(referentConceptIds,
      phenotypeDescription$referentConceptId) %>% unique()
}
writeLines(paste("Post processing cohort table completed in", 
                 scales::comma(as.numeric(difftime(time1 = Sys.time(), 
                                                   time2 = postProcessingStartTime, 
                                                   units = "secs"))), 
                 "seconds."))

if (isValidConnection) {
  referentTimeStart <- Sys.time()
  referentConceptIdsDataFrame <-
    renderTranslateQuerySql(
      connection = dataSource$connection,
      vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
      concept_id_list = referentConceptIds,
      sql = SqlRender::readSql("sql/ConceptSynonymNamesForListOfConceptIds.sql"), 
      snakeCaseToCamelCase = TRUE
    ) %>%
    dplyr::arrange(.data$conceptId)
  writeLines(paste("Downloading details for referent concepts took", 
                   scales::comma(as.numeric(difftime(time1 = Sys.time(), 
                                                     time2 = referentTimeStart, 
                                                     units = "secs"))), 
                   "seconds."))
} else {
  referentConceptIdsDataFrame <-
    dplyr::tibble(conceptId = 0, conceptSynonymName = 'No matching concept')
}

referentConceptIdsSearchTerms <- referentConceptIdsDataFrame %>%
  dplyr::group_by(.data$conceptId) %>%
  dplyr::summarise(conceptNameSearchTerms = toString(.data$conceptSynonymName)) %>%
  dplyr::ungroup()


if (exists('cohortMetaData')) {
  if ('referentConceptId' %in% colnames(cohortMetaData)) {
    cohortReferentConceptSearchTerms <- cohortMetaData %>%
      dplyr::select(.data$cohortId, .data$referentConceptId) %>% 
      dplyr::distinct() %>% 
      dplyr::left_join(referentConceptIdsSearchTerms,
                       by = c("referentConceptId" = "conceptId")) %>%
      dplyr::group_by(.data$cohortId) %>% 
      dplyr::mutate(referentConceptId = paste0(as.character(.data$referentConceptId), collapse = ", "),
                    referentConceptIdsSearchTerms = paste0(.data$conceptNameSearchTerms, collapse = ", ")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(referentConceptIdsSearchTerms = paste(referentConceptId,referentConceptIdsSearchTerms)) %>%
      dplyr::select(-.data$referentConceptId, - .data$conceptNameSearchTerms) %>% 
      dplyr::distinct()
    
    cohort <- cohort %>% 
      dplyr::left_join(y = cohortReferentConceptSearchTerms, by = c('cohortId'))
    
    remove(cohortReferentConceptSearchTerms)
  }
  if ('cohortType' %in% colnames(cohortMetaData)) {
    cohortType <- cohortMetaData %>%
      dplyr::select(.data$cohortId, .data$cohortType) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(.data$cohortId) %>% 
      dplyr::mutate(cohortType = paste0(.data$cohortType, collapse = ", ")) %>%
      dplyr::ungroup() %>% 
      dplyr::distinct()
    
    cohort <- cohort %>% 
      dplyr::left_join(y = cohortType, by = c('cohortId'))
    
    remove(cohortType)
  } else {
    cohort$cohortType <- ""
  }
}
remove(cohortMetaData)

if (exists('phenotypeDescription')) {
  if ('referentConceptId' %in% colnames(phenotypeDescription)) {
    phenotypeDescription <- phenotypeDescription %>%
      dplyr::left_join(referentConceptIdsSearchTerms,
                       by = c('referentConceptId' = 'conceptId')) %>%
      dplyr::select(-.data$referentConceptId) %>%
      dplyr::rename(
        phenotypeSynonyms = .data$conceptNameSearchTerms
      )
  }
}

assign(x = "combinationsOfPhenotypeDatabaseCohort",
       getCombinationsOfPhenotypeDatabaseCohort(dataSource = dataSource))
