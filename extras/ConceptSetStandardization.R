library(magrittr)
library(PhenotypeLibrary)
# get conceptSets from all cohort json
# remotes::install_github("ohdsi/ConceptSetDiagnostics",
#                         dependencies = FALSE)

projectFolder <- rstudioapi::getActiveProject()
preProcessedFolder <- file.path(projectFolder, "preprocessed")
dir.create(path = preProcessedFolder, showWarnings = FALSE)

#### collect all JSON
####
####
#### Collect from github repositories
repositories <-
  PhenotypeLibrary:::getRepositoryDetailsForGitHubOrganization(
    organizations = c('OHDSI-studies', 'OHDSI'),
    githubToken = Sys.getenv("githubTokenSimple")
  )

filesInGitHub <- list()
for (i in (1:nrow(repositories))) {
  repository <- repositories[i,]
  files <-
    PhenotypeLibrary:::getListOfFilesInGitHubRepositories(
      repo = paste0(repository$organization, '/', repository$name),
      branch = repository$defaultBranch,
      githubToken = Sys.getenv("githubTokenSimple")
    )

  if (!is.null(files)) {
    filesInGitHub[[i]] <- dplyr::tibble(
      name = repository$name,
      organization = repository$organization,
      branch = repository$defaultBranch,
      file = files
    )
  }
}
filesInGitHub <- dplyr::bind_rows(filesInGitHub)
filesInGitHubJson <- filesInGitHub %>%
  dplyr::filter(stringr::str_detect(string = tolower(file), pattern = '.json'))
saveRDS(object = filesInGitHubJson,
        file = file.path(preProcessedFolder, 'filesInGithubJson.rds'))








##############
filesInGitHubJson <-
  readRDS(file = file.path(preProcessedFolder, 'filesInGithubJson.rds'))
# copy files to local folder
cohortJsonInGitHub <- list()
dir.create(
  path = file.path(projectFolder, "preprocessed"),
  showWarnings = FALSE,
  recursive = TRUE
)
for (i in (1:nrow(filesInGitHubJson))) {
  row <- filesInGitHubJson[i, ]
  remoteSource <-
    file.path(
      "https://raw.githubusercontent.com",
      row$organization,
      row$name,
      row$branch,
      row$file
    )
  # RJSONIO::toJSON(,digits = 23, pretty = TRUE)
  remoteJson <-
    tryCatch(
      expr = RJSONIO::toJSON(RJSONIO::fromJSON(remoteSource, digits = 23), digits = 23),
      error = function(e) {
        NA
      }
    )

  # check if can be rendered into SQL
  if (nchar(dplyr::coalesce(remoteJson, '')) != 0) {
    remoteExpression <- RJSONIO::fromJSON(remoteJson, digits = 23)
  } else {
    remoteExpression <- NA
  }

  if (length(intersect(x = c("ConceptSets", "PrimaryCriteria"),
                y = names(remoteExpression))) == 2) {
    print(i)
    sql <-
      tryCatch(
        expr = ROhdsiWebApi::getCohortSql(
          cohortDefinition = remoteExpression,
          baseUrl = Sys.getenv("BaseUrl"),
          generateStats = TRUE
        ),
        error = function(e) {
          NA
        },
        warning = function(e) {
          NA
        },
        message = function(e) {
          NA
        }
      )
  } else {
    sql <- ""
    writeLines(paste0(
      "       Skipping json - ",
      basename(row$file),
      " at ",
      dirname(row$file)
    ))
  }
  cohortJsonInGitHub[[i]] <- dplyr::tibble(
    source = 'github',
    organization = row$organization,
    repository = row$name,
    branch = row$branch,
    file = row$file,
    json = dplyr::coalesce(remoteJson, ""),
    sql = dplyr::coalesce(sql, "")
  )
}

data <- list()
for (i in (1:length(cohortJsonInGitHub))) {
  data[[i]] <- cohortJsonInGitHub[[i]] %>%
    tidyr::replace_na(replace = list(sql = "", json = ""))
}
cohortJsonInGitHub <- dplyr::bind_rows(data)
saveRDS(
  object = cohortJsonInGitHub,
  file = file.path(projectFolder, "preprocessed", "cohortJsonInGithub.rds")
)


##################### collect from local folder
pathToLocalFilesWithJson <- "D:/git/github/ohdsi/json"
localJsonFiles <-
  dplyr::tibble(
    fullName = list.files(
      path = pathToLocalFilesWithJson,
      pattern = ".json",
      all.files = TRUE,
      full.names = TRUE,
      recursive = TRUE
    )
  ) %>%
  # dplyr::mutate(baseName = stringr::str_replace(
  #   string = basename(.data$fullName),
  #   pattern = ".json",
  #   replacement = ""
  # )) %>%
  dplyr::mutate(
    source = stringr::str_replace(
      string = .data$fullName,
      pattern = pathToLocalFilesWithJson,
      replacement = ""
    )
  ) %>%
  dplyr::select(.data$source)


cohortJsonInLocal <- list()
for (i in (1:nrow(localJsonFiles))) {
  row <- localJsonFiles[i, ]
  remoteSource <- file.path(pathToLocalFilesWithJson, row$source)
  # RJSONIO::toJSON(,digits = 23, pretty = TRUE)
  remoteJson <-
    tryCatch(
      expr = RJSONIO::toJSON(RJSONIO::fromJSON(remoteSource, digits = 23), digits = 23),
      error = function(e) {
        NA
      }
    )

  # check if can be rendered into SQL
  if (nchar(dplyr::coalesce(remoteJson, '')) != 0) {
    remoteExpression <- RJSONIO::fromJSON(remoteJson, digits = 23)
  } else {
    remoteExpression <- NA
  }

  if (length(intersect(x = c("ConceptSets", "PrimaryCriteria"),
                       y = names(remoteExpression))) == 2) {
    print(i)
    sql <-
      tryCatch(
        expr = ROhdsiWebApi::getCohortSql(
          cohortDefinition = remoteExpression,
          baseUrl = Sys.getenv("BaseUrl"),
          generateStats = TRUE
        ),
        error = function(e) {
          NA
        },
        warning = function(e) {
          NA
        },
        message = function(e) {
          NA
        }
      )
  } else {
    sql <- ""
    writeLines(paste0("       Skipping json - ", basename(row$source)))
  }
  cohortJsonInLocal[[i]] <- dplyr::tibble(
    source = 'local',
    file = row$source,
    json = dplyr::coalesce(remoteJson, ""),
    sql = dplyr::coalesce(sql, "")
  )
}

data <- list()
for (i in (1:length(cohortJsonInLocal))) {
  data[[i]] <- cohortJsonInLocal[[i]] %>%
    tidyr::replace_na(replace = list(sql = "", json = ""))
}
cohortJsonInLocal <- dplyr::bind_rows(data)
saveRDS(
  object = cohortJsonInLocal,
  file = file.path(projectFolder, "preprocessed", "cohortJsonInLocal.rds")
)


# Details for connecting to the server:
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = paste(
      Sys.getenv("phenotypeLibraryDbServer"),
      Sys.getenv("phenotypeLibraryDbDatabase"),
      sep = "/"
    ),
    user = Sys.getenv("shinyDbUserGowtham"),
    password = Sys.getenv("shinyDbPasswordGowtham"),
    port = Sys.getenv("phenotypeLibraryDbPort")
  )

connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)



############# create unique concept sets
cohortJsonInGitHub <-
  readRDS(file = file.path(projectFolder, "preprocessed", "cohortJsonInGithub.rds"))
cohortJsonInLocalFile <-
  readRDS(file = file.path(projectFolder, "preprocessed", "cohortJsonInLocal.rds"))

allJsons <-
  dplyr::bind_rows(
    cohortJsonInGitHub %>% dplyr::select(.data$json),
    cohortJsonInLocalFile %>% dplyr::select(.data$json)
  ) %>% dplyr::distinct()


############# find all concept set expression in the cohorts.
conceptSetsFromCohortsToPostProcess <- list()
k <- 0
for (i in (1:nrow(allJsons))) {
  print(paste0("i = ", i))
  row <- allJsons[i, ]
  if ('json' %in% colnames(row) && nchar(row$json) > 5) {
    json <-
      RJSONIO::fromJSON(content = row$json, digits = 23)
    if ('PrimaryCriteria' %in% names(json)) {
      if (is.null(json$ConceptSets)) {
        print(paste0("Skipping ", i, " no concept set detected."))
        next
      }
      if (length(json$ConceptSets) == 0) {
        print(paste0("Skipping ", i, " no concept set detected."))
        next
      }

      for (j in (1:length(json$ConceptSets))) {
        print(paste0("     j = ", j))
        k <- k + 1
        id <- json$ConceptSets[[j]]$id
        name <- json$ConceptSets[[j]]$name

        if (length(json$ConceptSets[[j]]$expression$items) > 0) {
          conceptSetExpressionJsonOriginal <-
            (json$ConceptSets[[j]]$expression) %>%
            RJSONIO::toJSON(pretty = TRUE, digits = 23)
          conceptSetExpressionJsonSignature <-
            ConceptSetDiagnostics::getConceptSetSignatureExpression(
              connection = connection,
              conceptSetExpression = json$ConceptSets[[j]]$expression
            ) %>% RJSONIO::toJSON(digits = 23, pretty = TRUE)
          json <- conceptSetExpressionJsonSignature %>%
            RJSONIO::fromJSON(digits = 23)
          conceptSetDataFrame <-   ConceptSetDiagnostics::getConceptSetDataFrameFromExpression(connection = connection,
                                                                        conceptSetExpression = json,
                                                                        updateVocabularyFields = TRUE)
          conceptSetExpressionJsonUpdatedVocabulary <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptTable(conceptTable = conceptSetDataFrame) %>%
            RJSONIO::toJSON(digits = 23, pretty = TRUE)
        } else {
          conceptSetExpressionJsonOriginal <- ""
          conceptSetExpressionJsonSignature <- ""
          conceptSetExpressionJsonUpdatedVocabulary <- ""
        }

        conceptSetsFromCohortsToPostProcess[[k]] <-
          dplyr::tibble(
            json = allJsons$json[i],
            conceptSetId = !!id,
            conceptSetName = !!name,
            conceptSetExpressionJsonOriginal = !!conceptSetExpressionJsonOriginal,
            conceptSetExpressionJsonSignature = !!conceptSetExpressionJsonSignature,
            conceptSetExpressionJsonUpdatedVocabulary = !!conceptSetExpressionJsonUpdatedVocabulary
          )
      }
    } else {
      print("   skipping - not cohort json")
    }
  } else {
    print("   skipping - no json found")
  }
}
conceptSetsFromCohortsToPostProcess <-
  dplyr::bind_rows(conceptSetsFromCohortsToPostProcess) %>%
  dplyr::arrange(.data$json, .data$conceptSetExpressionJsonOriginal) %>%
  dplyr::distinct()

saveRDS(
  object = conceptSetsFromCohortsToPostProcess,
  file = file.path(
    projectFolder,
    "preprocessed",
    "conceptSetsFromCohortsToPostProcess.rds"
  )
)


#############
conceptSetsFromCohortsToPostProcess <-
  readRDS(file = file.path(
    projectFolder,
    "preprocessed",
    "conceptSetsFromCohortsToPostProcess.rds"
  )) %>%
  dplyr::distinct()

conceptSetSignature <- conceptSetsFromCohortsToPostProcess %>%
  dplyr::filter(nchar(.data$conceptSetExpressionJsonSignature) > 5) %>%
  dplyr::select(.data$conceptSetExpressionJsonSignature)  %>%
  dplyr::distinct() %>%
  dplyr::mutate(conceptSetUniqueId = dplyr::row_number()) %>%
  dplyr::relocate(.data$conceptSetUniqueId)

conceptSetSignature2 <- list()
for (i in (1:nrow(conceptSetSignature))) {
  print(i)
  row <- conceptSetSignature[i, ]
  conceptSetIdDataFrame <-
    ConceptSetDiagnostics::getConceptSetDataFrameFromExpression(
      conceptSetExpression = row$conceptSetExpressionJsonSignature %>% RJSONIO::fromJSON(digits = 23),
      updateVocabularyFields = TRUE,
      recordCount = TRUE,
      connection = connection
    )
  numberOfUniqueConceptId <-
    conceptSetIdDataFrame$conceptId %>% unique() %>% length()
  numberOfUniqueIncludeConceptId <- conceptSetIdDataFrame %>%
    dplyr::filter(!.data$isExcluded == TRUE) %>%
    dplyr::pull(.data$conceptId) %>%
    unique() %>%
    length()
  numberOfUniqueConceptIdStandard <- conceptSetIdDataFrame %>%
    dplyr::filter(.data$standardConcept == 'S') %>%
    dplyr::filter(!.data$isExcluded == TRUE) %>%
    dplyr::pull(.data$conceptId) %>%
    unique() %>%
    length()
  numberOfUniqueExcludeConceptId <- conceptSetIdDataFrame %>%
    dplyr::filter(.data$isExcluded == TRUE) %>%
    dplyr::pull(.data$conceptId) %>%
    unique() %>%
    length()
  row$conceptSetExpressionJson <-
    ConceptSetDiagnostics::getConceptSetExpressionFromConceptTable(conceptTable = conceptSetIdDataFrame) %>%
    RJSONIO::toJSON(digits = 23, pretty = TRUE)

  conceptIdsInConceptSet <- conceptSetIdDataFrame %>%
    dplyr::arrange(dplyr::desc(.data$drc), dplyr::desc(.data$rc)) %>%
    dplyr::select(.data$conceptId, .data$conceptName) %>%
    dplyr::distinct()
  includedConceptIdsInConceptSet <- conceptSetIdDataFrame %>%
    dplyr::filter(.data$isExcluded != TRUE) %>%
    dplyr::arrange(dplyr::desc(.data$drc), dplyr::desc(.data$rc)) %>%
    dplyr::select(.data$conceptId, .data$conceptName) %>%
    dplyr::distinct()
  excludedConceptIdsInConceptSet <- conceptSetIdDataFrame %>%
    dplyr::filter(.data$isExcluded == TRUE) %>%
    dplyr::arrange(dplyr::desc(.data$drc), dplyr::desc(.data$rc)) %>%
    dplyr::select(.data$conceptId, .data$conceptName) %>%
    dplyr::distinct()
  row$listOfConceptIds <-
    paste0(conceptIdsInConceptSet$conceptId %>% unique(),
           collapse = ";;")
  row$listOfConceptNames <-
    paste0(conceptIdsInConceptSet$conceptName %>% unique(),
           collapse = ";;")
  row$listOfIncludedConceptIds <-
    paste0(includedConceptIdsInConceptSet$conceptId %>%
             unique(),
           collapse = ";;")
  row$listOfIncludedConceptNames <-
    paste0(includedConceptIdsInConceptSet$conceptName %>%
             unique(),
           collapse = ";;")
  row$numberOfUniqueConceptId <- numberOfUniqueConceptId
  row$numberOfUniqueConceptIdStandard <-
    numberOfUniqueConceptIdStandard
  row$numberOfUniqueExcludeConceptId = numberOfUniqueExcludeConceptId
  row$listOfExcludedConceptIds <-
    paste0(excludedConceptIdsInConceptSet %>% unique(),
           collapse = ";;")
  if (numberOfUniqueConceptId != numberOfUniqueConceptIdStandard) {
    row$hasNonStandardConceptId <- TRUE
  } else {
    row$hasNonStandardConceptId <- FALSE
  }
  domainsIds <- conceptSetIdDataFrame %>%
    dplyr::select(.data$conceptId, .data$domainId) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$domainId) %>%
    dplyr::summarise(values = dplyr::n_distinct(.data$conceptId)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = 1)
  row$numberOfUniqueDomains <- nrow(domainsIds)
  domains <- domainsIds %>%
    tidyr::pivot_wider(
      names_from = "domainId",
      id_cols = "id",
      values_from = "values",
      names_prefix = "countConceptInDomain"
    ) %>%
    dplyr::select(-.data$id)
  row <- tidyr::crossing(row, domains)


  row$referentConceptIdRecommended1 <-
    conceptIdsInConceptSet$conceptId[1]
  row$referentConceptNameRecommended1 <-
    conceptIdsInConceptSet$conceptName[1]


  row$referentConceptIdRecommended2 <-
    conceptIdsInConceptSet$conceptId[2]
  row$referentConceptNameRecommended2 <-
    conceptIdsInConceptSet$conceptName[2]


  conceptSetSignature2[[i]] <- row
}
conceptSetSignature <- dplyr::bind_rows(conceptSetSignature2) %>%
  dplyr::mutate(conceptSetExpressionName = paste0(.data$conceptSetUniqueId, " ",
                                                  .data$referentConceptNameRecommended1, " (",
                                                  .data$referentConceptIdRecommended1, "-",
                                                  .data$numberOfUniqueConceptId, "-",
                                                  .data$numberOfUniqueConceptIdStandard, "-",
                                                  .data$numberOfUniqueExcludeConceptId, "-",
                                                  .data$numberOfUniqueDomains, ")"))

saveRDS(
  object = conceptSetSignature,
  file = file.path(
    projectFolder,
    "preprocessed",
    "conceptSetSignature.rds"
  )
)
