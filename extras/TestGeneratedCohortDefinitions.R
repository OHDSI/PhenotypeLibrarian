library(magrittr)
projectFolder <- rstudioapi::getActiveProject()
cohortDefinitions <- readRDS(file.path(rstudioapi::getActiveProject(), 'inst', 'Cohorts', paste0('Cohorts', todaysDate  , '.rds'))) %>% 
  dplyr::tibble() %>% 
  dplyr::arrange(.data$cohortId)


postDefinitionForReview <- function(conceptSetUniqueId,
                                    cohortDefinitions,
                                    baseUrl = Sys.getenv("BaseUrl")) {
  data <- cohortDefinitions %>% 
    dplyr::filter(.data$conceptSetUniqueId == !!conceptSetUniqueId)
  
  post <- list()
  for (i in (1:nrow(data))) {
    datum <- data[i,]
    post[[i]] <- ROhdsiWebApi::postCohortDefinition(name = datum$cohortName,
                                                    cohortDefinition = RJSONIO::fromJSON(datum$cohortCirceJsonFromCapr, 
                                                                                         digits = 23), 
                                                    baseUrl = baseUrl)
  }
  return(dplyr::bind_rows(post))
}


deleteDefinitionsAfterReview <- function(cohortDefinitionIds,
                                         baseUrl = Sys.getenv("BaseUrl")) {
  for (i in (1:length(cohortDefinitionIds))) {
    ROhdsiWebApi::deleteCohortDefinition(cohortId = cohortDefinitionIds[[i]], 
                                         baseUrl = baseUrl)
  }
}


post <- postDefinitionForReview(conceptSetUniqueId = 1, 
                                cohortDefinitions = cohortDefinitions)

deleteDefinitionsAfterReview(cohortDefinitionIds = post$id)
