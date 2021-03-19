cohortId <- 141933001
targetCohortIds <- cohortId
comparatorCohortIds <- 141933003
cohortIds <- c(targetCohortIds, comparatorCohortIds)
databaseId <- 'OPTUM_EXTENDED_DOD'
databaseIds <- c(databaseId, 'OPTUM_PANTHER')

connection = NULL
connectionDetails = NULL
resultsDatabaseSchema = NULL
library(magrittr)


data <- PhenotypeLibrarian::getTimeDistributionResult(cohortIds = cohortIds, 
                                                     databaseIds = databaseIds)
PhenotypeLibrarian::plotTimeDistribution(data = data, 
                                        cohortIds = c(141933001),
                                        databaseIds = databaseIds)



########################
data <- PhenotypeLibrarian::getIncidenceRateResult(cohortIds = cohortIds,
                                                  stratifyByAgeGroup = FALSE,
                                                  stratifyByGender = FALSE,
                                                  stratifyByCalendarYear = TRUE,
                                                  databaseIds = databaseIds)
PhenotypeLibrarian::plotIncidenceRate(data = data,
                                     stratifyByAgeGroup = FALSE,
                                     stratifyByGender = FALSE,
                                     stratifyByCalendarYear = TRUE)

data <- PhenotypeLibrarian::getIncidenceRateResult(cohortIds = cohortIds,
                                                  stratifyByAgeGroup = TRUE,
                                                  stratifyByGender = FALSE,
                                                  stratifyByCalendarYear = TRUE,
                                                  databaseIds = databaseIds)
PhenotypeLibrarian::plotIncidenceRate(data = data,
                                     stratifyByAgeGroup = TRUE,
                                     stratifyByGender = FALSE,
                                     stratifyByCalendarYear = TRUE)

#################
data <- PhenotypeLibrarian::getCohortCountResult(databaseIds = databaseIds)

########################
data <- PhenotypeLibrarian::getCohortOverlapResult(targetCohortIds = targetCohortIds,
                                                  comparatorCohortIds = comparatorCohortIds,
                                                  databaseIds = databaseIds)
PhenotypeLibrarian::plotCohortOverlapVennDiagram(data = data, 
                                                targetCohortIds = targetCohortIds,
                                                comparatorCohortIds = comparatorCohortIds,
                                                databaseIds = databaseId)
######################
covariateReference <- PhenotypeLibrarian::getCovariateReference(isTemporal = FALSE)
cohorts <- PhenotypeLibrarian::getCohortReference()
temporalCovariateReference <- PhenotypeLibrarian::getCovariateReference(isTemporal = TRUE)
timeReference <- PhenotypeLibrarian::getTimeReference()


data <- PhenotypeLibrarian::getCovariateValueResult(cohortIds = cohortIds, 
                                                   databaseIds = databaseIds, 
                                                   minProportion = 0.03,
                                                   isTemporal = FALSE)

data <- PhenotypeLibrarian::compareCovariateValueResult(targetCohortIds = targetCohortIds,
                                                       comparatorCohortIds = comparatorCohortIds,
                                                       databaseIds = databaseId,
                                                       isTemporal = FALSE,
                                                       minProportion = 0)

plot <- PhenotypeLibrarian::plotCohortComparisonStandardizedDifference(data = data,
                                                                      targetCohortIds = targetCohortIds, 
                                                                      comparatorCohortIds = comparatorCohortIds,
                                                                      cohortReference = cohorts,
                                                                      covariateReference = covariateReference,
                                                                      concept = NULL, # to subset based on domain, or vocabulary
                                                                      databaseIds = 'OPTUM_EXTENDED_DOD')
plot

data <- PhenotypeLibrarian::getCovariateValueResult(cohortIds = cohortIds, 
                                                   databaseIds = databaseIds, 
                                                   minProportion = 0.03,
                                                   isTemporal = TRUE)

data <- PhenotypeLibrarian::getCovariateValueResult(cohortIds = cohortIds, 
                                                   databaseIds = databaseIds, 
                                                   minProportion = 0.03,
                                                   isTemporal = TRUE, 
                                                   timeIds = c(1,2,3))


data <- PhenotypeLibrarian::getConceptSetDataDiagnostics(cohortIds = cohortId,
                                                        databaseIds = databaseId) %>% 
  dplyr::filter(type == 'included')  %>% 
  dplyr::inner_join(conceptSets) %>% 
  dplyr::inner_join(concept) %>% 
  dplyr::inner_join(database) %>% 
  dplyr::inner_join(cohort)

View(data)
  
