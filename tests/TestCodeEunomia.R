library(PhenotypeLibrarian)
library(Eunomia)

#baseUrl <- Sys.getenv("OHDSIbaseUrl")

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
cdmDatabaseSchema <- "main"
cohortDatabaseSchema <- "main"
cohortTable <- "cohort"
oracleTempSchema <- NULL
folder <- "D:\\git\\bitbucket\\PhenotypeLibrarianTest\\results\\firstRun\\eunomia" #tempfile()
unlink(x = folder, recursive = TRUE, force = TRUE)
dir.create(folder, recursive = TRUE, showWarnings = FALSE)


PhenotypeLibrarian::instantiateCohortSet(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        oracleTempSchema = oracleTempSchema,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortTable = cohortTable,
                                        packageName = "PhenotypeLibrarian",
                                        cohortToCreateFile = "settings/CohortsToCreateForTesting.csv",
                                        generateInclusionStats = TRUE,
                                        createCohortTable = TRUE,
                                       # incremental = TRUE, 
                                       # incrementalFolder = file.path(folder, "incremental"),
                                        inclusionStatisticsFolder = file.path(folder, "inclusionStatistics"))

# debug(PhenotypeLibrarian::runPhenotypeLibrarian)
# debug(PhenotypeLibrarian::breakDownIndexEvents)

PhenotypeLibrarian::runPhenotypeLibrarian(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        oracleTempSchema = oracleTempSchema,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortTable = cohortTable,
                                        packageName = "PhenotypeLibrarian",
                                        cohortToCreateFile = "settings/CohortsToCreateForTesting.csv",
                                        inclusionStatisticsFolder = file.path(folder, "inclusionStatistics"),
                                        exportFolder =  file.path(folder, "export"),
                                        databaseId = "Eunomia",
                                        runInclusionStatistics = TRUE,
                                        runBreakdownIndexEvents = TRUE,
                                        runCohortCharacterization = TRUE,
                                        runTemporalCohortCharacterization = TRUE,
                                        runCohortOverlap = TRUE,
                                        runIncidenceRate = TRUE,
                                        runIncludedSourceConcepts = TRUE,
                                        runOrphanConcepts = TRUE,
                                        runTimeDistributions = TRUE,
                                        incremental = TRUE,
                                        minCellCount = 10,
                                        incrementalFolder = file.path(folder, "incremental"))

PhenotypeLibrarian::preMergeDiagnosticsFiles(dataFolder = file.path(folder, "export"))

