# Template 4 -----------------------------
#first occurrence of concept set event, first time in persons history
#no prior observation period requirement, follow up till end of observation period
createCohortTemplate4 <- function(query, nm, genOp, IPVisit = NULL) {
  if (typeof(query) != "list") {
    query <- list(query)
  }
  firstOccurrence <- Capr::createFirstAttribute()
  query <- lapply(query, addAttributeToQuery, attribute = firstOccurrence)

  # Create primary criteria --------------------
  pc <- Capr::createPrimaryCriteria(Name = paste(nm, "PC", sep = "_"),
                                    ComponentList = query,
                                    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L, PostDays = 0L),
                                    Limit = "All")

  # Create cohort definition ---------------------
  cd <- Capr::createCohortDefinition(Name = paste0(nm, "Cohort"),
                                     PrimaryCriteria = pc)
  cohortInfo <- Capr::compileCohortDefinition(cd, genOp)
  return(cohortInfo)
}
