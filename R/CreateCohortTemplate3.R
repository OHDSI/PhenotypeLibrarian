

#template 3 ----------------------
#First occurrence of concept set event, first time in person history
#minimum prior observation period of 365 days, follow-up till end of observaiton period

createCohortTemplate3 <- function(query, nm, genOp, IPVisit = NULL) {
  if (typeof(query) != "list") {
    query <- list(query)
  }
  firstOccurrence <- Capr::createFirstAttribute() # create first occurrence attribute
  query <- lapply(query, addAttributeToQuery, attribute = firstOccurrence) # add attribute to query

  # Create primary criteria --------------------
  pc <- Capr::createPrimaryCriteria(Name = paste(nm, "PC", sep = "_"),
                                    ComponentList = query,
                                    ObservationWindow = Capr::createObservationWindow(PriorDays = 365L, PostDays = 0L),
                                    Limit = "All")

  # Create cohort definition ---------------------
  cd <- Capr::createCohortDefinition(Name = paste0(nm, "Cohort"),
                                     PrimaryCriteria = pc)
  cohortInfo <- Capr::compileCohortDefinition(cd, genOp)
  return(cohortInfo)
}
