# template 1 -------------------------------------
#all events of concept set with no such events in prior clean window
#period (365 days). No continuous observation period requirement.
#Persons exit the cohort on start_date + 1day
createCohortTemplate1 <- function(query, nm, genOp, IPVisit = NULL) {
  if (typeof(query) != "list") {
    query <- list(query) #convert query into list of queries to handle case of condition + occurrence
  }

  # Create primary criteria --------------------
  pc <- Capr::createPrimaryCriteria(Name = paste(nm, "PC", sep = "_"),
                                    ComponentList = query,
                                    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L, PostDays = 0L),
                                    Limit = "All")
  #Create Additional Criteria ---------------------------
  #Skip

  #Create Inclusion Rules ----------------------------------
  #remove occurrence start date attribute from list of query
  query <- lapply(query, removeAttributeType, attrName = "OccurrenceStartDate")
  
  #create timeline for inclusion rules
  tl <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 365,
                                                              StartCoeff = "Before",
                                                              EndDays = 1,
                                                              EndCoeff = "Before"),
                             IgnoreObservationPeriod = TRUE)
  #Turn query into count
  #exactly zero occurrences 365 days before initial occurrence of concept
  crit <- lapply(query, Capr::createCount, Logic = "exactly", Count = 0, Timeline = tl)


  #turn count into group
  rule1 <- Capr::createGroup(Name = "No events in clean window - 365 days",
                             type = "ALL",
                             criteriaList = crit)

  #turn group into InclusionRules
  irs <- Capr::createInclusionRules(Name = paste(nm, "IRs", sep = "_"),
                                    Contents = list(rule1),
                                    Limit = "All")

  # Create end strategy ---------------------------
  #need end strategy of exit after 1 day of start date
  es <- Capr::createDateOffsetEndStrategy(offset = 1L,
                                          eventDateOffset = "StartDate")

  # Create cohort definition ---------------------
  cd <- Capr::createCohortDefinition(Name = paste0(nm, "Cohort"),
                                     PrimaryCriteria = pc,
                                     InclusionRules = irs,
                                     EndStrategy = es)

  cohortInfo <- Capr::compileCohortDefinition(cd, genOp)
  return(cohortInfo)
}
