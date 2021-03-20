# Template 2 ----------------------------------
#all events of concept set with overlapping inpatient visit
#with no such events in prior clean window period (365 days).
#no continuous observation period requirement Person exit the cohort
createCohortTemplate2 <- function(query, nm, genOp, IPVisit = IPVisit) {
  if (typeof(query) != "list") {
    query <- list(query)
  }

  # Create primary criteria --------------------
  pc <- Capr::createPrimaryCriteria(Name = paste(nm, "PC", sep = "_"),
                                    ComponentList = query,
                                    ObservationWindow = Capr::createObservationWindow(PriorDays = 0L, PostDays = 0L),
                                    Limit = "All")
  #Create Additional Criteria ---------------------------
  ac <- Capr::createAdditionalCriteria(Name = paste(nm, "AC", sep = "_"),
                                       Contents = IPVisit,
                                       Limit = "All")


  #Create Inclusion Rules ----------------------------------
  #remove occurrence start date attribute from list of query
  query <- lapply(query, removeAttributeType, attrName = "OccurrenceStartDate")
  
  #create timeline for inclusion rules
  tl <- Capr::createTimeline(StartWindow = Capr::createWindow(StartDays = 365,
                                                              StartCoeff = "Before",
                                                              EndDays = 1,
                                                              EndCoeff = "Before"),
                             IgnoreObservationPeriod = TRUE)

  # create new version of query where inpatient visit is nested
  corrCrit <- Capr::createCorrelatedCriteriaAttribute(IPVisit)
  corrQuery <- lapply(query, addAttributeToQuery, attribute = corrCrit)

  #Turn nested query into count
  #exactly zero occurrences 365 days before initial occurrence of concept
  crit <- lapply(corrQuery, Capr::createCount, Logic = "exactly", Count = 0, Timeline = tl)


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
                                     AdditionalCriteria = ac,
                                     InclusionRules = irs,
                                     EndStrategy = es)
  cohortInfo <- Capr::compileCohortDefinition(cd, genOp)
  return(cohortInfo)
}
