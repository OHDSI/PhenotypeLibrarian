createCohortDefinition <- function(query, nm) {
  if (typeof(query) != "list") {
    query <- list(query)
  }
  pc <- createPrimaryCriteria(Name = paste(nm, "PC", sep = "_"),
                              ComponentList = query,
                              ObservationWindow = createObservationWindow(PriorDays = 0L, PostDays = 0L),
                              Limit = "All")

  #create timeline
  tl <- createTimeline(StartWindow = createWindow(StartDays = 365,
                                                  StartCoeff = "Before",
                                                  EndDays = 1,
                                                  EndCoeff = "Before"),
                       IgnoreObservationPeriod = TRUE)
  #create count
  crit <- lapply(query, createCount, Logic = "exactly", Count = 0, Timeline = tl)


  #create group
  rule1 <- createGroup(Name = "No events in clean window - 365 days",
                       type = "ALL",
                       criteriaList = crit)

  #create InclusionRules
  irs <- createInclusionRules(Name = paste(nm, "IRs", sep = "_"),
                              Contents = list(rule1),
                              Limit = "All")

  #Create end strategy
  es <- createDateOffsetEndStrategy(offset = 1L,
                                    eventDateOffset = "StartDate")

  #create cohort definition
  cd <- createCohortDefinition(Name = paste0(nm, "Cohort"),
                               PrimaryCriteria = pc,
                               InclusionRules = irs,
                               EndStrategy = es)
  return(cd)
}
