#' @export
generateConditionObservationProcedurePhenotypes <- function(genOp,
                                                            name,
                                                            cseJson,
                                                            templateFn = c("template1",
                                                                           "template2",
                                                                           "template3",
                                                                           "template4"),
                                                            IPVisit = NULL,
                                                            occurrenceStartDateAttribute = NULL) {
  conceptSetExpression <-
    RJSONIO::fromJSON(content = cseJson, digits = 23) #parse json to R object
  conceptSetExpressionTable <-
    ConceptSetDiagnostics::getConceptSetDataFrameFromExpression(conceptSetExpression = conceptSetExpression)

  mappingFields <-
    c("includeDescendants", "includeMapped", "isExcluded") #identify mapping fields
  conceptSet <-
    conceptSetExpressionTable[, -which(names(conceptSetExpressionTable) %in% mappingFields)] #get conceptSet table
  mapping <-
    conceptSetExpressionTable[, which(names(conceptSetExpressionTable) %in% mappingFields)] #get mapping Table

  #create concept Mapping
  conceptMapping <- Capr::createConceptMapping(
    n = nrow(mapping),
    includeDescendants = mapping$includeDescendants,
    isExcluded = mapping$isExcluded,
    includeMapped = mapping$includeMapped
  )

  #create concept Set Expression
  cse <-
    Capr::createConceptSetExpressionCustom(conceptSet = conceptSet,
                                           Name = name,
                                           conceptMapping = conceptMapping)

  templateFn <- match.arg(templateFn)
  templateFn <- switch(
    templateFn,
    template1 = createCohortTemplate1,
    template2 = createCohortTemplate2,
    template3 = createCohortTemplate3,
    template4 = createCohortTemplate4
  )

  standardConcept <-
    unique(conceptSetExpressionTable$standardConcept)
  if (!is.na(standardConcept) &&
      length(standardConcept) == 1 &&
      standardConcept == 'S') {
    allConceptsAreStandard <- TRUE
  } else {
    allConceptsAreStandard <- FALSE
  }

  allDomains <-
    unique(conceptSetExpressionTable$domainId) %>% sort()

  ll <- NA
  tmp <- list()

  if (length(intersect(allDomains, c(
    'Condition', 'Observation', 'Procedure'
  ))) > 0) {
    if (allConceptsAreStandard) {
      #start by checking if all concepts are standard
      if (length(allDomains) == 1) {
        if (allDomains == "Condition") {
          tmp[[1]] <- Capr::createConditionOccurrence(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                        attributeList =
                                                          list(
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          ll <- templateFn(
            query = tmp,
            nm = name,
            genOp = genOp,
            IPVisit = IPVisit
          )
        } else if (allDomains == "Observation") {
          tmp[[1]] <- Capr::createObservation(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <- Capr::createObservation(conceptSetExpression = cse,
                                                attributeList =
                                                  list(
                                                    occurrenceStartDateAttribute
                                                  ))
          }
          ll <- templateFn(
            query = tmp,
            nm = name,
            genOp = genOp,
            IPVisit = IPVisit
          )
        } else if (allDomains == "Procedure") {
          tmp[[1]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                                        attributeList =
                                                          list(
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          ll <- templateFn(
            query = tmp,
            nm = name,
            genOp = genOp,
            IPVisit = IPVisit
          )
        }
      } else if (length(allDomains) == 2) {
        if (length(intersect(
          x = allDomains,
          y = c("Condition", "Observation", "Procedure")
        )) == 2) {
          if (allDomains[[1]] == 'Condition') {
            tmp[[1]] <-
              Capr::createConditionOccurrence(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[1]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            first <- 'Condition'
          } else if (allDomains[[1]] == 'Observation') {
            tmp[[1]] <- Capr::createObservation(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[1]] <- Capr::createObservation(conceptSetExpression = cse,
                                                  attributeList =
                                                    list(
                                                      occurrenceStartDateAttribute
                                                    ))
            }
            first <- 'Observation'
          } else if (allDomains[[1]] == 'Procedure') {
            tmp[[1]] <-
              Capr::createProcedureOccurrence(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[1]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            first <- 'Procedure'
          }

          if (allDomains[[2]] == 'Condition') {
            tmp[[2]] <-
              Capr::createConditionOccurrence(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[2]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            second <- 'Condition'
          } else if (allDomains[[2]] == 'Observation') {
            tmp[[2]] <- Capr::createObservation(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[2]] <- Capr::createObservation(conceptSetExpression = cse,
                                                  attributeList =
                                                    list(
                                                      occurrenceStartDateAttribute
                                                    ))
            }
            second <- 'Observation'
          } else if (allDomains[[2]] == 'Procedure') {
            tmp[[2]] <-
              Capr::createProcedureOccurrence(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[2]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            second <- 'Procedure'
          }
          ll <- templateFn(
            query = tmp,
            genOp = genOp,
            IPVisit = IPVisit,
            nm = paste0(name, first, "+", second)
          )
        }
      } else if (length(allDomains) == 3) {
        if (length(intersect(
          x = allDomains,
          y = c("Condition", "Observation", "Procedure")
        )) == 3) {
          tmp[[1]] <-
            Capr::createConditionOccurrence(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                        attributeList =
                                                          list(
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          tmp[[2]] <-
            Capr::createObservation(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[2]] <- Capr::createObservation(conceptSetExpression = cse,
                                                attributeList =
                                                  list(
                                                    occurrenceStartDateAttribute
                                                  ))
          }
          tmp[[3]] <-
            Capr::createProcedureOccurrence(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[3]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                                        attributeList =
                                                          list(
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          ll <- templateFn(
            query = tmp,
            genOp = genOp,
            IPVisit = IPVisit,
            nm = paste0(name, "Condition+Observation+Procedure")
          )
        }
      }
    } else {
      # some concepts are not standard
      if (length(allDomains) == 1) {
        if (allDomains == "Condition") {
          tmp[[1]] <-
            Capr::createConditionOccurrence(conceptSetExpression = cse)
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                        attributeList =
                                                          list(
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          tmp[[2]] <-
            Capr::createConditionOccurrence(attributeList =
                                              list(
                                                Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse)
                                              ))
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[2]] <- Capr::createConditionOccurrence(attributeList =
                                                          list(
                                                            Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse),
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          ll <- templateFn(
            query = tmp,
            nm = paste0(name, "Condition+Condition Source"),
            genOp = genOp,
            IPVisit = IPVisit
          )
        } else if (allDomains == "Observation") {
          tmp[[1]] <-
            Capr::createObservation(conceptSetExpression = cse)
          tmp[[2]] <- Capr::createObservation(attributeList =
                                                list(
                                                  Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse)
                                                ))
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <-
              Capr::createObservation(conceptSetExpression = cse,
                                      attributeList =
                                        list(
                                          occurrenceStartDateAttribute
                                        ))
            tmp[[2]] <- Capr::createObservation(attributeList =
                                                  list(
                                                    Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse),
                                                    occurrenceStartDateAttribute
                                                  ))
          }
          ll <- templateFn(
            query = tmp,
            nm = paste0(name, "Observation+Observation Source"),
            genOp = genOp,
            IPVisit = IPVisit
          )
        } else if (allDomains == "Procedure") {
          tmp[[1]] <-
            Capr::createProcedureOccurrence(conceptSetExpression = cse)
          tmp[[2]] <-
            Capr::createProcedureOccurrence(attributeList =
                                              list(
                                                Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse)
                                              ))
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <-
              Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                              attributeList =
                                                list(
                                                  occurrenceStartDateAttribute
                                                ))
            tmp[[2]] <- Capr::createProcedureOccurrence(attributeList =
                                                          list(
                                                            Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse),
                                                            occurrenceStartDateAttribute
                                                          ))
          }
          ll <- templateFn(
            query = tmp,
            nm = paste0(name, "Procedure+Procedure Source"),
            genOp = genOp,
            IPVisit = IPVisit
          )
        }
      } else if (length(allDomains) == 2) {
        if (length(intersect(
          x = allDomains,
          y = c("Condition", "Observation", "Procedure")
        )) == 2) {
          if (allDomains[[1]] == 'Condition') {
            tmp[[1]] <-
              Capr::createConditionOccurrence(conceptSetExpression = cse)
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[1]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            tmp[[2]] <-
              Capr::createConditionOccurrence(attributeList =
                                                list(
                                                  Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse)
                                                ))
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[2]] <- Capr::createConditionOccurrence(attributeList =
                                                            list(
                                                              Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse),
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            first <- 'Condition'
          } else if (allDomains[[1]] == 'Observation') {
            tmp[[1]] <- Capr::createObservation(conceptSetExpression = cse)
            tmp[[2]] <- Capr::createObservation(attributeList =
                                                  list(
                                                    Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse)
                                                  ))
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[1]] <- Capr::createObservation(conceptSetExpression = cse,
                                                  attributeList =
                                                    list(
                                                      occurrenceStartDateAttribute
                                                    ))
              tmp[[2]] <- Capr::createObservation(attributeList =
                                                    list(
                                                      Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse),
                                                      occurrenceStartDateAttribute
                                                    ))
            }
            first <- 'Observation'
          } else if (allDomains[[1]] == 'Procedure') {
            tmp[[1]] <-
              Capr::createProcedureOccurrence(conceptSetExpression = cse)
            tmp[[2]] <-
              Capr::createProcedureOccurrence(attributeList =
                                                list(
                                                  Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse)
                                                ))
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[1]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
              tmp[[2]] <- Capr::createProcedureOccurrence(attributeList =
                                                            list(
                                                              Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse),
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            first <- 'Procedure'
          }

          if (allDomains[[2]] == 'Condition') {
            tmp[[3]] <-
              Capr::createConditionOccurrence(conceptSetExpression = cse)
            tmp[[4]] <-
              Capr::createConditionOccurrence(attributeList =
                                                list(
                                                  Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse)
                                                ))
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[3]] <- Capr::createConditionOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
              tmp[[4]] <- Capr::createConditionOccurrence(attributeList =
                                                            list(
                                                              Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse),
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            second <- 'Condition'
          } else if (allDomains[[2]] == 'Observation') {
            tmp[[3]] <- Capr::createObservation(conceptSetExpression = cse)
            tmp[[4]] <- Capr::createObservation(attributeList =
                                                  list(
                                                    Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse)
                                                  ))
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[3]] <- Capr::createObservation(conceptSetExpression = cse,
                                                  attributeList =
                                                    list(
                                                      occurrenceStartDateAttribute
                                                    ))
              tmp[[4]] <- Capr::createObservation(attributeList =
                                                    list(
                                                      Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse),
                                                      occurrenceStartDateAttribute
                                                    ))
            }
            second <- 'Observation'
          } else if (allDomains[[2]] == 'Procedure') {
            tmp[[3]] <-
              Capr::createProcedureOccurrence(conceptSetExpression = cse)
            tmp[[4]] <-
              Capr::createProcedureOccurrence(attributeList =
                                                list(
                                                  Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse)
                                                ))
            if (!is.null(occurrenceStartDateAttribute)) {
              tmp[[3]] <- Capr::createProcedureOccurrence(conceptSetExpression = cse,
                                                          attributeList =
                                                            list(
                                                              occurrenceStartDateAttribute
                                                            ))
              tmp[[4]] <- Capr::createProcedureOccurrence(attributeList =
                                                            list(
                                                              Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse),
                                                              occurrenceStartDateAttribute
                                                            ))
            }
            second <- 'Procedure'
          }
          ll <-
            templateFn(
              query = tmp,
              nm = paste0(
                name,
                first,
                "+",
                first,
                " source + ",
                second,
                " + ",
                second,
                " source"
              ),
              genOp = genOp,
              IPVisit = IPVisit
            )
        }
      } else if (length(allDomains) == 3) {
        if (length(intersect(
          x = allDomains,
          y = c("Condition", "Observation", "Procedure")
        )) == 3) {
          tmp[[1]] <-
            Capr::createConditionOccurrence(conceptSetExpression = cse)
          tmp[[2]] <-
            Capr::createConditionOccurrence(attributeList =
                                              list(
                                                Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse)
                                              ))
          tmp[[3]] <-
            Capr::createProcedureOccurrence(conceptSetExpression = cse)
          tmp[[4]] <-
            Capr::createProcedureOccurrence(attributeList =
                                              list(
                                                Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse)
                                              ))
          tmp[[5]] <-
            Capr::createObservation(conceptSetExpression = cse)
          tmp[[6]] <-
            Capr::createObservation(attributeList =
                                      list(
                                        Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse)
                                      ))
          if (!is.null(occurrenceStartDateAttribute)) {
            tmp[[1]] <-
              Capr::createConditionOccurrence(conceptSetExpression = cse,
                                              attributeList =
                                                list(
                                                  occurrenceStartDateAttribute
                                                ))
            tmp[[2]] <-
              Capr::createConditionOccurrence(attributeList =
                                                list(
                                                  Capr::createConditionSourceConceptAttribute(ConceptSetExpression = cse),
                                                  occurrenceStartDateAttribute
                                                ))
            tmp[[3]] <-
              Capr::createProcedureOccurrence(conceptSetExpression = cse)
            tmp[[4]] <-
              Capr::createProcedureOccurrence(attributeList =
                                                list(
                                                  Capr::createProcedureSourceConceptAttribute(ConceptSetExpression = cse),
                                                  occurrenceStartDateAttribute
                                                ))
            tmp[[5]] <-
              Capr::createObservation(conceptSetExpression = cse,
                                      attributeList =
                                        list(
                                          occurrenceStartDateAttribute
                                        ))
            tmp[[6]] <-
              Capr::createObservation(attributeList =
                                        list(
                                          Capr::createObservationSourceConceptAttribute(ConceptSetExpression = cse),
                                          occurrenceStartDateAttribute
                                        ))
          }
          ll <-
            templateFn(
              query = tmp,
              nm = paste0(
                name,
                "condition + condition source + procedure + procedure source + observation + observation source"
              ),
              genOp = genOp,
              IPVisit = IPVisit
            )
        }
      }
    }
  }
  return(ll)
}
