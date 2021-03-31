shiny::shinyServer(function(input, output, session) {
  
  shinyWidgetsPickerOptions <- shinyWidgets::pickerOptions(
    actionsBox = TRUE,
    liveSearch = TRUE,
    liveSearchNormalize = TRUE,
    size = 10,
    liveSearchStyle = "contains",
    liveSearchPlaceholder = "Not selected",
    virtualScroll = 50,
    # mobile = TRUE,
    selectOnTab = TRUE,
    showTick = TRUE,
    width	= TRUE,
    windowPadding = 2
  )
  
  showAllMenuItem <- reactiveVal(FALSE)
  rvCharacterizationPrettyTableGenerated <- shiny::reactiveVal(value = FALSE)
  
  output$isHeaderbarVisible <- shiny::reactive(x = {
    return(showAllMenuItem())
  })
  
  shiny::outputOptions(x = output,
                       name = "isHeaderbarVisible",
                       suspendWhenHidden = FALSE)
  
  output$menuItems <- shinydashboard::renderMenu({
    menuList <- list(
      shinydashboard::menuItem(text = "Search", tabName = "search")
    )
    
    menuList[[2]] <- 
      if (exists(x = "aboutText"))
        shinydashboard::menuItem(text = "About", tabName = "about")
    
    if (showAllMenuItem()) {
      menuList[[3]] <- 
        if (exists(x = "cohortCount"))
          addInfo(
            item = shinydashboard::menuItem(text = "Concept Set Diagnostics", tabName = "conceptSetDiagnostics"),
            infoId = "conceptSetDiagnosticsInfo"
          )
      
      menuList[[4]] <- 
        if (exists(x = "cohortCount"))
          addInfo(
            item = shinydashboard::menuItem(text = "Cohort Counts", tabName = "cohortCounts"),
            infoId = "cohortCountsInfo"
          )
      
      menuList[[5]] <- 
        if (exists(x = "incidenceRate"))
          addInfo(
            item = shinydashboard::menuItem(text = "Incidence Rate", tabName = "incidenceRate"),
            infoId = "incidenceRateInfo"
          )
      
      menuList[[6]] <- 
        if (exists(x = "timeDistribution"))
          addInfo(
            item = shinydashboard::menuItem(text = "Time Distributions", tabName = "timeDistribution"),
            infoId = "timeDistributionInfo"
          )
      
      menuList[[7]] <-
        if (exists(x = "inclusionRuleStats"))
          addInfo(
            item = shinydashboard::menuItem(text = "Inclusion Rule Statistics", tabName = "inclusionRuleStats"),
            infoId = "inclusionRuleStatsInfo"
          )
      menuList[[8]] <-
        if (exists(x = "indexEventBreakdown"))
          addInfo(
            item = shinydashboard::menuItem(text = "Index Event Breakdown", tabName = "indexEventBreakdown"),
            infoId = "indexEventBreakdownInfo"
          )
      menuList[[9]] <- 
        if (exists(x = "visitContext"))
          addInfo(
            item = shinydashboard::menuItem(text = "Visit Context", tabName = "visitContext"),
            infoId = "visitContextInfo"
          )
      
      menuList[[10]] <-
        if (exists(x = "covariateValue"))
          addInfo(
            shinydashboard::menuItem(text = "Cohort Characterization", tabName = "cohortCharacterization"),
            infoId = "cohortCharacterizationInfo"
          )
      
      menuList[[11]] <-
        if (exists(x = "covariateValue"))
          addInfo(
            shinydashboard::menuItem(text = "Compare Cohort Char.", tabName = "compareCharacterization"),
            infoId = "cohortCharacterizationInfo"
          )
      
      menuList[[12]] <- 
        if (exists(x = "temporalCovariateValue"))
          addInfo(
            shinydashboard::menuItem(text = "Temporal Characterization", tabName = "temporalCharacterization"),
            infoId = "temporalCharacterizationInfo"
          )
      
      menuList[[13]] <- 
        if (exists(x = "cohortOverlap"))
          addInfo(
            shinydashboard::menuItem(text = "Cohort Overlap", tabName = "cohortOverlap"),
            infoId = "cohortOverlapInfo"
          )
    }
    
    menuList[[14]] <-
      shinydashboard::menuItem(text = "Database information", tabName = "databaseInformation")
    
    shinydashboard::sidebarMenu(menuList)
  })
  
  searchTableRowIsSelected <-  shiny::reactive(x = {
    length <- length(input$cohortSearchTableResults_rows_selected)
    
    if (length == 2) {
      return(6)
    } else {
      return(12)
    }
  })
  
  
  ### GOOGLE SHEET - PHENOTYPE NOTES  
  
  searchClick <- reactiveVal(0)
  
  searchCommentText <- callModule(markdownInput::moduleMarkdownInput, "searchComment")
  observeEvent(eventExpr = input$submitSearchComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForSearch
    appendToAnnotationService(menuItem = "search",
                              annotation = searchCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = 'All',
                              appSignature = appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  cohortCountsCommentText <- callModule(markdownInput::moduleMarkdownInput, "cohortCountsComment")
  observeEvent(eventExpr = input$submitCohortCountsComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForCohortCounts
    databaseIds <- selectedDatabaseIds()
    appendToAnnotationService(menuItem = "cohortCounts",
                              annotation = cohortCountsCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature =  appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  incidenceRateCommentText <- callModule(markdownInput::moduleMarkdownInput, "incidenceRateComment")
  observeEvent(eventExpr = input$submitIncidenceRateComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForIncidenceRate
    databaseIds <- input$selectedDatabaseIdsForIncidenceRate
    appendToAnnotationService(menuItem = "incidenceRate",
                              annotation = incidenceRateCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature =  appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  timeDistributionCommentText <- callModule(markdownInput::moduleMarkdownInput, "timeDistributionComment")
  observeEvent(eventExpr = input$submitTimeDistributionComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForTimeDistribution
    databaseIds <- input$selectedDatabaseIdsForTimeDistribution
    appendToAnnotationService(menuItem = "timeDistributions",
                              annotation = timeDistributionCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature =  appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  inclusionRuleStatisticsCommentText <- callModule(markdownInput::moduleMarkdownInput, "inclusionRuleStatisticsComment")
  observeEvent(eventExpr = input$submitInclusionRuleStatisticsComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForInclusionRuleStatistics
    databaseIds <- input$selectedDatabaseIdsForInclusionRuleStatistics
    appendToAnnotationService(menuItem = "inclusionRuleStatistics",
                              annotation = inclusionRuleStatisticsCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature =  appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  indexEventBreakDownCommentText <- callModule(markdownInput::moduleMarkdownInput, "indexEventBreakDownComment")
  observeEvent(eventExpr = input$submitIndexEventBreakDownComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForIndexEventBreakDown
    databaseIds <- input$selectedDatabaseIdsForIndexEventBreakDown
    appendToAnnotationService(menuItem = "indexEventBreakdown",
                              annotation = inclusionRuleStatisticsCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature =  appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  visitContextCommentText <- callModule(markdownInput::moduleMarkdownInput, "visitContextComment")
  observeEvent(eventExpr = input$submitVisitContextComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForVisitContext
    databaseIds <- input$selectedDatabaseIdsForVisitContext
    appendToAnnotationService(menuItem = "visitContext",
                              annotation = visitContextCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature =  appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  cohortCharacterizationCommentText <- callModule(markdownInput::moduleMarkdownInput, "cohortCharacterizationComment")
  observeEvent(eventExpr = input$submitCohortCharacterizationComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForCohortCharacterization
    databaseIds <- input$selectedDatabaseIdsForCohortCharacterization
    appendToAnnotationService(menuItem = "cohortCharacterization",
                              annotation = cohortCharacterizationCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature = appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  
  compareCohortCharCommentText <- callModule(markdownInput::moduleMarkdownInput, "compareCohortCharComment")
  observeEvent(eventExpr = input$submitCompareCohortCharComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForCompareCohortChar
    databaseIds <- selectedDatabaseIds()
    appendToAnnotationService(menuItem = "compareCohortChar",
                              annotation = compareCohortCharCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature = appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  temporalCharacterizationCommentText <- callModule(markdownInput::moduleMarkdownInput, "temporalCharacterizationComment")
  observeEvent(eventExpr = input$submitTemporalCharacterizationComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForTemporalCharacterization
    databaseIds <- selectedDatabaseIds()
    appendToAnnotationService(menuItem = "temporalCharacterization",
                              annotation = temporalCharacterizationCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature = appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  cohortOverlapCommentText <- callModule(markdownInput::moduleMarkdownInput, "cohortOverlapComment")
  observeEvent(eventExpr = input$submitCohortOverlapComment, handlerExpr = {
    cohortIds <- input$selectedCohortIdsForCohortOverlap
    databaseIds <- selectedDatabaseIds()
    appendToAnnotationService(menuItem = "cohortOverlap",
                              annotation = cohortOverlapCommentText(),
                              cohortIds = cohortIds,
                              databaseIds = databaseIds, 
                              appSignature = appSignatureValue,
                              annotationPermission = annotationPermission, 
                              annotationService = annotationService)
    searchClick(searchClick() + 1)
  })
  
  observe({
    idx <- input$cohortSearchTableResults_rows_selected
    cohortIds <- cohort[idx,]$cohortId %>% unique()
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForSearch",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = cohortIds,
      selected = cohortIds
    )
  })
  
  observe({
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForCohortCounts",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForCohortCounts",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForIncidenceRate",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForIncidenceRate",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForTimeDistribution",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForTimeDistribution",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForInclusionRuleStatistics",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForInclusionRuleStatistics",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForIndexEventBreakDown",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForIndexEventBreakDown",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForVisitContext",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForVisitContext",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForCohortCharacterization",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForCohortCharacterization",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForCompareCohortChar",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForCompareCohortChar",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForTemporalCharacterization",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForTemporalCharacterization",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedCohortIdsForCohortOverlap",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedCohortIds(),
      selected = selectedCohortIds()
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectedDatabaseIdsForCohortOverlap",
      choicesOpt = list(style = rep_len("color: black;", 999)),
      choices = selectedDatabaseIds(),
      selected = selectedDatabaseIds()
    )
  })
  
  
  
  ## Reading from annotation service
  readComments <- shiny::reactive({
    searchClick()
    data <- readFromAnnotationService(
      menuItem = currentReadingTab(),
      appSignatureValue = appSignatureValue,
      annotationPermission = annotationPermission,
      annotationService = annotationService
    )
    if(!is.null(data)) {
      data <- data %>% 
        dplyr::rename(markdown = annotation)
    }
    return(data)
  })
  
  genrateResponseFromGoogleSheet <- function(cohortIds,databaseIds) {
    if (is.null(readComments()))
      return(NULL)
    sheetResults <- NULL
    resultString <- ""
    
    cohortIdString <-
      paste(cohortIds, collapse = "|")
    sheetResults <-  readComments() %>%
      dplyr::filter(grepl(cohortIdString, .data$cohortId)) %>%
      dplyr::select(
        .data$cohortId,
        .data$databaseId,
        .data$dateTimeEntry,
        .data$markdown,
        .data$userPriority
      )
    
    cohortGroups <- sheetResults %>%
      dplyr::select(.data$cohortId) %>%
      dplyr::distinct()
    
    availableCohorts <- c()
    if (nrow(cohortGroups) > 0) {
      for (i in 1:nrow(cohortGroups)) {
        CohortSplit <-
          toString(cohortGroups[i,]) %>% strsplit(split = ",")
        for (j in 1:length(CohortSplit[[1]])) {
          CohortSplit[[1]][j]  <- trimws(CohortSplit[[1]][j])
          if ((CohortSplit[[1]][j] %in% cohortIds) &&
              (!CohortSplit[[1]][j] %in% availableCohorts)) {
            availableCohorts <- c(availableCohorts, CohortSplit[[1]][j])
          }
        }
      }
    }
    if (length(availableCohorts) > 0) {
      for (i in  1:length(availableCohorts)) {
        resultString <-
          paste0(
            resultString,
            "<h4 style='padding: 10px;background-color:#ddd;font-weight:bold;border-radius:5px;'>",
            i,
            ". Cohort ID : ",
            availableCohorts[i],
            "<span style='font-size:12px'> - ",
            cohort[cohort$cohortId == availableCohorts[i], ]$cohortName,
            "</span>",
            "</h4>"
          )
        
        databaseGroup <- sheetResults %>%
          dplyr::filter(grepl(availableCohorts[i], .data$cohortId)) %>%
          dplyr::filter(.data$databaseId != "")  %>%
          dplyr::select(.data$databaseId) %>%
          unique()
        availableDatabase <- c()
        if (nrow(databaseGroup) > 0) {
          for (l in 1:nrow(databaseGroup)) {
            DatabaseSplit <-
              toString(databaseGroup[l,]) %>% strsplit(split = ",")
            for (m in 1:length(DatabaseSplit[[1]])) {
              DatabaseSplit[[1]][m]  <- trimws(DatabaseSplit[[1]][m])
              if ((DatabaseSplit[[1]][m] %in% databaseIds) &&
                  (!DatabaseSplit[[1]][m] %in% availableDatabase)) {
                availableDatabase <- c(availableDatabase, DatabaseSplit[[1]][m])
              }
            }
          }
        }
        if (length(availableDatabase) > 0) {
          for (k in  1:length(availableDatabase)) {
            resultString <-
              paste0(
                resultString,
                "<div style='padding: 8px;font-weight:bold;border-radius:5px 5px 0 0;'>",
                i,
                ".",
                k,
                ". Database : ",
                availableDatabase[k],
                "</div><div style='border-radius:0 0 5px 5px;'>"
              )
            result <-  sheetResults %>%
              dplyr::filter(grepl(availableCohorts[i], .data$cohortId)) %>%
              dplyr::filter(grepl(availableDatabase[k], .data$databaseId)) %>%
              dplyr::arrange(-.data$userPriority)
            
            for (j in 1:nrow(result)) {
              databaseSplit <- result[j,]$databaseId %>% strsplit(split = ",")
              recomendation <- ""
              if (result[j,]$userPriority > 0) {
                recomendation <-
                  paste(recomendation,
                        "( Recommendecd <span style='color:#D7BE69'>")
                
                for (r in 1:as.numeric(result[j,]$userPriority)) {
                  recomendation <- paste(recomendation, "&#9733; ")
                }
                recomendation <- paste(recomendation, ") </span>")
              }
              resultString <-
                paste(
                  resultString,
                  "<span style= 'margin-left:30px;padding:0'><span style='font-size:14px;'> ",
                  lubridate::as_date(result[j,]$dateTimeEntry),
                  recomendation,
                  " : ",
                  result[j,]$markdown,
                  "\n</span><br/>"
                )
            }
            resultString <- paste(resultString, "</div>")
          }
        }
        
      }
    }
    return(withMathJax(HTML(
      markdown::renderMarkdown(text = knitr::knit(text = resultString, quiet = TRUE))
    )))
    
  }
  
  currentReadingTab <- shiny::reactiveVal()
  
  output$searchCommentResults <- shiny::renderUI({
    idx <- input$cohortSearchTableResults_rows_selected
    if (length(idx) > 0) {
      currentReadingTab('search')
      cohortIds <- cohort[idx,]$cohortId %>% unique()
      genrateResponseFromGoogleSheet(cohortIds,"All")
    }
  })
  
  output$cohortCountsCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('cohortCounts')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$incidenceRateCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('incidenceRate')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$timeDistributionCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('timeDistributions')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$inclusionRuleStatisticsCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('inclusionRuleStatistics')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$indexEventBreakDownCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('indexEventBreakdown')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$visitContextCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('visitContext')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$cohortCharacterizationCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('cohortCharacterization')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$compareCohortCharCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('compareCohortChar')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$temporalCharacterizationCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('temporalCharacterization')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  output$cohortOverlapCommentResults <- shiny::renderUI({
    cohortIds <- selectedCohortIds()
    databaseIds <- selectedDatabaseIds()
    if (length(cohortIds) > 0) {
      currentReadingTab('cohortOverlap')
      genrateResponseFromGoogleSheet(cohortIds,databaseIds)
    }
  })
  
  
  
  output$dynamicUIGenerationCohortDetailsOne <- shiny::renderUI(
    shiny::column(
      searchTableRowIsSelected(),
      shiny::conditionalPanel(
        "output.cohortSearchResultsCountOfSelected > 0&input.compareCohorts=='No Comparision'",
        shiny::tabsetPanel(
          id = "cohortDetails",
          type = "tab",
          shiny::tabPanel(title = "Description",
                          value = "descriptionFirst",
                          copyToClipboardButton(toCopyId = "cohortDetailsTextFirst", 
                                                style = "margin-top: 5px; margin-bottom: 5px;"),
                          shiny::htmlOutput("cohortDetailsTextFirst")),
          shiny::tabPanel(
            value = "cohortDefinitionFirst",
            title = "Cohort definition",
            copyToClipboardButton(toCopyId = "cohortDefinitionDetailsFirst", 
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::htmlOutput(outputId = "cohortDefinitionDetailsFirst")
          ),
          shiny::tabPanel(
            value = "cohortDefinitionConceptsetFirst",
            title = "Concept Sets",
            DT::DTOutput(outputId = "cohortDefinitionConceptSetsTableFirst"),
            shiny::conditionalPanel(
              condition = "output.cohortConceptSetsSelectedFirstRowIsSelected == true",
              shiny::tabsetPanel(
                id = "conceptsetExpressionTabFirst",
                shiny::tabPanel(
                  value = "conceptsetExpressionFirst",
                  title = "Expression",
                  DT::DTOutput(outputId = "cohortConceptsetExpressionDataTableFirst")
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionJsonFirst",
                  title = "Json",
                  copyToClipboardButton(toCopyId = "cohortConceptsetExpressionJsonFirst", 
                                        style = "margin-top: 5px; margin-bottom: 5px;"),
                  shiny::verbatimTextOutput(outputId = "cohortConceptsetExpressionJsonFirst"),
                  tags$head(
                    tags$style(
                      "#cohortConceptsetExpressionJsonFirst { max-height:400px};"
                    )
                  )
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionResolvedFirst",
                  title = "Resolved",
                  shinydashboard::box(
                    title = "Left Panel",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = NULL,
                    shiny::tabsetPanel(
                      id = "resolvedConceptsetExpressionFirst",
                      shiny::tabPanel(
                        value = "resolvedConceptsetExpressionTabPanelFirst",
                        title = "Resolved",
                        DT::DTOutput(outputId = "resolvedConceptSetExpressionDtStandardFirst"),
                      ),
                      shiny::tabPanel(
                        value = "mappedConceptsetExpressionTabPanelFirst",
                        title = "Mapped standard to non standard",
                        DT::DTOutput(outputId = "resolvedConceptSetExpressionDtMappedFirst")
                      )
                    )
                  )
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionOptimizedFirst",
                  title = "Optimized",
                  shiny::tabsetPanel(
                    id = "optimizedConceptsetExpressionFirst",
                    shiny::tabPanel(
                      value = "retainedConceptsetExpressionFirst",
                      title = "Retained",
                      DT::DTOutput(outputId = "optimizedConceptSetExpressionDtRetainedFirst")
                    ),
                    shiny::tabPanel(
                      value = "removedConceptsetExpressionFirst",
                      title = "Removed",
                      DT::DTOutput(outputId = "optimizedConceptSetExpressionDtRemovedFirst")
                    )),
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionRecommendedFirst",
                  title = "Recommended",
                  shiny::tabsetPanel(
                    id = "recommendedConceptsetExpressionFirst",
                    shiny::tabPanel(
                      value = "standartRecommendedConceptSetExpressionFirst",
                      title = "Standard",
                      DT::DTOutput(outputId = "recommendedConceptSetExpressionDtStandardFirst")
                    ),
                    shiny::tabPanel(
                      value = "nonStandartRecommendedConceptSetExpressionFirst",
                      title = "Non Standard",
                      DT::DTOutput(outputId = "recommendedConceptSetExpressionDtSourceFirst")
                    )),
                )
              )
            )
          ),
          shiny::tabPanel(
            value = "cohortDefinitionJsonFirst",
            title = "JSON",
            copyToClipboardButton(toCopyId = "cohortDefinitionJsonFirst", 
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::verbatimTextOutput(outputId = "cohortDefinitionJsonFirst"),
            tags$head(
              tags$style(
                "#cohortDefinitionJsonFirst { max-height:400px};"
              )
            )
          ),
          shiny::tabPanel(
            value = "cohortDefinitionSqlFirst",
            title = "SQL",
            copyToClipboardButton(toCopyId = "cohortDefinitionSqlFirst", 
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::verbatimTextOutput(outputId = "cohortDefinitionSqlFirst"),
            tags$head(
              tags$style(
                "#cohortDefinitionSqlFirst { max-height:400px};"
              )
            )
          )
        )
      )
    )
  )
  output$dynamicUIGenerationCohortDetailsTwo <- shiny::renderUI(
    shiny::column(
      searchTableRowIsSelected(),
      shiny::conditionalPanel(
        "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='No Comparision'",
        shiny::tabsetPanel(
          id = "cohortDetailsSecond",
          type = "tab",
          shiny::tabPanel(title = "Description",
                          value = "descriptionSecond",
                          copyToClipboardButton(toCopyId = "cohortDetailsTextSecond", 
                                                style = "margin-top: 5px; margin-bottom: 5px;"),
                          shiny::htmlOutput(outputId = "cohortDetailsTextSecond")),
          shiny::tabPanel(
            value = "cohortDefinitionSecond",
            title = "Cohort definition",
            copyToClipboardButton(toCopyId = "cohortDefinitionDetailsSecond", 
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::htmlOutput(outputId = "cohortDefinitionDetailsSecond")
          ),
          shiny::tabPanel(
            value = "cohortDefinitionConceptsetSecond",
            title = "Concept Sets",
            DT::DTOutput(outputId = "cohortDefinitionConceptSetsTableSecond"),
            shiny::conditionalPanel(
              condition = "output.cohortConceptSetsSelectedSecondRowIsSelected == true",
              shiny::tabsetPanel(
                id = "conceptsetExpressionTabSecond",
                shiny::tabPanel(
                  value = "conceptsetExpressionSecond",
                  title = "Expression",
                  DT::DTOutput(outputId = "cohortConceptsetExpressionDataTableSecond")
                ),
                shiny::tabPanel(
                  value = "conceptetExpressionJsonSecond",
                  title = "Json",
                  copyToClipboardButton(toCopyId = "cohortConceptsetExpressionJsonSecond", 
                                        style = "margin-top: 5px; margin-bottom: 5px;"),
                  shiny::verbatimTextOutput(outputId = "cohortConceptsetExpressionJsonSecond"),
                  tags$head(
                    tags$style(
                      "#cohortConceptsetExpressionJsonSecond { max-height:400px};"
                    )
                  )
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionResolvedSecond",
                  title = "Resolved",
                  shinydashboard::box(
                    title = "Right panel",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = NULL,
                    shiny::tabsetPanel(
                      id = "resolvedConceptsetExpressionSecond",
                      shiny::tabPanel(
                        value = "resolvedConceptsetExpressionTabPanelSecond",
                        title = "Standard",
                        DT::DTOutput(outputId = "resolvedConceptSetExpressionDtStandardSecond")
                      ),
                      shiny::tabPanel(
                        value = "mappedConceptsetExpressionTabPanelSecond",
                        title = "Mapped standard to non standard",
                        DT::DTOutput(outputId = "resolvedConceptSetExpressionDtMappedSecond")
                      )
                    )
                  )
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionOptimizedSecond",
                  title = "Optimized",
                  shiny::tabsetPanel(
                    id = "optimizedConceptsetExpressionSecond",
                    shiny::tabPanel(
                      value = "retainedConceptsetExpressionSecond",
                      title = "Retained",
                      DT::DTOutput(outputId = "optimizedConceptSetExpressionDtRetainedSecond")
                    ),
                    shiny::tabPanel(
                      value = "removedConceptsetExpressionSecond",
                      title = "Removed",
                      DT::DTOutput(outputId = "optimizedConceptSetExpressionDtRemovedSecond")
                    )),
                ),
                shiny::tabPanel(
                  value = "conceptsetExpressionRecommendedSecond",
                  title = "Recommended",
                  shiny::tabsetPanel(
                    id = "recommendedConceptsetExpressionSecond",
                    shiny::tabPanel(
                      value = "standardRecommendedConceptsetExpressionSecond",
                      title = "Standard",
                      DT::DTOutput(outputId = "recommendedConceptSetExpressionDtStandardSecond")
                    ),
                    shiny::tabPanel(
                      value = "nonStandardRecommendedConceptsetExpressionSecond",
                      title = "Non Standard",
                      DT::DTOutput(outputId = "recommendedConceptSetExpressionDtSourceSecond")
                    )),
                )
              )
            )
          ),
          shiny::tabPanel(
            value = "cohortDefinitionJsonSecond",
            title = "JSON",
            copyToClipboardButton(toCopyId = "cohortDefinitionJsonSecond", 
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::verbatimTextOutput("cohortDefinitionJsonSecond"),
            tags$head(
              tags$style(
                "#cohortDefinitionJsonSecond { max-height:400px};"
              )
            )
          ),
          shiny::tabPanel(
            value = "cohortDefinitionSqlSecond",
            title = "SQL",
            copyToClipboardButton(toCopyId = "cohortDefinitionSqlSecond", 
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::verbatimTextOutput("cohortDefinitionSqlSecond"),
            tags$head(
              tags$style(
                "#cohortDefinitionSqlSecond { max-height:400px};"
              )
            )
          )
        )
      )
    )
  )
  
  
  output$isResolveClicked <- shiny::reactive({
    return(input$conceptsetExpressionTabFirst == 'conceptsetExpressionResolvedFirst')
  })
  
  shiny::outputOptions(x = output,
                       name = "isResolveClicked",
                       suspendWhenHidden = FALSE)
  
  ############### search tab ######################################
  rvCohortSearch <- shiny::reactiveValues()
  # Cohort search results
  cohortSearchResults <- shiny::reactive(x = {
    
    regularexpression <- gsub("[^0-9A-Za-z///' ]","" , input$searchText ,ignore.case = TRUE)
    if (input$searchText != "" && regularexpression != "") {
      searchString <- input$searchText
      searchFieldWeight <- dplyr::tibble(
        searchFields = c("cohortName","referentConceptIdsSearchTerms",
                         "json","logicDescription","phenotypeName",
                         "cohortType"),
        searchPoints = c(5,3,2,1,2,1))
      
      searchStringSplit <-
        stringr::str_split(string = tolower(searchString),
                           pattern = " ")[[1]]
      
      searchInField <- function(searchTable = 'cohort',
                                searchString,
                                searchField,
                                points) {
        if (searchField %in% colnames(searchTable)) {
          data <- searchTable %>%
            dplyr::filter(stringr::str_detect(
              string = tolower(.data[[searchField]]),
              pattern = tolower(searchString)
            )) %>%
            dplyr::select(.data$cohortId) %>%
            dplyr::mutate(points = points) %>% 
            dplyr::mutate(wordSearched = word)
          return(data)
        }
      }
      
      searchResultByWords <- list()
      for (i in (1:length(searchStringSplit))) {
        word <- searchStringSplit[[i]]
        searchResult <- list()
        for (j in (1:nrow(searchFieldWeight))) {
          searchResult[[j]] <- searchInField(searchTable = cohort,
                                             searchField = searchFieldWeight[j,]$searchFields,
                                             searchString = word,
                                             points = searchFieldWeight[j,]$searchPoints)
        }
        searchResultByWords[[i]] <- dplyr::bind_rows(searchResult)
      }
      data <- dplyr::bind_rows(searchResultByWords) %>% 
        dplyr::group_by(.data$cohortId) %>%
        dplyr::summarise(points = sum(points))  %>% 
        dplyr::inner_join(y = cohort, by = "cohortId") %>% 
        dplyr::arrange(dplyr::desc(points)) %>% 
        dplyr::select(-.data$points) %>% 
        dplyr::distinct()
    } else {
      data <- cohort
    }
    return(data)
  })
  
  
  
  rvCohortSearch$noSearchResult <- shiny::reactiveVal(value = FALSE, label = "Search result return indicator")
  output$cohortSearchTableResults <- DT::renderDT(expr = {
    data <- cohortSearchResults()
    if (nrow(data) == 0) {
      rvCohortSearch$noSearchResult(TRUE)
      return(dplyr::tibble(Note = "Search did not result any cohort"))
    }
    if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
      data <- data %>%
        dplyr::select(.data$phenotypeId,
                      .data$phenotypeName,
                      .data$cohortId,
                      .data$cohortName)
    } else {
      data <- data %>%
        dplyr::select(.data$cohortId,
                      .data$cohortName)
    }
    # selectedRowindex <- c()
    # queryStringList <- shiny::getQueryString()
    # if (length(queryStringList) > 0) {
    #   cohortIds <- as.integer(strsplit(x = queryStringList$cohortIds[1],split = ",")[[1]])
    #   data1 <- as.data.frame(data)
    #   selectedRowindex <- as.integer(rownames(data1[data1$cohortId %in% cohortIds,]))
    # }
    
    table <- standardDataTable(data = data,
                               selectionMode = "multiple" #,selected = selectedRowindex
                               )
    return(table)
  }, server = TRUE)
  
  ## Appending Query String to URL
  queryStringCohortIds <- reactiveVal(c())
  observeEvent(eventExpr = input$cohortSearchTableResults_rows_selected,handlerExpr = {
    idx <- input$cohortSearchTableResults_rows_selected
    queryStringCohortIds(as.vector(cohortSearchResults()[idx, ]$cohortId))
  })
  
  queryStringInputTab <- reactiveVal("")
  observeEvent(eventExpr = input$tabs,handlerExpr = {
    queryStringInputTab(input$tabs)
  })
  
  queryStringDatabaseIds <- reactiveVal(c())
  observeEvent(eventExpr = input$selectedDatabases,handlerExpr = {
    databaseIds <-  database %>% 
      dplyr::filter(.data$databaseName %in% input$selectedDatabases) %>% 
      dplyr::select(.data$databaseId) %>% 
      dplyr::pull()
    queryStringDatabaseIds(databaseIds)
  })
  
  #clicking "LoadSelectedCohort" button if Value found in query string initially.
  clickLoadSelectedCohortButton <- reactiveVal(NULL)
  observeEvent(eventExpr = list(getQueryString(),input$cohortSearchTableResults_rows_selected),handlerExpr = {
    if (length(input$cohortSearchTableResults_rows_selected) > 0 && length(getQueryString()) > 0 && input$tabs != getQueryString()$inputTab) {
      clickLoadSelectedCohortButton(TRUE)
    }
  })
  
  observeEvent(eventExpr = clickLoadSelectedCohortButton(),
               handlerExpr = {
                 isolate({shinydashboard::updateTabItems(session, "tabs", getQueryString()$inputTab)})
                 databaseIds <- strsplit(getQueryString()$databaseIds,split = ",")[[1]]
                 tempSelectedDatabaseNames <-  database %>% 
                   dplyr::filter(.data$databaseId %in% databaseIds) %>% 
                   dplyr::select(.data$databaseName) %>% 
                   dplyr::pull()
                 shinyWidgets::updatePickerInput(session = session,inputId = "selectedDatabases",selected = tempSelectedDatabaseNames)
               })
  
  
  finalQueryString <- reactiveVal()
  observeEvent(eventExpr = list(queryStringCohortIds(),queryStringInputTab(),queryStringDatabaseIds()),handlerExpr = {
    if ((queryStringInputTab() != "" && queryStringInputTab() != "search") || length(queryStringCohortIds() > 0)) {
      finalQueryString("?cohortIds=")
      finalQueryString(paste0(finalQueryString(),paste0(queryStringCohortIds(),collapse = ",")))
      finalQueryString(paste0(finalQueryString(),"&inputTab="))
      finalQueryString(paste0(finalQueryString(),queryStringInputTab()))
      finalQueryString(paste0(finalQueryString(),"&databaseIds="))
      finalQueryString(paste0(finalQueryString(),paste0(queryStringDatabaseIds(),collapse = ",")))
      shiny::updateQueryString(finalQueryString(),mode = "push")
    }
  })
  
  # selection of rows
  cohortSearchResultRecentTwoSelection <- shiny::reactive(x = {
    idx <- input$cohortSearchTableResults_rows_selected
    if (length(idx) > 1  && 
        isFALSE(rvCohortSearch$noSearchResult())) {
      # get the last two rows selected
      lastRowsSelected <- idx[c(length(idx), length(idx) - 1)]
      rvCohortSearch$twoRowsSelected <- TRUE
    } else {
      lastRowsSelected <- idx
      rvCohortSearch$twoRowsSelected <- FALSE
    }
    return(cohortSearchResults()[lastRowsSelected,])
  })
  
  computeDataForUpsetplot <- shiny::reactive({
    idx <- input$cohortSearchTableResults_rows_selected
    selectedCohortRows <- cohortSearchResults()[idx, ]
    conceptSetDetails <- list()
    cohortWithConceptSetDataFrame <-
      data.frame() %>% dplyr::tibble()
    if (nrow(selectedCohortRows) <= 0) {
      return(NULL)
    }
    shiny::withProgress(message = 'Computing Data for Upset Plot', {
      for (i in 1:nrow(selectedCohortRows)) {
        conceptSetDetails[[i]] <-
          getConceptSetDetailsFromCohortDefinition(cohortDefinitionExpression =
                                                     RJSONIO::fromJSON(selectedCohortRows[i,]$json))
        conceptSetExpression <-
          conceptSetDetails[[i]]$conceptSetExpression
        if (nrow(selectedCohortRows) > 0) {
          for (j in 1:nrow(conceptSetExpression)) {
            expression <-
              conceptSetDetails[[i]]$conceptSetExpression[j,]$expression
            data <-
              ConceptSetDiagnostics::resolveConceptSetExpression(connection = dataSource$connection,
                                                                 conceptSetExpression = expression)
            resolvedConceptIds <- data$resolved$conceptId %>% unique()
            resolvedConceptIdCounts <-
              ConceptSetDiagnostics::getConceptPrevalenceCountsForConceptIds(connection = dataSource$connection,
                                                                             conceptIdsList = resolvedConceptIds)
            data$resolvedConcepts <- data$resolvedConcepts %>%
              dplyr::left_join(y = resolvedConceptIdCounts, by = "conceptId") %>%
              dplyr::arrange(dplyr::desc(.data$drc)) %>%
              dplyr::distinct() %>%
              dplyr::select(.data$conceptId,
                            .data$conceptName,
                            .data$rc,
                            .data$dbc)
            
            # data$resolvedConcepts$conceptId <- as.factor(data$resolvedConcepts$conceptId)
            data$resolvedConcepts$cohortId <-
              selectedCohortRows[i,]$cohortId
            data$resolvedConcepts$cohortName <-
              selectedCohortRows[i,]$cohortName
            data$resolvedConcepts$cohortShortName <- paste0("c", i)
            data$resolvedConcepts$value <- as.vector(1)
            
            cohortWithConceptSetDataFrame <-
              rbind(cohortWithConceptSetDataFrame,
                    data$resolvedConcepts)
          }
        }
      }
    })
    return(cohortWithConceptSetDataFrame)
  })
  
  output$conceptSetsForUpsetTable <- DT::renderDT(expr = {
    dataTable <-
      standardDataTable(data = computeDataForUpsetplot(),
                        pageLength = 5,
                        selectionMode = "multiple",
                        selected = c())
    return(dataTable)
  })
  
  output$upsetPlotForConceptSets <- shiny::renderPlot({
    validate(need(
      input$cohortSearchTableResults_rows_selected > 1,
      paste0("Please select atleast two different cohorts to show the plot")
    ))
    
    data <- computeDataForUpsetplot()
    idx <- input$conceptSetsForUpsetTable_rows_selected
    if (!is.null(idx)) {
      data <- data[idx, ]
    }
    
    noOfCohorts <- data %>% 
      dplyr::distinct(.data$cohortId)
    
    validate(need(
      nrow(noOfCohorts) > 1,
      paste0("Please select atleast two different cohorts to show the plot")
    ))
    
    if (is.null(data)) {
      return(NULL)
    }
    
    upsetColumns <-
      data$cohortShortName %>%
      unique()
    
    data[is.na(data)] <- 0
    resultData <- data %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(
        id_cols = c(.data$conceptId, .data$conceptName, .data$rc, .data$dbc),
        names_from = .data$cohortShortName,
        values_from = .data$value,
        values_fill = as.vector(0)
      )
    
    plot <- UpSetR::upset(
      as.data.frame(resultData),
      sets = c(upsetColumns),
      sets.x.label = "No of Concept Sets",
      boxplot.summary = c("rc", "dbc"),
      order.by = "freq"
    )
    return(plot)
  })
  
  #circeR human readable description
  cohortSearchResultRecentTwoSelectionCirceRDetails <- shiny::reactive(x = {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering human readable cohort description using CirceR", value = 0)
    # shiny::withProgress(
    #   message = "Calling CirceR. Rendering human readable description.",
    #   expr = {
    data <- cohortSearchResultRecentTwoSelection()
    if (nrow(cohortSearchResultRecentTwoSelection()) > 0) {
      details <- list()
      for (i in (1:nrow(data))) {
        progress$inc(1/nrow(data), detail = paste("Doing part", i))
        circeExpression <-
          CirceR::cohortExpressionFromJson(expressionJson = data[i, ]$json)
        circeExpressionMarkdown <-
          CirceR::cohortPrintFriendly(circeExpression)
        circeConceptSetListmarkdown <-
          CirceR::conceptSetListPrintFriendly(circeExpression$conceptSets)
        details[[i]] <- data[i, ]
        details[[i]]$circeConceptSetListmarkdown <-
          circeConceptSetListmarkdown
        details[[i]]$htmlExpressionCohort <-
          convertMdToHtml(circeExpressionMarkdown)
        details[[i]]$htmlExpressionConceptSetExpression <-
          convertMdToHtml(circeConceptSetListmarkdown)
        
      }
      details <- dplyr::bind_rows(details)
    } else {
      return(NULL)
    }
    return(details)
    
  })
  
  # count number of rows selected
  cohortSearchResultNumberOfSelectedRows <- shiny::reactive({
    return(length(input$cohortSearchTableResults_rows_selected))
  })
  output$cohortSearchResultsCountOfSelected <- shiny::reactive({
    return(cohortSearchResultNumberOfSelectedRows())
  })
  shiny::outputOptions(x = output,
                       name = "cohortSearchResultsCountOfSelected",
                       suspendWhenHidden = FALSE)
  
  # Details of cohort
  cohortDetailsTextReactive <- shiny::reactive(x = {
    data <- cohortSearchResultRecentTwoSelection()
    if (!is.null(data) && nrow(data) > 0) {
      if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
        phenotypeDetails <- phenotypeDescription %>%
          dplyr::filter(.data$phenotypeId %in% data$phenotypeId) %>% 
          dplyr::select(.data$phenotypeId,
                        .data$overview,
                        .data$presentation,
                        .data$assessment,
                        .data$plan,
                        .data$prognosis,
                        .data$phenotypeSynonyms)
        data <- data %>%
          dplyr::left_join(y = phenotypeDetails,
                           by = "phenotypeId")
      } else {
        colnamesInData <- colnames(data)
        if (!'phenotypeName' %in% colnamesInData) {
          data$phenotypeName <- "Unassigned"
        }
        if (!'overview' %in% colnamesInData) {
          data$overview <- ""
        }
        if (!'presentation' %in% colnamesInData) {
          data$presentation <- ""
        }
        if (!'assessment' %in% colnamesInData) {
          data$assessment <- ""
        }
        if (!'plan' %in% colnamesInData) {
          data$plan <- ""
        }
        if (!'prognosis' %in% colnamesInData) {
          data$prognosis <- ""
        }
        if (!'phenotypeSynonyms' %in% colnamesInData) {
          data$phenotypeSynonyms <- ""
        }
      }
      if (is.null(data)) {
        return(NULL)
      } else {
        details <- list()
        colNamesData <- colnames(data)
        if (!'logicDescription' %in% colNamesData) {
          data$logicDescription <- "Not given."
        }
        if (!'referentConceptIdsSearchTerms' %in% colNamesData) {
          data$referentConceptIdsSearchTerms <- 0
        }
        for (i in (1:nrow(data))) {
          details[[i]] <-       tags$table(
            style = "margin-top: 5px;",
            tags$tr(
              tags$td(tags$strong("Cohort ID: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$cohortId)
            ),
            tags$tr(
              tags$td(tags$strong("Cohort Name: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$cohortName)
            ),
            tags$tr(
              tags$td(tags$strong("Logic: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$logicDescription)
            ),
            tags$tr(
              tags$td(tags$strong("Synonyms: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$referentConceptIdsSearchTerms)
            ),
            tags$tr(
              tags$td(tags$strong("Phenotype Name: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(paste(data[i, ]$phenotypeName, " (",data[i, ]$phenotypeId,")"))
            ),
            tags$tr(
              tags$td(tags$strong("Phenotype Synonyms: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(paste(data[i, ]$phenotypeSynonyms, " (",data[i, ]$phenotypeSynonyms,")"))
            ),
            tags$tr(
              tags$td(tags$strong("Overview: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$overview)
            ),
            tags$tr(
              tags$td(tags$strong("Presentation: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$presentation)
            ),
            tags$tr(
              tags$td(tags$strong("Assessment: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$assessment)
            ),
            tags$tr(
              tags$td(tags$strong("Plan: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$plan)
            ),
            tags$tr(
              tags$td(tags$strong("Prognosis: ")),
              tags$td(HTML("&nbsp;&nbsp;")),
              tags$td(data[i, ]$prognosis)
            )
          )
        }
        return(details)
      }
    }
  })
  
  cohortConceptSets <- shiny::reactive(x = {
    if (is.null(cohortSearchResultRecentTwoSelection())) {
      return(NULL)
    } else {
      details <- list()
      for (i in 1:nrow(cohortSearchResultRecentTwoSelection())) {
        details[[i]] <- getConceptSetDetailsFromCohortDefinition(
          cohortDefinitionExpression =
            RJSONIO::fromJSON(cohortSearchResultRecentTwoSelection()[i,]$json)
        )
      }
      return(details)
    }
  })
  
  output$cohortDetailsTextFirst <- shiny::renderUI(expr = {
    return(cohortDetailsTextReactive()[[1]])
  })
  output$cohortDefinitionJsonFirst <- shiny::renderText({
    cohortSearchResultRecentTwoSelection()[1,]$json
  }) 
  output$cohortDefinitionSqlFirst <- shiny::renderText({
    cohortSearchResultRecentTwoSelection()[1,]$sql
  })
  output$cohortDefinitionDetailsFirst <- shiny::renderUI(expr = {
    cohortSearchResultRecentTwoSelectionCirceRDetails()[1, ]$htmlExpressionCohort %>%
      shiny::HTML()
  })
  output$cohortDefinitionConceptSetsTableFirst <-
    DT::renderDT(expr = {
      if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression) &&
          nrow(cohortConceptSets()[[1]]$conceptSetExpression) > 0) {
        data <- cohortConceptSets()[[1]]$conceptSetExpression %>%
          dplyr::select(.data$id, .data$name)
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  
  output$cohortDetailsTextSecond <- shiny::renderUI(expr = {
    if (!is.null(cohortDetailsTextReactive()) &&
        length(cohortDetailsTextReactive()) == 2 &&
        !is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortDetailsTextReactive()[[2]])
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionJsonSecond <- shiny::renderText({
    if (!is.null(cohortDetailsTextReactive()) &&
        length(cohortDetailsTextReactive()) == 2 &&
        !is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortSearchResultRecentTwoSelection()[2,]$json)
    } else {
      return(NULL)
    }
  }) 
  output$cohortDefinitionSqlSecond <- shiny::renderText({
    if (!is.null(cohortSearchResultRecentTwoSelection()) &&
        nrow(cohortSearchResultRecentTwoSelection()) == 2 &&
        !is.null(cohortSearchResultRecentTwoSelection()[[2]])) {
      return(cohortSearchResultRecentTwoSelection()[2,]$sql)
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionDetailsSecond <- shiny::renderUI(expr = {
    if (!is.null(cohortDetailsTextReactive()) &&
        length(cohortDetailsTextReactive()) == 2 &&
        !is.null(cohortDetailsTextReactive()[[2]])) {
      return(cohortSearchResultRecentTwoSelectionCirceRDetails()[2, ]$htmlExpressionCohort %>%
               shiny::HTML())
    } else {
      return(NULL)
    }
  })
  output$cohortDefinitionConceptSetsTableSecond <-
    DT::renderDT(expr = {
      if (length(cohortConceptSets()) == 2) {
        data <- cohortConceptSets()[[2]]$conceptSetExpression
        if (nrow(data) > 0) {
          data <- data %>%
            dplyr::select(.data$id, .data$name)
          dataTable <- standardDataTable(data = data)
          return(dataTable)
        }
      } else {
        return(NULL)
      }
    })
  
  # synchronize the selection of tabset panels when comparing two cohorts
  shiny::observeEvent(eventExpr = {
    input$cohortSearchTableResults_rows_selected
  },handlerExpr = {
    if (searchTableRowIsSelected() == 6) {
      if (!is.null(input$cohortDetails)) {
        if (input$cohortDetails == "descriptionFirst") {
          shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "descriptionSecond")
        } else if (input$cohortDetails == "cohortDefinitionFirst")  {
          shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionSecond")
        } else if (input$cohortDetails == "cohortDefinitionConceptsetFirst") {
          shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionConceptsetSecond")
        } else if (input$cohortDetails == "cohortDefinitionJsonFirst") {
          shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionJsonSecond")
        } else if (input$cohortDetails == "cohortDefinitionSqlFirst") {
          shiny::updateTabsetPanel(session, inputId = "cohortDetailsSecond", selected = "cohortDefinitionSqlSecond")
        }
      }
      
      if (!is.null(input$conceptsetExpressionTabFirst)) {
        if (input$conceptsetExpressionTabFirst == "conceptsetExpressionFirst") {
          shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionSecond")
        } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionJsonFirst") {
          shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptetExpressionJsonSecond")
        } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionResolvedFirst") {
          shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionResolvedSecond")
        } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionOptimizedFirst") {
          shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionOptimizedSecond")
        } else if (input$conceptsetExpressionTabFirst == "conceptsetExpressionRecommendedFirst") {
          shiny::updateTabsetPanel(session, inputId = "conceptsetExpressionTabSecond", selected = "conceptsetExpressionRecommendedSecond")
        }
      }
      
      if (!is.null(input$resolvedConceptsetExpressionFirst)) {
        if (input$resolvedConceptsetExpressionFirst == "resolvedConceptsetExpressionTabPanelFirst") {
          shiny::updateTabsetPanel(session, inputId = "resolvedConceptsetExpressionSecond", selected = "resolvedConceptsetExpressionTabPanelSecond")
        } else if (input$resolvedConceptsetExpressionFirst == "mappedConceptsetExpressionTabPanelFirst") {
          shiny::updateTabsetPanel(session, inputId = "resolvedConceptsetExpressionSecond", selected = "mappedConceptsetExpressionTabPanelFirst")
        }
      }
      
      if (!is.null(input$optimizedConceptsetExpressionFirst)) {
        if (input$optimizedConceptsetExpressionFirst == "retainedConceptsetExpressionFirst") {
          shiny::updateTabsetPanel(session, inputId = "optimizedConceptsetExpressionSecond", selected = "retainedConceptsetExpressionSecond")
        } else if (input$optimizedConceptsetExpressionFirst == "removedConceptsetExpressionFirst") {
          shiny::updateTabsetPanel(session, inputId = "optimizedConceptsetExpressionSecond", selected = "removedConceptsetExpressionSecond")
        }
      }
      
      if (!is.null(input$recommendedConceptsetExpressionFirst)) {
        if (input$recommendedConceptsetExpressionFirst == "standartRecommendedConceptSetExpressionFirst") {
          shiny::updateTabsetPanel(session, inputId = "recommendedConceptsetExpressionSecond", selected = "standardRecommendedConceptsetExpressionSecond")
        } else if (input$recommendedConceptsetExpressionFirst == "nonStandartRecommendedConceptSetExpressionFirst") {
          shiny::updateTabsetPanel(session, inputId = "recommendedConceptsetExpressionSecond", selected = "nonStandardRecommendedConceptsetExpressionSecond")
        }
      }
    }
  })
  
  # enable comparison of two cohorts. When one is selected, default to no comparison.
  shiny::observeEvent(eventExpr = cohortSearchResultNumberOfSelectedRows() != 2,
                      handlerExpr = {
                        shinyWidgets::updatePickerInput(session = session,
                                                        inputId = "compareCohorts",
                                                        selected = "No Comparision")
                      })
  
  
  # selected concept set in a cohort definition.
  cohortConceptSetsSelectedFirst <- shiny::reactive(x = {
    if (is.null(input$cohortDefinitionConceptSetsTableFirst_rows_selected)) {
      return(NULL)
    } else {
      idx <- input$cohortDefinitionConceptSetsTableFirst_rows_selected
      if (length(idx) > 0) {
        if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression) &&
            nrow(cohortConceptSets()[[1]]$conceptSetExpression) > 0) {
          data <- cohortConceptSets()[[1]]$conceptSetExpression[idx, ]
          return(data)
        }
      }     
    }
  })
  output$cohortConceptSetsSelectedFirstRowIsSelected <- shiny::reactive(x = {
    return(!is.null(cohortConceptSetsSelectedFirst()))
  })
  shiny::outputOptions(x = output,
                       name = "cohortConceptSetsSelectedFirstRowIsSelected",
                       suspendWhenHidden = FALSE)
  output$cohortConceptsetExpressionDataTableFirst <-
    DT::renderDT(expr = {
      if (!is.null(cohortConceptSetsSelectedFirst())) {
        data <- cohortConceptSetsSelectedFirst()
        data <- cohortConceptSets()[[1]]$conceptSetExpressionDetails
        data <- data %>%
          dplyr::filter(.data$id == cohortConceptSetsSelectedFirst()$id)
        data <- data %>% 
          dplyr::select(.data$conceptId, .data$conceptName,
                        .data$isExcluded, .data$includeDescendants,
                        .data$includeMapped, 
                        .data$standardConcept, .data$invalidReason,
                        .data$conceptCode, .data$domainId, 
                        .data$vocabularyId, .data$conceptClassId) %>% 
          dplyr::rename(invalid = .data$invalidReason,
                        code = .data$conceptCode,
                        id = .data$conceptId,
                        name = .data$conceptName,
                        standard = .data$standardConcept,
                        exclude = .data$isExcluded,
                        descendants = .data$includeDescendants,
                        mapped = .data$includeMapped) %>% 
          dplyr::mutate(exclude = as.integer(.data$exclude),
                        descendants = as.integer(.data$descendants),
                        mapped = as.integer(.data$mapped))
        dataTable <- standardDataTable(data = data, selectionMode = "single")
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  output$cohortConceptsetExpressionJsonFirst <- shiny::renderText({
    cohortConceptSetsSelectedFirst()$json
  })
  
  
  cohortConceptSetsSelectedSecond <- shiny::reactive(x = {
    if (is.null(input$cohortDefinitionConceptSetsTableSecond_rows_selected)) {
      return(NULL)
    } else {
      idx <- input$cohortDefinitionConceptSetsTableSecond_rows_selected
      if (length(idx) > 0 && length(cohortConceptSets()) == 2) {
        data <- cohortConceptSets()[[2]]$conceptSetExpression
        if (nrow(data)) {
          data <- data[idx, ]
          return(data)
        }
      } else {
        return(NULL)
      }
    }
  })
  output$cohortConceptSetsSelectedSecondRowIsSelected <- shiny::reactive(x = {
    return(!is.null(cohortConceptSetsSelectedSecond()))
  })
  shiny::outputOptions(x = output,
                       name = "cohortConceptSetsSelectedSecondRowIsSelected",
                       suspendWhenHidden = FALSE)
  output$cohortConceptsetExpressionDataTableSecond <-
    DT::renderDT(expr = {
      if (!is.null(cohortConceptSetsSelectedSecond())) {
        data <- cohortConceptSetsSelectedSecond()
        data <- cohortConceptSets()[[2]]$conceptSetExpressionDetails
        data <- data %>%
          dplyr::filter(.data$id == cohortConceptSetsSelectedSecond()$id)
        data <- data %>% 
          dplyr::select(.data$conceptId, .data$conceptName,
                        .data$isExcluded, .data$includeDescendants,
                        .data$includeMapped, 
                        .data$standardConcept, .data$invalidReason,
                        .data$conceptCode, .data$domainId, 
                        .data$vocabularyId, .data$conceptClassId) %>% 
          dplyr::rename(invalid = .data$invalidReason,
                        code = .data$conceptCode,
                        id = .data$conceptId,
                        name = .data$conceptName,
                        standard = .data$standardConcept,
                        exclude = .data$isExcluded,
                        descendants = .data$includeDescendants,
                        mapped = .data$includeMapped) %>% 
          dplyr::mutate(exclude = as.integer(.data$exclude),
                        descendants = as.integer(.data$descendants),
                        mapped = as.integer(.data$mapped))
        dataTable <- standardDataTable(data = data, selectionMode = "single")
        return(dataTable)
      } else {NULL}
    })
  output$cohortConceptsetExpressionJsonSecond <- shiny::renderText({
    cohortConceptSetsSelectedSecond()$json
  })
  
  
  # resolved concept set expression
  resolvedConceptSetExpressionReactiveFirst <-
    shiny::reactive(x = {
      if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression$json)) {
        shiny::withProgress(message = 'Resolving Concept Set Expression', {
          expression <- cohortConceptSetsSelectedFirst()$expression
          data <- ConceptSetDiagnostics::resolveConceptSetExpression(connection = dataSource$connection,
                                                                     conceptSetExpression = expression)
        })
        return(data)
      }
    })
  
  output$resolvedConceptSetExpressionDtStandardFirst <-
    DT::renderDT(expr = {
      if (!is.null(resolvedConceptSetExpressionReactiveFirst())) {
        data <- resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(NULL)
      }
    })
  
  
  output$resolvedConceptSetExpressionDtMappedFirst <-
    DT::renderDT(expr = {
      data <- resolvedConceptSetExpressionReactiveFirst()$mappedConcepts
      if (!is.null(data)) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  resolvedConceptSetExpressionReactiveSecond <-
    shiny::reactive(x = {
      if (length(cohortConceptSets()) == 2) {
        shiny::withProgress(message = 'Resolving Concept Set Expression', {
          expression <- cohortConceptSetsSelectedSecond()$expression
          data <- ConceptSetDiagnostics::resolveConceptSetExpression(connection = dataSource$connection,
                                                                     conceptSetExpression = expression)
          return(data)
        })
      } else {
        return(NULL)
      }
    })
  output$resolvedConceptSetExpressionDtStandardSecond <-
    DT::renderDT(expr = {
      data <- resolvedConceptSetExpressionReactiveSecond()
      if (!is.null(data)) {
        data <- data$resolvedConcepts
        if (!is.null(data) && nrow(data) > 0) {
          dataTable <- standardDataTable(data = data)
          return(dataTable)
        }
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  output$presentInLeftTable <- DT::renderDT({
    leftData <-
      resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
    rightData <-
      resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
    result <- dplyr::setdiff(leftData, rightData)
  })
  
  output$presentInRightTable <- DT::renderDT({
    leftData <-
      resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
    rightData <-
      resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
    result <- dplyr::setdiff(rightData, leftData)
  })
  
  output$presentInBothTable <- DT::renderDT({
    leftData <-
      resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
    rightData <-
      resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
    result <- dplyr::intersect(leftData, rightData)
  })
  
  
  output$resolvedConceptSetExpressionDtMappedSecond <-
    DT::renderDT(expr = {
      if (!is.null(resolvedConceptSetExpressionReactiveSecond()$mappedConcepts)) {
        data <- resolvedConceptSetExpressionReactiveSecond()$mappedConcepts
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  
  
  #optimized
  optimizedConceptSetExpressionReactiveFirst <-
    shiny::reactive(x = {
      if (!is.null(cohortConceptSets()[[1]]$conceptSetExpression$json)) {
        conceptSetExpression <- cohortConceptSetsSelectedFirst()$expression
        data <- ConceptSetDiagnostics::optimizeConceptSetExpression(
          connection = dataSource$connection,
          conceptSetExpression = conceptSetExpression,
          vocabularyDatabaseSchema = 'vocabulary'
        )
        return(data)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  output$optimizedConceptSetExpressionDtRetainedFirst <-
    DT::renderDT(expr = {
      data <-
        optimizedConceptSetExpressionReactiveFirst()$recommended %>%
        ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(
          updateVocabularyFields = TRUE,
          recordCount = TRUE,
          connection = dataSource$connection
        )
      if (!is.null(data) > 0 &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  output$optimizedConceptSetExpressionDtRemovedFirst <-
    DT::renderDT(expr = {
      data <-
        optimizedConceptSetExpressionReactiveFirst()$removed %>%
        ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(
          updateVocabularyFields = TRUE,
          recordCount = TRUE,
          connection = dataSource$connection
        )
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  optimizedConceptSetExpressionReactiveSecond <-
    shiny::reactive(x = {
      if (!is.null(cohortConceptSets()[[2]]$conceptSetExpression$json)) {
        conceptSetExpression <- cohortConceptSetsSelectedSecond()$expression
        data <- ConceptSetDiagnostics::optimizeConceptSetExpression(
          connection = dataSource$connection,
          conceptSetExpression = conceptSetExpression,
          vocabularyDatabaseSchema = 'vocabulary'
        )
        return(data)
      }
    })
  
  output$optimizedConceptSetExpressionDtRetainedSecond <-
    DT::renderDT(expr = {
      data <- optimizedConceptSetExpressionReactiveSecond()$recommended %>% 
        ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(updateVocabularyFields = TRUE,
                                                                                        recordCount = TRUE, 
                                                                                        connection = dataSource$connection)
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  output$optimizedConceptSetExpressionDtRemovedSecond <-
    DT::renderDT(expr = {
      data <- optimizedConceptSetExpressionReactiveSecond()$removed %>% 
        ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(updateVocabularyFields = TRUE,
                                                                                        recordCount = TRUE, 
                                                                                        connection = dataSource$connection)
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  
  
  #recommended
  recommendedConceptSetExpressionStandardReactiveFirst <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveFirst())) {
        resolvedConcepts <-
          resolvedConceptSetExpressionReactiveFirst()$resolvedConcepts
        mappedConcepts <-
          resolvedConceptSetExpressionReactiveFirst()$mappedConcepts
        conceptIds <-
          c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <-
            ConceptSetDiagnostics::getRecommendedStandard(connection = dataSource$connection,
                                                          conceptList = conceptIds)
          data <- data %>%
            dplyr::filter(.data$conceptId != 0) %>%
            dplyr::distinct()
        }
      }
      return(data)
    })
  
  recommendedConceptSetExpressionSourceReactiveFirst <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveFirst())) {
        resolvedconcepts <-
          resolvedConceptSetExpressionReactiveFirst()$resolvedconcepts
        mappedconcepts <-
          resolvedConceptSetExpressionReactiveFirst()$mappedconcepts
        conceptids <-
          c(resolvedconcepts$conceptid, mappedconcepts$conceptid) %>% unique()
        if (length(conceptids) > 0) {
          data <-
            conceptsetdiagnostics::getrecommendedsource(datasource = datasource$connection,
                                                        conceptlist = conceptids) %>%
            dplyr::distinct()
        }
      }
      return(data)
    })
  
  output$recommendedConceptSetExpressionDtStandardFirst <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionStandardReactiveFirst()
      data <- data %>% 
        dplyr::rename(id = .data$conceptId,
                      name = .data$conceptName,
                      standard = .data$standardConcept) %>% 
        dplyr::select(.data$conceptInSet, .data$id, .data$name,
                      .data$rc, .data$drc, .data$dbc, .data$dbc,
                      .data$vocabularyId, .data$domainId, .data$standard) %>% 
        dplyr::arrange(dplyr::desc(.data$drc))
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  output$recommendedConceptSetExpressionDtSourceFirst <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionSourceReactiveFirst()
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  recommendedConceptSetExpressionStandardReactiveSecond <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveSecond())) {
        resolvedConcepts <-
          resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
        mappedConcepts <-
          resolvedConceptSetExpressionReactiveSecond()$mappedConcepts
        conceptIds <-
          c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <-
            ConceptSetDiagnostics::getRecommendedStandard(connection = dataSource$connection,
                                                          conceptList = conceptIds) %>%
            dplyr::distinct()
        }
      }
      return(data)
    })
  
  recommendedConceptSetExpressionSourceReactiveSecond <-
    shiny::reactive(x = {
      data <- NULL
      if (!is.null(resolvedConceptSetExpressionReactiveSecond())) {
        resolvedConcepts <-
          resolvedConceptSetExpressionReactiveSecond()$resolvedConcepts
        mappedConcepts <-
          resolvedConceptSetExpressionReactiveSecond()$mappedConcepts
        conceptIds <-
          c(resolvedConcepts$conceptId, mappedConcepts$conceptId) %>% unique()
        if (length(conceptIds) > 0) {
          data <-
            ConceptSetDiagnostics::getRecommendedSource(connection = dataSource$connection,
                                                        conceptList = conceptIds) %>%
            dplyr::distinct()
        }
      }
      return(data)
    })
  
  output$recommendedConceptSetExpressionDtStandardSecond <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionStandardReactiveSecond()
      if (!is.null(data) &&
          nrow(data) > 0) {
        data <- data %>% 
          dplyr::rename(id = .data$conceptId,
                        name = .data$conceptName,
                        standard = .data$standardConcept) %>% 
          dplyr::select(.data$conceptInSet, .data$id, .data$name,
                        .data$rc, .data$drc, .data$dbc, .data$dbc,
                        .data$vocabularyId, .data$domainId, .data$standard) %>% 
          dplyr::arrange(dplyr::desc(.data$drc))
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  output$recommendedConceptSetExpressionDtSourceSecond <-
    DT::renderDT(expr = {
      data <- recommendedConceptSetExpressionSourceReactiveSecond()
      if (!is.null(data) &&
          nrow(data) > 0) {
        dataTable <- standardDataTable(data = data)
        return(dataTable)
      } else {
        return(dplyr::tibble("No recommendation"))
      }
    })
  
  
  # compare the differences between two cohort definitions using diffr
  output$logicDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchResultRecentTwoSelection()[1,]
    cohort2 <- cohortSearchResultRecentTwoSelection()[2,]
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
    file1 <- tempfile()
    writeLines(cohort1$logicDescription, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$logicDescription, con = file2)
    detailsDiffOutput <- diffr::diffr(
      file1,
      file2,
      wordWrap = TRUE,
      before = cohort1$cohortName,
      after = cohort2$cohortName
    )
    unlink(file1)
    unlink(file2)
    return(detailsDiffOutput)
  })
  
  output$jsonDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchResultRecentTwoSelection()[1,]
    cohort2 <- cohortSearchResultRecentTwoSelection()[2,]
    
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
    
    file1 <- tempfile()
    writeLines(cohort1$json, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$json, con = file2)
    jsonDiffOutput <- diffr::diffr(
      file1 = file1,
      file2 = file2,
      wordWrap = TRUE,
      before = cohort1$cohortName,
      after = cohort2$cohortName
    )
    unlink(file1)
    unlink(file2)
    return(jsonDiffOutput)
  })
  
  output$sqlDifferenceBetweenCohorts <- diffr::renderDiffr({
    cohort1 <- cohortSearchResultRecentTwoSelection()[1,]
    cohort2 <- cohortSearchResultRecentTwoSelection()[2,]
    
    if (is.null(cohort1) || is.null(cohort2)) {
      return(NULL)
    }
    
    file1 <- tempfile()
    writeLines(cohort1$sql, con = file1)
    file2 <- tempfile()
    writeLines(cohort2$sql, con = file2)
    sqlDiffOutput <- diffr::diffr(
      file1 = file1,
      file2 = file2,
      wordWrap = FALSE,
      before = cohort1$cohortName,
      after = cohort2$cohortName,
      width = "100%"
    )
    unlink(file1)
    unlink(file2)
    return(sqlDiffOutput)
  })
  
  ######## After selecting cohort using button ################
  # shiny header drop down options
  headerFilterOptionsPhenotypeDatabaseCohort <- shiny::reactive(x = {
    idx <- input$cohortSearchTableResults_rows_selected
    if (all(length(cohortsSelectedByActionButton()) > 0,
            length(idx) > 0)) {
      selectedCohortIds <-
        cohortSearchResults()[idx,]$cohortId %>% unique()
    } else {
      return(NULL)
    }
    
    data <- combinationsOfPhenotypeDatabaseCohort %>%
      dplyr::filter(.data$cohortId %in%
                      selectedCohortIds) %>%
      dplyr::left_join(
        y = database %>%
          dplyr::select(.data$databaseId,
                        .data$databaseName),
        by = "databaseId"
      ) %>%
      dplyr::left_join(y = cohort %>%
                         dplyr::select(.data$cohortId,
                                       .data$cohortName, 
                                       .data$shortName,
                                       .data$compoundName),
                       by = "cohortId")
    
    if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
      data <- data %>%
        dplyr::left_join(
          y = phenotypeDescription %>%
            dplyr::select(.data$phenotypeId,
                          .data$phenotypeName),
          by = "phenotypeId"
        )
    } else {
      data$phenotypeName <- "No phenotype name"
    }
    return(data)
  },
  label = "drop down options")
  
  optionsForDropDownDatabase <- shiny::reactive(x = {
    if (length(cohortsSelectedByActionButton()) > 0 &&
        !is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
      data <- headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$databaseName) %>%
        dplyr::distinct() %>%
        dplyr::pull()
      return(data)
    }
  })
  optionsForDropDownCohort <- shiny::reactive(x = {
    if (length(cohortsSelectedByActionButton()) > 0 &&
        !is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
      data <- headerFilterOptionsPhenotypeDatabaseCohort() %>%
        dplyr::select(.data$compoundName) %>%
        dplyr::distinct() %>%
        dplyr::pull()
      return(data)
    }
  })
  if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
    optionsForDropDownPhenotype <- shiny::reactive(x = {
      if (length(cohortsSelectedByActionButton()) > 0 &&
          !is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
        data <- headerFilterOptionsPhenotypeDatabaseCohort() %>%
          dplyr::select(.data$phenotypeName) %>%
          dplyr::distinct() %>%
          dplyr::pull()
        return(data)
      }
    })
  }
  
  
  # cohorts selected by action button
  cohortsSelectedByActionButton <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                             idx <- input$cohortSearchTableResults_rows_selected
                             if (length(idx) > 0) {
                               return(cohortSearchResults()[idx, ]$cohortId %>% unique())
                             } else {
                               return(NULL)
                             }
                           }
                         })
  
  # Pre-fetch data --------------------------------------------------------------------------
  progressBarMessagePreFetchCohortCount <- shiny::reactive(x = {
    length(unique(combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()$cohortId))
  })
  progressBarMessagePreFetchDatabaseCount <- shiny::reactive(x = {
    length(unique(combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()$databaseId))
  })
  progressBarMessagePreFetchTemplateFirst <- shiny::reactive(x = {
    paste0("Working on combination of ", 
           progressBarMessagePreFetchCohortCount(), 
           " cohorts on ", 
           progressBarMessagePreFetchDatabaseCount(), 
           " data sources.")
  })
  cohortCountsPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching Cohort Count data"
                             ),
                             value = 0,
                             {
                               if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                                 if (length(cohortsSelectedByActionButton()) != 0) {
                                   data <- getCohortCountResult(dataSource = dataSource,
                                                                cohortIds = cohortsSelectedByActionButton())
                                   return(data)
                                 } else {
                                   data <- dplyr::tibble()
                                 }
                               }
                             }
                           )
                         })
  
  incidenceRateDataPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching Incidence Rate data"
                             ),
                             value = 0,
                             {
                               if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                                 if (length(cohortsSelectedByActionButton()) != 0) {
                                   data <- getIncidenceRateResult(dataSource = dataSource,
                                                                  cohortIds = cohortsSelectedByActionButton()) %>%
                                     dplyr::mutate(incidenceRate = dplyr::case_when(.data$incidenceRate < 0 ~ 0,
                                                                                    TRUE ~ .data$incidenceRate))
                                   return(data)
                                 } else {
                                   data <- dplyr::tibble()
                                 }
                               }
                             }
                           )
                         })
  
  timeDistributionPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching Time Distribution Rate data."
                             ),
                             value = 0,
                             {
                               if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                                 if (length(cohortsSelectedByActionButton()) != 0) {
                                   data <- getTimeDistributionResult(dataSource = dataSource,
                                                                     cohortIds = cohortsSelectedByActionButton())
                                   return(data)
                                 } else {
                                   return(dplyr::tibble())
                                 }
                               }
                             }
                           )
                         })
  
  inclusionRuleTablePreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching Inclusion Rule data"
                             ),
                             value = 0,
                             {
                               if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                                 if (length(cohortsSelectedByActionButton()) != 0) {
                                   data <- getInclusionRuleStats(dataSource = dataSource,
                                                                 cohortIds = cohortsSelectedByActionButton())
                                   return(data)
                                 } else {
                                   return(dplyr::tibble())
                                 }
                               }
                             }
                           )
                         })
  
  indexEventBreakDownDataPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching Index Event Breakdown data"
                             ),
                             value = 0,
                             {
                               if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                                 if (length(cohortsSelectedByActionButton()) != 0) {
                                   data <- getIndexEventBreakdown(
                                     dataSource = dataSource,
                                     cohortIds = cohortsSelectedByActionButton(),
                                     cohortCounts = cohortCountsPreFetch()
                                   )
                                   return(data)
                                 } else {
                                   return(dplyr::tibble())
                                 }
                               }
                             }
                           )
                         })
  
  visitContextDataPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           shiny::withProgress(
                             message = paste0(
                               progressBarMessagePreFetchTemplateFirst(),
                               "\n",
                               "Pre-fetching Visit Context data"
                             ),
                             value = 0,
                             {
                               if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                                 if (length(cohortsSelectedByActionButton()) != 0) {
                                   data <- getVisitContextResults(
                                     dataSource = dataSource,
                                     cohortIds = cohortsSelectedByActionButton(),
                                     cohortCounts = cohortCountsPreFetch()
                                   )
                                   return(data)
                                 } else {
                                   return(dplyr::tibble())
                                 }
                               }
                             }
                           )
                         })
  characterizationDataPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                             if (length(cohortsSelectedByActionButton()) != 0) {
                               data <- getCovariateValueResult(dataSource = dataSource,
                                                               table = "covariateValue",
                                                               cohortIds = cohortsSelectedByActionButton())
                               return(data)
                             } else {
                               return(dplyr::tibble())
                             }
                           } else {
                             return(dplyr::tibble())
                           }
                         })
  
  
  temporalCharacterizationDataPreFetch <-
    shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                         valueExpr = {
                           if (input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                             if (length(cohortsSelectedByActionButton()) != 0) {
                               data <- getCovariateValueResult(dataSource = dataSource,
                                                               table = "temporalCovariateValue",
                                                               cohortIds = cohortsSelectedByActionButton())
                               return(data)
                             } else {
                               return(dplyr::tibble())
                             }
                           }
                         })
  
  cohortOverlapPreFetch <- shiny::eventReactive(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                                                valueExpr = {
                                                  if (input$loadSelectedCohorts[1] != 0 ||
                                                      !is.null(clickLoadSelectedCohortButton())) {
                                                    if (length(cohortsSelectedByActionButton()) > 1) {
                                                      combisOfTargetComparator <-
                                                        tidyr::crossing(targetCohortId = cohortsSelectedByActionButton(),
                                                                        comparatorCohortId = cohortsSelectedByActionButton()) %>%
                                                        dplyr::filter(!.data$targetCohortId == .data$comparatorCohortId) %>%
                                                        dplyr::distinct()
                                                      data <-
                                                        getCohortOverlapResult(
                                                          dataSource = dataSource,
                                                          targetCohortIds = combisOfTargetComparator$targetCohortId,
                                                          comparatorCohortIds = combisOfTargetComparator$comparatorCohortId
                                                        )
                                                      return(data)
                                                    } else {
                                                      return(dplyr::tibble())
                                                    }
                                                  }
                                                })
  
  
  # filter combinations to filter 'PreFetch' data.
  combinationToFilterPreFetchDataBasedOnUserChoice <- shiny::reactive(x = {
    if (!is.null(headerFilterOptionsPhenotypeDatabaseCohort())) {
      data <- headerFilterOptionsPhenotypeDatabaseCohort() %>% 
        dplyr::filter(.data$cohortId %in% cohortsSelectedByActionButton()) %>% 
        dplyr::distinct() 
      return(data)
    } else {
      return(NULL)
    }
  })
  combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId <- shiny::reactive(x = {
    if (!is.null(combinationToFilterPreFetchDataBasedOnUserChoice())) {
      return(combinationToFilterPreFetchDataBasedOnUserChoice() %>% 
               dplyr::select(.data$cohortId,
                             .data$databaseId,
                             .data$shortName) %>% 
               dplyr::distinct())
    } else {
      return(NULL)
    }
  })
  
  
  # observe event that gets trigger when select cohorts button is pressed
  shiny::observeEvent(eventExpr = {list(clickLoadSelectedCohortButton(),input$loadSelectedCohorts)},
                      handlerExpr = {
                        if(input$loadSelectedCohorts[1] != 0 || !is.null(clickLoadSelectedCohortButton())) {
                          showAllMenuItem(TRUE)
                          shiny::withProgress(
                            message = paste0(
                              progressBarMessagePreFetchTemplateFirst(),
                              "\n",
                              "Initiating...."
                            ),
                            value = 0,
                            {
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "selectedDatabases",
                                label = "Database",
                                choices = optionsForDropDownDatabase(),
                                selected = optionsForDropDownDatabase()
                              )
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "selectedCohorts",
                                label = "Cohort",
                                choices = optionsForDropDownCohort(),
                                selected = optionsForDropDownCohort()
                              )
                              
                              if (exists("phenotypeDescription") &&
                                  nrow(phenotypeDescription) > 0) {
                                shinyWidgets::updatePickerInput(
                                  session = session,
                                  inputId = "selectedPhenotypes",
                                  choicesOpt = list(style = rep_len("color: black;", 999)),
                                  choices = optionsForDropDownPhenotype(),
                                  selected = optionsForDropDownPhenotype()
                                )
                              }
                              
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "incidenceRateAgeFilter",
                                label = "Age filter",
                                choices = incidenceRateAgeFilter(),
                                selected = incidenceRateAgeFilter()
                              )
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "incidenceRateGenderFilter",
                                label = "Gender",
                                choices = incidenceRateGenderFilter()[stringr::str_detect(string = incidenceRateGenderFilter(),
                                                                                          pattern = "Male|Female")] %>% sort(),
                                selected = incidenceRateGenderFilter()
                              )
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "incidenceRateCalendarFilter",
                                label = "Calendar Year",
                                choices = incidenceRateCalendarYearFilter()  %>% sort(),
                                selected = incidenceRateCalendarYearFilter()
                              )
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "temporalCharacterizationAnalysisNameFilter",
                                choices = temporalCharacterizationAnalysisNameFilter() %>% sort(),
                                selected = temporalCharacterizationAnalysisNameFilter()
                              )
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "temporalCharacterizationDomainFilter",
                                choices = temporalCharacterizationDomainFilter() %>% sort(),
                                selected = temporalCharacterizationDomainFilter()
                              )
                              
                              shinyWidgets::updatePickerInput(
                                session = session,
                                inputId = "temporalChoices",
                                choices = temporalCharacterizationTemporalChoicesFilter() %>% sort(),
                                selected = temporalCharacterizationTemporalChoicesFilter()
                              )
                              rvCharacterizationPrettyTableGenerated(FALSE)
                            }
                            
                          )
                        }
                      })
  
  # filter pre fetch data
  progressBarMessageFilterCohortCount <- reactiveVal(NULL);
  shiny::observeEvent(eventExpr = {list(
                                       input$tabs, input$loadSelectedCohorts)
  }, handlerExpr = {
    if (isFALSE(input$selectedCohorts_open) || !is.null(input$tabs)) {
      progressBarMessageFilterCohortCount(length(input$selectedCohorts))
    }
  })
  
  progressBarMessageFilterDatabaseCount <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(
                                        input$tabs, input$loadSelectedCohorts)
  },handlerExpr = {
    if (isFALSE(input$selectedDatabases_open) || !is.null(input$tabs)) {
      progressBarMessageFilterDatabaseCount(length(input$selectedDatabases))
    }
  })
  
  progressBarMessageFilter <- shiny::reactive(x = {
    paste0("Working on combination of ", 
           progressBarMessageFilterCohortCount(), 
           " cohorts on ", 
           progressBarMessageFilterDatabaseCount(), 
           " data sources.")
  })
  
  selectedCohortIds <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$selectedCohorts_open,
    input$tabs, input$loadSelectedCohorts)
  },handlerExpr = {
    if (isFALSE(input$selectedCohorts_open) || !is.null(input$tabs)) {
      selectedCohortIds <- cohort %>% 
        dplyr::filter(.data$compoundName %in% input$selectedCohorts) %>% 
        dplyr::select(.data$cohortId) %>% 
        dplyr::pull()
      selectedCohortIds(selectedCohortIds)
    }
  })
  
  selectedDatabaseIds <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$selectedDatabases_open,
                                        input$tabs, input$loadSelectedCohorts)
  },handlerExpr = {
    if (isFALSE(input$selectedDatabases_open) || !is.null(input$tabs)) {
      selectedDatabaseIds <-  database %>% 
        dplyr::filter(.data$databaseName %in% input$selectedDatabases) %>% 
        dplyr::select(.data$databaseId) %>% 
        dplyr::pull()
      selectedDatabaseIds(selectedDatabaseIds)
    } 
  })
  
  # cohort count--------------------------------
  cohortCountsDataFiltered <- shiny::reactive({
    shiny::withProgress(
      message = paste0(progressBarMessageFilter(),
                       ". ",
                       "Getting Cohort Counts data"),
      value = 0,
      {
        filter <-
          combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% 
          dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
          dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
        data <- cohortCountsPreFetch() %>%
          dplyr::inner_join(y = filter,
                            by = c("cohortId", "databaseId")) %>% 
          dplyr::relocate(.data$databaseId, .data$shortName, .data$cohortId)
        return(data)
      }
    )
  })
  output$cohortCountsTable <- DT::renderDT(expr = {
    data <- cohortCountsDataFiltered() %>% 
      dplyr::select(-.data$shortName)
    if (input$pivotCohortCount == 'All') {
      data <- data
    } else if (input$pivotCohortCount == 'Subjects') {
      data <- data %>% 
        dplyr::select(-.data$cohortEntries) %>% 
        tidyr::pivot_wider(id_cols = c(.data$cohortId),
                           values_from = .data$cohortSubjects,
                           names_from = .data$databaseId)
    } else if (input$pivotCohortCount == 'Entries') {
      data <- data %>% 
        dplyr::select(-.data$cohortEntries) %>% 
        tidyr::pivot_wider(id_cols = c(.data$cohortId),
                           values_from = .data$cohortSubjects,
                           names_from = .data$databaseId)
    }
    dataTable <- standardDataTable(data = data, selected = NULL)
    return(dataTable)
  }, server = TRUE)
  
  # Incidence rate --------------------------------------------------------------------------------
  incidenceRateAgeFilter <- shiny::reactive(x = {
    if (nrow(incidenceRateDataPreFetch()) > 0) {
      ageFilter <- incidenceRateDataPreFetch() %>%
        dplyr::select(.data$ageGroup) %>%
        dplyr::distinct() %>%
        dplyr::arrange(as.integer(sub(
          pattern = '-.+$', '', x = .data$ageGroup
        ))) %>% 
        dplyr::pull()
    } else {
      ageFilter <- NULL
    }
    return(ageFilter)
  })
  incidenceRateGenderFilter <- shiny::reactive(x = {
    if (nrow(incidenceRateDataPreFetch()) > 0) {
      genderFilter <-
        incidenceRateDataPreFetch()$gender %>% unique()
    } else {
      genderFilter <- NULL
    }
    return(genderFilter)
  })
  incidenceRateCalendarYearFilter <- shiny::reactive(x = {
    if (nrow(incidenceRateDataPreFetch()) > 0) {
      calendarYear <-
        incidenceRateDataPreFetch()$calendarYear %>% unique()
    } else {
      calendarYear <- NULL
    }
    return(calendarYear)
  })
  
  selectedIncidenceRateAgeFilter <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts, input$incidenceRateAgeFilter_open)
  },handlerExpr = {
    if (isFALSE(input$incidenceRateAgeFilter_open) || !is.null(input$tabs)) {
      selectedIncidenceRateAgeFilter(input$incidenceRateAgeFilter)
    }
  })
  
  selectedIncidenceRateCalendarFilter <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts, input$incidenceRateCalendarFilter_open)
  },handlerExpr = {
    if (isFALSE(input$incidenceRateCalendarFilter_open) || !is.null(input$tabs)) {
      selectedIncidenceRateCalendarFilter(input$incidenceRateCalendarFilter)
    }
  })
  
  selectedIncidenceRateGenderFilter <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts, input$incidenceRateGenderFilter_open)
  },handlerExpr = {
    if (isFALSE(input$incidenceRateGenderFilter_open) || !is.null(input$tabs)) {
      selectedIncidenceRateGenderFilter(input$incidenceRateGenderFilter)
    }
  })
  
  incidenceRateDataFiltered <- reactive({
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        "\n",
        "Getting Incidence Rate data"
      ),
      value = 0,
      {
        stratifyByAge <- "Age" %in% input$irStratification
        stratifyByGender <- "Gender" %in% input$irStratification
        stratifyByCalendarYear <-
          "Calendar Year" %in% input$irStratification
        filter <-
          combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
          dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
          dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
        data <- incidenceRateDataPreFetch() %>%
          dplyr::inner_join(y = filter,
                            by = c("cohortId", "databaseId"))
        if (stratifyByAge) {
          data <- data %>%
            dplyr::filter(.data$ageGroup %in% selectedIncidenceRateAgeFilter())
        } 
        
        if (stratifyByCalendarYear) {
          data <- data %>%
            dplyr::filter(.data$calendarYear %in% selectedIncidenceRateCalendarFilter())
        } 
        
        if (stratifyByGender) {
          data <- data %>%
            dplyr::filter(.data$gender %in% selectedIncidenceRateGenderFilter())
        } 
        
        return(data)
      }
    )
  })
  
  output$incidenceRatePlot <- ggiraph::renderggiraph(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                                         "\n",
                                         "Generating Incidence Rate plots"), 
                        value = 0, {
                          data <- incidenceRateDataFiltered()
                          if (nrow(data) > 0) {
                            stratifyByAge <- "Age" %in% input$irStratification
                            stratifyByGender <- "Gender" %in% input$irStratification
                            stratifyByCalendarYear <-
                              "Calendar Year" %in% input$irStratification
                            plot <- plotIncidenceRate(
                              data = data,
                              shortNameRef = cohort,
                              stratifyByAgeGroup = stratifyByAge,
                              stratifyByGender = stratifyByGender,
                              stratifyByCalendarYear = stratifyByCalendarYear,
                              yscaleFixed = input$irYscaleFixed,
                              minPersonYears = input$minPersonYear
                            )
                            return(plot)
                          } else {
                            NULL
                          }
                        })
  })
  
  output$incidenceRateTable <- DT::renderDT(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                                         "\n",
                                         "Creating Incidence Rate table"), 
                        value = 0, {
                          data <- incidenceRateDataFiltered()
                          if (nrow(data) > 0) {
                            # isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
                            # data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
                            colnames(data) <-
                              colnames(data) %>% stringr::str_replace_all(string = .,
                                                                          pattern = "Value",
                                                                          replacement = "")
                          }
                          table <- standardDataTable(data, selected = NULL)
                          return(table)
                        })
  }, server = TRUE)
  
  # Time distribution -----------------------------------------------------------------------------
  timeDistributionFiltered <- reactive({
    data <- timeDistributionPreFetch() 
    filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
      dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
      dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
    if (nrow(data) > 0) {
      data <- timeDistributionPreFetch() %>%
        dplyr::inner_join(y = filter,
                          by = c("cohortId", "databaseId")) %>% 
        dplyr::relocate(.data$databaseId, .data$shortName, .data$cohortId, .data$timeMetric)
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$timeDistributionPlot <- ggiraph::renderggiraph(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                                         "\n",
                                         "Generating Time Distribution plot(S)"), 
                        value = 0, {
                          data <- timeDistributionFiltered()
                          validate(need(nrow(data) > 0, paste0("No data for this combination")))
                          plot <- plotTimeDistribution(data = data,
                                                       shortNameRef = cohort)
                          return(plot)
                        })
  })
  output$timeDistributionTable <- DT::renderDT(expr = {
    shiny::withProgress(message = paste0(progressBarMessageFilter(),
                                         "\n",
                                         "Generating Time Distribution Table"), 
                        value = 0, {
                          data <- timeDistributionFiltered()
                          if (nrow(data) > 0) {
                            # isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
                            data <- data %>% 
                              dplyr::select(-.data$shortName)
                            # data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
                            colnames(data) <-
                              colnames(data) %>% stringr::str_replace_all(string = .,
                                                                          pattern = "Value",
                                                                          replacement = "")
                          }
                          table <- standardDataTable(data, selected = NULL)
                          return(table)
                        })
  }, server = TRUE)
  
  # Inclusion rules table -----------------------------------------------------------------------
  inclusionRuleFiltered <- reactive({
    if (nrow(inclusionRuleTablePreFetch()) > 0) {
      filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
        dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
        dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
      data <- inclusionRuleTablePreFetch() %>%
        dplyr::relocate(.data$databaseId, .data$cohortId) %>% 
        dplyr::inner_join(y = filter %>% 
                            dplyr::select(.data$cohortId, .data$databaseId),
                          by = c("cohortId", "databaseId")) %>% 
        dplyr::mutate(meetPercent = .data$meetSubjects/.data$totalSubjects,
                      gainPercent = .data$gainSubjects/.data$totalSubjects,
                      remainPercent = .data$remainSubjects/.data$totalSubjects) %>% 
        dplyr::arrange(dplyr::desc(.data$remainPercent))
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$inclusionRuleTable <- DT::renderDT(expr = {
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        ". ",
        "Generating Inclusion Rules table"
      ),
      value = 0,
      {
        if (nrow(inclusionRuleFiltered()) > 0) {
          data <- inclusionRuleFiltered()
          table <- standardDataTable(data, selected = NULL)
          return(table)
        } else {
          return(dplyr::tibble("No Inclusion rules data for the selected combination."))
        }
      }
    )
  }, server = TRUE)
  
  
  # Index event breakdown ----------------------------------------------------------------
  indexEventBreakDownDataFiltered <- reactive({
    if (nrow(indexEventBreakDownDataPreFetch()) > 0) {
      filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
        dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
        dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
      data <- indexEventBreakDownDataPreFetch() %>%
        dplyr::inner_join(y = filter %>% 
                            dplyr::select(.data$cohortId, 
                                          .data$databaseId),
                          by = c("cohortId", "databaseId")) %>% 
        dplyr::relocate(.data$databaseId, .data$cohortId)
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  
  output$indexEventBreakDownTable <- DT::renderDT(expr = {
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        "\n",
        "Generating Index Event Breakdown table"
      ),
      value = 0,
      {
        data <- indexEventBreakDownDataFiltered()
        if ('conceptCount' %in% colnames(data)) {
          data <- data %>%
            dplyr::mutate(percentEntries =  round(
              x = (.data$conceptCount / .data$cohortEntries) * 100,
              digits = 2
            )) %>%
            dplyr::arrange(dplyr::desc(.data$percentEntries))
        }
        if ('subjectCount' %in% colnames(data)) {
          data <- data %>%
            dplyr::mutate(percentSubjects =  round(
              x = (.data$subjectCount / .data$cohortSubjects) * 100,
              digits = 2
            )) %>%
            dplyr::arrange(dplyr::desc(.data$percentSubjects))
        } else {
          data <- data %>%
            dplyr::mutate(percentSubjects = NA)
        }
        
        if (input$pivotIndexEventBreakDown == 'All') {
          data <- data %>%
            dplyr::mutate(percentEntries = percentEntries / 100,
                          percentSubjects = percentSubjects / 100)
        } else if (input$pivotIndexEventBreakDown == 'Concept count') {
          data <-
            pivotIndexBreakDownData(data = data, variable = 'conceptCount')
        } else if (input$pivotIndexEventBreakDown == 'Subject count') {
          data <-
            pivotIndexBreakDownData(data = data, variable = 'subjectCount')
        } else if (input$pivotIndexEventBreakDown == 'Percent entries') {
          data <-
            pivotIndexBreakDownData(data = data, variable = 'percentEntries')
        } else if (input$pivotIndexEventBreakDown == 'Percent persons') {
          data <-
            pivotIndexBreakDownData(data = data, variable = 'percentSubjects')
        }
        dataTable <- standardDataTable(data, selected = NULL)
        return(dataTable)
      }
    )
  }, server = TRUE)
  
  
  # Visit Context --------------------------------------------------------------------------------------------
  visitContextDataFiltered <- reactive({
    if (nrow(visitContextDataPreFetch()) > 0) {
      shiny::withProgress(
        message = paste0(
          progressBarMessageFilter(),
          ". ",
          "Getting Visit Context data"
        ),
        value = 0,
        {
          filter <-
            combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
            dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
            dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
          data <- visitContextDataPreFetch() %>%
            dplyr::inner_join(y = filter,
                              by = c("cohortId", "databaseId"))
          
          if (input$pivotVisitContext == 'All') {
            data <- data
          } else if (input$pivotVisitContext == 'Percent') {
            data <- data %>% 
              dplyr::select(-dplyr::contains("subjects"))
          } else if (input$pivotVisitContext == 'Subjects') {
            data <- data %>% 
              dplyr::select(-dplyr::contains("percent"))
          }
        }
      )
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  output$visitContextTable <- DT::renderDT(expr = {
    shiny::withProgress(
      message = paste0(
        progressBarMessageFilter(),
        "\n",
        "Generating Visit Context table"
      ),
      value = 0,
      {
        data <- visitContextDataFiltered() %>% 
          dplyr::relocate(.data$databaseId,
                          .data$cohortId) %>% 
          dplyr::select(-.data$shortName)
        # isPhenotypeLibraryMode <- exists("phenotypeDescription") && nrow(phenotypeDescription) > 0
        # data <- addMetaDataInformationToResults(data = data, isPhenotypeLibraryMode = isPhenotypeLibraryMode)
        table <- standardDataTable(data, selected = NULL)
      })
  }, server = TRUE) 
  
  # Characterization -----------------------------------------------------------------
  characterizationDataFilterOptions <-
    shiny::reactive({
      shiny::withProgress(message = 'Loading...', value = 0, {
        data <- characterizationDataPreFetch()
        if (nrow(data) <= 0) {
          return(dplyr::tibble())
        }
        data <- data %>%
          dplyr::select(.data$covariateId) %>%
          dplyr::distinct() %>%
          dplyr::left_join(covariateRef, by = "covariateId") %>%
          dplyr::left_join(
            analysisRef %>%
              dplyr::select(.data$analysisId,
                            .data$analysisName,
                            .data$domainId,
                            .data$isBinary),
            by = "analysisId"
          )
        data <- data %>% 
          tidyr::replace_na(list(domainId = 'Other', analysisName = 'Other'))
      })
      return(data)
    })
  
  characterizationDataFiltered <- shiny::reactive(x = {
    dataFilterOptions <-
      characterizationDataFilterOptions()
    if (nrow(dataFilterOptions) <= 0) {
      return(dplyr::tibble())
    }
    filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
      dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
      dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
    data <- characterizationDataPreFetch() %>%
      dplyr::inner_join(y = filter,
                        by = c("cohortId", "databaseId")) %>%
      dplyr::inner_join(y = dataFilterOptions,
                        by = c("covariateId" = "covariateId")) %>%
      dplyr::relocate(.data$databaseId,
                      .data$analysisId,
                      .data$analysisName,
                      .data$domainId,
                      .data$covariateId,
                      .data$covariateName,
                      .data$conceptId,
                      .data$conceptName,
                      .data$isBinary) %>% 
      dplyr::arrange(dplyr::desc(.data$mean)) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(conceptName = dplyr::case_when(.data$conceptId == 0 ~ .data$covariateName, TRUE ~ .data$conceptName))
    return(data)
  })
  
  output$characterizationTableRaw <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering characterization table', value = 0, {
        if (nrow(characterizationDataFiltered()) <= 0) {
          return(dplyr::tibble())
        }
        data <- characterizationDataFiltered() %>%
          dplyr::relocate(.data$databaseId,
                          .data$shortName,
                          .data$cohortId) %>% 
          dplyr::select(-.data$shortName)
        if (nrow(data) > 0) {
          table <- standardDataTable(data = data, selected = NULL)
          return(table)
        } else {
          dplyr::tibble("No characterization data")
        }
      })
    }, server = TRUE) 
  
  characterizationTablePretty <- shiny::reactive(x = {
    data <- characterizationDataFiltered()
    if (nrow(data) > 0) {
      analysisIds <- prettyAnalysisIds
      table <- data %>%
        prepareTable1() %>%
        dplyr::rename(percent = .data$value)
      characteristics <- table %>%
        dplyr::select(.data$characteristic,
                      .data$position,
                      .data$header,
                      .data$sortOrder) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$characteristic, .data$position, .data$header) %>%
        dplyr::summarise(sortOrder = max(.data$sortOrder)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$position, desc(.data$header)) %>%
        dplyr::mutate(sortOrder = dplyr::row_number()) %>%
        dplyr::distinct()
      
      characteristics <- dplyr::bind_rows(
        tidyr::crossing(
          characteristics %>%
            dplyr::filter(.data$header == 1),
          dplyr::tibble(cohortId = unique(data$cohortId)),
          dplyr::tibble(databaseId = unique(data$databaseId))
        ),
        characteristics %>%
          dplyr::filter(.data$header == 0) %>%
          tidyr::crossing(dplyr::tibble(databaseId = unique(data$databaseId)) %>%
                            tidyr::crossing(
                              dplyr::tibble(cohortId = unique(data$cohortId))
                            )
          ))
      data <- characteristics %>%
        dplyr::left_join(
          table %>%
            dplyr::select(-.data$sortOrder),
          by = c(
            "databaseId",
            "cohortId",
            "characteristic",
            "position",
            "header"
          )
        )  %>%
        dplyr::arrange(.data$databaseId, .data$cohortId, .data$sortOrder) %>%
        dplyr::select(-.data$position, -.data$header) %>%
        dplyr::relocate(.data$sortOrder, .after = dplyr::last_col())
      
      rvCharacterizationPrettyTableGenerated(TRUE)
      
    } else {
      data <- dplyr::tibble()
    }
    return(data)
  })
  
  output$characterizationTablePrettyCohortCountText <-
    shiny::renderUI(expr = {
      if (!is.null(input$characterizationTablePrettyDtDropDownCohort)) {
        cohortIdSelectedForPrettyTable <- cohort %>%
          dplyr::filter(cohortName == input$characterizationTablePrettyDtDropDownCohort) %>%
          dplyr::pull(.data$cohortId)
        data <- cohortCountsPreFetch()
        data <- data %>%
          dplyr::filter(.data$databaseId %in% input$characterizationTablePrettyDtDropDownDatabase) %>%
          dplyr::filter(.data$cohortId %in% cohortIdSelectedForPrettyTable)
        
        output <- paste0(
          "The number of subjects in the ",
          input$characterizationTablePrettyDtDropDownCohort,
          " cohort for ",
          input$characterizationTablePrettyDtDropDownDatabase,
          " is ",
          scales::comma(data$cohortSubjects),
          " while the number of events is ",
          scales::comma(data$cohortEntries)
        )
        
        return(tags$table(
          tags$tr(
            tags$td(output)),
          tags$tr(
            tags$td(tags$br())
          )))
      }
    })
  
  output$characterizationTablePrettyDt <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering Characterization table 1', value = 0, {
        if (rvCharacterizationPrettyTableGenerated() &&
            !is.null(input$characterizationTablePrettyDtDropDownDatabase) &&
            !is.null(input$characterizationTablePrettyDtDropDownCohort)) {
          data <- characterizationTablePretty() 
          cohortIdSelectedForPrettyTable <- cohort %>% 
            dplyr::filter(cohortName == input$characterizationTablePrettyDtDropDownCohort) %>% 
            dplyr::pull(.data$cohortId)
          data <- data %>%
            dplyr::filter(.data$databaseId %in% input$characterizationTablePrettyDtDropDownDatabase) %>% 
            dplyr::filter(.data$cohortId %in% cohortIdSelectedForPrettyTable) %>% 
            dplyr::select(.data$characteristic, .data$percent)
          data <- data %>% 
            tidyr::replace_na(list(percent = 0))
          if (nrow(data) > 0) {
            table <- standardDataTable(data = data, pageLength = -1, selected = NULL)
            return(table)
          }
        }
      })
    }, server = TRUE)
  
  
  characterizationPrettyDatabaseFilter <- shiny::reactive(x = {
    if (nrow(characterizationTablePretty()) > 0) {
      characterizationPrettyDatabaseFilter <-
        characterizationTablePretty()$databaseId %>% 
        unique() %>% 
        sort()
      return(characterizationPrettyDatabaseFilter)
    } else {
      return(NULL)
    }
  })
  
  characterizationPrettyCohortFilter <- shiny::reactive(x = {
    if (nrow(characterizationTablePretty()) > 0) {
      characterizationPrettyCohortFilter <-
        cohort %>% dplyr::inner_join(characterizationTablePretty() %>% 
                                       dplyr::select(.data$cohortId),
                                     by = "cohortId") %>% 
        dplyr::pull(.data$cohortName) %>% 
        unique() %>% 
        sort()
      return(characterizationPrettyCohortFilter)
    } else {
      return(NULL)
    }
  })
  
  characterizationPrettyDomainChoicesFilter <- shiny::reactive(x = {
    if (nrow(characterizationDataFiltered()) > 0) {
      characterizationPrettyDomainChoiceFilter <-
        characterizationDataFiltered() %>% 
        dplyr::select(.data$domainId) %>%
        dplyr::pull(.data$domainId) %>% 
        unique()
      return(characterizationPrettyDomainChoiceFilter)
    } else {
      return(NULL)
    }
  })
  
  shiny::observeEvent(eventExpr = {
    (!is.null(input$tabs) && input$tabs == "cohortCharacterization")
  },
  handlerExpr = {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "characterizationTablePrettyDtDropDownDatabase",
      choices = characterizationPrettyDatabaseFilter(),
      selected = characterizationPrettyDatabaseFilter()[1]
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "characterizationTablePrettyDtDropDownCohort",
      choices = characterizationPrettyCohortFilter(),
      selected = characterizationPrettyCohortFilter()[1]
    )
  })
  
  shiny::observeEvent(eventExpr = {
    (!is.null(input$tabs) && input$tabs == "compareCharacterization")
  },
  handlerExpr = {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "compareCharacterizationTableDropDownDatabase",
      choices = characterizationPrettyDatabaseFilter(),
      selected = characterizationPrettyDatabaseFilter()[1]
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "compareCharacterizationTableDropCohort1",
      choices = characterizationPrettyCohortFilter(),
      selected = characterizationPrettyCohortFilter()[1]
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "compareCharacterizationTableDropCohort2",
      choices = characterizationPrettyCohortFilter(),
      selected = characterizationPrettyCohortFilter()[1]
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "compareCharacterizationDomainChoices",
      choices = characterizationPrettyDomainChoicesFilter(),
      selected = characterizationPrettyDomainChoicesFilter()
    )
  })
  
  compareCharacterizationSelectedDomainChoices <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts,input$compareCharacterizationDomainChoices_open)
  },handlerExpr = {
    if (isFALSE(input$compareCharacterizationDomainChoices_open) || !is.null(input$tabs)) {
      compareCharacterizationSelectedDomainChoices(input$compareCharacterizationDomainChoices)
    }
  })
  
  compareCharacterizationData <- shiny::reactive(x = {
    data <- characterizationDataFiltered() %>% 
      dplyr::filter(.data$databaseId %in% 
                      input$compareCharacterizationTableDropDownDatabase)
    
    data <- data %>% 
      dplyr::filter(.data$domainId %in% compareCharacterizationSelectedDomainChoices())
    
    cohortId1 <- cohort %>% 
      dplyr::filter(.data$cohortName %in% 
                      input$compareCharacterizationTableDropCohort1) %>% 
      dplyr::pull(.data$cohortId)
    
    cohortId2 <- cohort %>% 
      dplyr::filter(.data$cohortName %in% 
                      input$compareCharacterizationTableDropCohort2) %>% 
      dplyr::pull(.data$cohortId)
    
    if (cohortId1 == cohortId2) {
      return(dplyr::tibble("Please select different cohorts."))
    }
    data1 <- data %>% 
      dplyr::filter(.data$cohortId == cohortId1)
    
    data2 <- data %>% 
      dplyr::filter(.data$cohortId == cohortId2)
    
    output <- compareCohortCharacteristics(characteristics1 = data1, 
                                           characteristics2 = data2) %>% 
      dplyr::select(.data$databaseId, .data$cohortId1, .data$cohortId2,
                    .data$isBinary1, .data$analysisName1, 
                    .data$domainId1, .data$conceptId, .data$conceptName1,
                    .data$mean1, .data$mean2, .data$stdDiff
      ) %>% 
      dplyr::rename("isBinary" = "isBinary1",
                    "analysisName" = "analysisName1",
                    "domainId" = "domainId1",
                    "conceptName" = "conceptName1")
    return(output)
  })
  
  output$compareCharacterizationTargetAndComparatorCohort <- shiny::renderUI({
    if (!is.null(input$compareCharacterizationTableDropCohort1)) {
      cohortCountData <- cohortCountsPreFetch()
      cohortIdSelectedForPrettyTableComparator <- cohort %>%
        dplyr::filter(cohortName == input$compareCharacterizationTableDropCohort1) %>%
        dplyr::pull(.data$cohortId)
      
      targetCohortData <- cohortCountData %>%
        dplyr::filter(.data$databaseId %in% input$characterizationTablePrettyDtDropDownDatabase) %>%
        dplyr::filter(.data$cohortId %in% cohortIdSelectedForPrettyTableComparator)
      
      outputComarator <- paste0(
        "Target cohort : The number of subjects in the ",
        input$compareCharacterizationTableDropCohort1,
        " cohort for ",
        input$characterizationTablePrettyDtDropDownDatabase,
        " is ",
        scales::comma(targetCohortData$cohortSubjects),
        " while the number of events is ",
        scales::comma(targetCohortData$cohortEntries)
      )
      
      cohortIdSelectedForPrettyTabletarget <- cohort %>%
        dplyr::filter(cohortName == input$compareCharacterizationTableDropCohort2) %>%
        dplyr::pull(.data$cohortId)
      comparatorCohortData <- cohortCountData %>%
        dplyr::filter(.data$databaseId %in% input$characterizationTablePrettyDtDropDownDatabase) %>%
        dplyr::filter(.data$cohortId %in% cohortIdSelectedForPrettyTabletarget)
      
      outputTarget <- paste0(
        "Comaparator cohort: The number of subjects in the ",
        input$compareCharacterizationTableDropCohort2,
        " cohort for ",
        input$characterizationTablePrettyDtDropDownDatabase,
        " is ",
        scales::comma(comparatorCohortData$cohortSubjects),
        " while the number of events is ",
        scales::comma(comparatorCohortData$cohortEntries)
      )
      
      return(tags$table(
        tags$tr(
          tags$td(
            outputComarator
          )
        ),
        tags$tr(
          tags$td(
            outputTarget
          )
        )
      ))
    }
  })
  
  output$compareCharacterizationTableDt <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering Compare Characterization table', value = 0, {
        data <- compareCharacterizationData() 
        if ('databaseId' %in% colnames(data)) {
          data <- data %>% 
            dplyr::select(-.data$databaseId)
        }
        if (nrow(data) > 0) {
          table <- standardDataTable(data = data, pageLength = 100, selected = NULL)
          return(table)
        }
      })
    }, server = TRUE)
  
  output$compareCharacterizationTablePlot <-
    ggiraph::renderggiraph(expr = {
      validate(
        need(
          expr = !input$compareCharacterizationTableDropCohort1 ==
            input$compareCharacterizationTableDropCohort2,
          message = "No plot rendered because the two chosen cohorts are the same."
        )
      )
      shiny::withProgress(message = 'Rendering Compare Cohort Characterization plot', {
        data <- compareCharacterizationData()
        plot <- plotCohortComparison(balance = data,
                                     maxMean = 1,
                                     minMean = 0.01,
                                     domain = "all")
      })
      return(plot)
    })
  
  # Temporal characterization -----------------------------------------------------------------
  temporalCharacterizationDataFilterOptions <-
    shiny::reactive({
      data <- temporalCharacterizationDataPreFetch() %>%
        dplyr::select(.data$timeId,
                      .data$covariateId) %>%
        dplyr::distinct() %>%
        dplyr::left_join(temporalTimeRef, by = "timeId") %>%
        dplyr::left_join(temporalCovariateRef, by = "covariateId") %>%
        dplyr::left_join(
          temporalAnalysisRef %>%
            dplyr::select(.data$analysisId,
                          .data$analysisName,
                          .data$domainId,
                          .data$isBinary),
          by = "analysisId"
        )
      return(data)
    })
  
  temporalCharacterizationAnalysisNameFilter <- shiny::reactive(x = {
    if (nrow(temporalCharacterizationDataFilterOptions()) > 0) {
      temporalCharacterizationAnalysisNameFilter <-
        temporalCharacterizationDataFilterOptions()$analysisName %>% unique()
      return(temporalCharacterizationAnalysisNameFilter)
    } else {
      return(NULL)
    }
  })
  
  temporalCharacterizationDomainFilter <- shiny::reactive(x = {
    if (nrow(temporalCharacterizationDataFilterOptions()) > 0) {
      temporalCharacterizationDomainFilter <-
        temporalCharacterizationDataFilterOptions()$domainId %>% unique()
      return(temporalCharacterizationDomainFilter)
    } else {
      return(NULL)
    }
  })
  
  temporalCharacterizationTemporalChoicesFilter <- shiny::reactive(x = {
    if (nrow(temporalCharacterizationDataFilterOptions()) > 0) {
      temporalCharacterizationTemporalChoicesFilter <-
        temporalCharacterizationDataFilterOptions()$temporalChoices  %>% unique()
      return(temporalCharacterizationTemporalChoicesFilter)
    } else {
      return(NULL)
    }
  })
  
  selectedTemporalChoices <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts,input$temporalChoices_open)
  },handlerExpr = {
    if (isFALSE(input$temporalChoices_open) || !is.null(input$tabs)) {
      selectedTemporalChoices(input$temporalChoices)
    }
  })
  
  selectedTemporalCharacterizationAnalysisNameFilter <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts,input$temporalCharacterizationAnalysisNameFilter_open)
  },handlerExpr = {
    if (isFALSE(input$temporalCharacterizationAnalysisNameFilter_open) || !is.null(input$tabs)) {
      selectedTemporalCharacterizationAnalysisNameFilter(input$temporalCharacterizationAnalysisNameFilter)
    }
  })
  
  selectedTemporalCharacterizationDomainFilter <- reactiveVal(NULL)
  shiny::observeEvent(eventExpr = {list(input$tabs, input$loadSelectedCohorts,input$temporalCharacterizationDomainFilter_open)
  },handlerExpr = {
    if (isFALSE(input$temporalCharacterizationDomainFilter_open) || !is.null(input$tabs)) {
      selectedTemporalCharacterizationDomainFilter(input$temporalCharacterizationDomainFilter)
    }
  })
  
  temporalCharacterizationDataFiltered <- shiny::reactive(x = {
    dataFilterOptions <-
      temporalCharacterizationDataFilterOptions() %>%
      dplyr::filter(
        temporalChoices %in% selectedTemporalChoices(),
        analysisName %in% selectedTemporalCharacterizationAnalysisNameFilter(),
        domainId %in% selectedTemporalCharacterizationDomainFilter()
      )
    filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId()  %>% 
      dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
      dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
    data <- temporalCharacterizationDataPreFetch() %>%
      dplyr::inner_join(y = filter,
                        by = c("cohortId", "databaseId")) %>%
      dplyr::inner_join(y = dataFilterOptions,
                        by = c("timeId" = "timeId",
                               "covariateId" = "covariateId")) %>%
      dplyr::relocate(.data$databaseId,
                      .data$shortName,
                      .data$cohortId,
                      .data$temporalChoices,
                      .data$startDay,
                      .data$endDay,
                      .data$analysisId,
                      .data$analysisName,
                      .data$domainId,
                      .data$covariateId,
                      .data$covariateName,
                      .data$conceptId,
                      .data$conceptName,
                      .data$isBinary) %>% 
      dplyr::arrange(dplyr::desc(.data$mean)) %>% 
      dplyr::distinct()
    return(data)
  })
  
  
  output$temporalCharacterizationTableRaw <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering Temporal Characterization table', value = 0, {
        data <- temporalCharacterizationDataFiltered() %>% 
          dplyr::select(-.data$shortName)
        table <- standardDataTable(data = data, selected = NULL)
        return(table)
      })
    }, server = TRUE)
  
  temporalCharacterizationTable <- shiny::reactive(x = {
    data <- temporalCharacterizationDataFiltered() %>% 
      dplyr::select(-.data$shortName)
    colnamesData <- colnames(data)
    colnamesData <- colnamesData[stringr::str_detect(string = colnamesData,
                                                     pattern = "mean|sd|percent|temporalChoices|Day", 
                                                     negate = TRUE)]
    data <- data %>% 
      dplyr::group_by(dplyr::across(.cols = colnamesData)) %>% 
      dplyr::arrange(.data$startDay, .data$endDay, by_group = TRUE) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-.data$sd, -.data$startDay, 
                    -.data$endDay, -.data$analysisId,
                    -.data$covariateId, -.data$covariateName, 
                    -.data$timeId) %>% 
      dplyr::rename('percent' = .data$mean) %>% 
      dplyr::distinct()
    return(data)
  })
  
  output$temporalCharacterizationTable <-
    DT::renderDT(expr = {
      shiny::withProgress(message = 'Rendering Temporal Characterization table', value = 0, {
        data <- temporalCharacterizationTable()
        colnamesData <- colnames(data)
        data <- tidyr::pivot_wider(data = data %>% dplyr::distinct(),
                                   id_cols = colnamesData,
                                   names_from = .data$temporalChoices,
                                   values_from = .data$percent
        )
        table <- standardDataTable(data = data, selected = NULL)
        return(table)
      })
    }, server = TRUE)
  
  output$temporalCharacterizationPlot <-
    ggiraph::renderggiraph(expr = {
      shiny::withProgress(message = 'Rendering temporal characterization plot.', value = 0, {
        data <- temporalCharacterizationDataFiltered() %>%
          dplyr::select(
            .data$databaseId,
            .data$shortName,
            .data$cohortId,
            .data$temporalChoices,
            .data$covariateId,
            .data$analysisId,
            .data$conceptId,
            .data$domainId,
            .data$conceptName,
            .data$isBinary,
            .data$mean,
            .data$sd
          )
        data <-
          compareTemporalCohortCharacteristics(characteristics1 = data,
                                               characteristics2 = data)
        plot <- plotTemporalCohortComparison(
          balance = data,
          maxMean = 1,
          minMean = 0.01,
          temporalChoice = "all",
          domain = "all"
        )
        return(plot)
      })
    })
  
  #Cohort Overlap ------------------------
  cohortOverlaDataFiltered <- shiny::reactive(x = {
    if (nrow(cohortOverlapPreFetch()) > 0) {
      filter <- combinationToFilterPreFetchDataBasedOnUserChoiceCohortIdDatabaseId() %>% 
        dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>% 
        dplyr::filter(.data$databaseId %in% selectedDatabaseIds())
      data <- cohortOverlapPreFetch() %>%
        dplyr::inner_join(y = filter,
                          by = c("databaseId" = "databaseId",
                                 "targetCohortId" = "cohortId")) %>%
        dplyr::inner_join(y = filter,
                          by = c("databaseId" = "databaseId",
                                 "comparatorCohortId" = "cohortId"))
      return(data)
    } else {
      return(NULL)
    }
  })
  
  output$cohortOverlapPlot <- ggiraph::renderggiraph(expr = {
    data <- cohortOverlaDataFiltered()
    validate(need(nrow(data) > 0, paste0("No data for this combination")))
    if (!is.null(cohortOverlaDataFiltered())) {
      shiny::withProgress(
        message = paste0(
          progressBarMessageFilter(),
          "\n",
          "Generating Cohort Overlap plot(S)"
        ),
        value = 0,
        {
          plot <- plotCohortOverlap(
            data = data,
            shortNameRef = cohort,
            yAxis = input$overlapPlotType
          )
          return(plot)
        }
      )
    } else {
      return(NULL)
    }
  })
  
  output$cohortOverlapData <-
    DT::renderDT(expr = {
      shiny::withProgress(message = paste0(progressBarMessageFilter(),
                                           ". ",
                                           "Generating Cohort Overlap table"), 
                          value = 0, {
                            data <- cohortOverlaDataFiltered() %>% 
                              dplyr::select(-.data$shortName)
                            # data <- addMetaDataInformationToResults(data = data)
                            table <- standardDataTable(data = data)
                            return(table)
                          })}, server = TRUE)
  
  output$databaseInformationTable <- DT::renderDT(expr = {
    table <- database[, c("databaseId", "databaseName", "description")]
    table <- standardDataTable(table)
    return(table)
  }, server = TRUE)
  
  shiny::observeEvent(input$cohortCountsInfo, {
    showInfoBox(title = "Cohort Counts", htmlFileName = "html/cohortCounts.html")
  })
  
  shiny::observeEvent(input$incidenceRateInfo, {
    showInfoBox(title = "Incidence Rate", htmlFileName = "html/incidenceRate.html")
  })
  
  shiny::observeEvent(input$timeDistributionInfo, {
    showInfoBox(title = "Time Distributions", htmlFileName = "html/timeDistribution.html")
  })
  
  shiny::observeEvent(input$conceptSetDiagnosticsInfo, {
    showInfoBox(title = "Concept Set Diagnostics",
                htmlFileName = "html/conceptSetDiagnostics.html")
  })
  
  shiny::observeEvent(input$inclusionRuleStatsInfo, {
    showInfoBox(title = "Inclusion Rule Statistics",
                htmlFileName = "html/inclusionRuleStats.html")
  })
  
  shiny::observeEvent(input$indexEventBreakdownInfo, {
    showInfoBox(title = "Index Event Breakdown", htmlFileName = "html/indexEventBreakdown.html")
  })
  
  shiny::observeEvent(input$visitContextInfo, {
    showInfoBox(title = "Visit Context",
                htmlFileName = "html/visitContext.html")
  })
  
  shiny::observeEvent(input$cohortCharacterizationInfo, {
    showInfoBox(title = "Cohort Characterization",
                htmlFileName = "html/cohortCharacterization.html")
  })
  
  shiny::observeEvent(input$compareCharacterizationInfo, {
    showInfoBox(title = "Compare Cohort Char.",
                htmlFileName = "html/compareCharacterization.html")
  })
  
  shiny::observeEvent(input$temporalCharacterizationInfo, {
    showInfoBox(title = "Temporal Characterization",
                htmlFileName = "html/temporalCharacterization.html")
  })
  
  shiny::observeEvent(input$cohortOverlapInfo, {
    showInfoBox(title = "Cohort Overlap", htmlFileName = "html/cohortOverlap.html")
  })
  
  cohortReference <- function(dashboardId) {
    shinydashboard::box(
      title = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      shiny::uiOutput(outputId = dashboardId)
    )
  }
  
  selectedCohorts <- shiny::reactive(x = {
    cohorts <- cohort %>%
      dplyr::filter(.data$cohortId %in% selectedCohortIds()) %>%
      dplyr::arrange(.data$cohortId) %>%
      dplyr::select(.data$compoundName)
    
    if (nrow(cohorts) == 0) {
      cohorts <-
        dplyr::tibble(compoundName = "Nothing selected.")
    }
    
    return(apply(cohorts, 1, function(x)
      tags$tr(lapply(x, tags$td))))
  })
  
  output$cohortCountsSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$indexEventBreakdownSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$characterizationSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$compareCharacterizationSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$temporalCharacterizationSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$inclusionRuleStatSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$cohortOverlapSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$incidenceRateSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$timeDistSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  output$visitContextSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  
  output$cohortSearchSelectedCohort <-
    shiny::renderUI(expr = {
      idx <- input$cohortSearchTableResults_rows_selected
      selectedCohortRows <- cohortSearchResults()[idx,] %>%
        dplyr::arrange(.data$cohortId) %>%
        dplyr::select(.data$compoundName)
      if (nrow(selectedCohortRows) == 0) {
        selectedCohortRows <-
          dplyr::tibble(compoundName = "Nothing selected.")
      }
      return(apply(selectedCohortRows, 1, function(x)
        tags$tr(lapply(x, tags$td))))
    })
  
  output$cohortCharCompareSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  
  output$conceptSetDiagnosticsSelectedCohort <-
    shiny::renderUI(expr = {
      selectedCohorts()
    })
  
  shiny::observe({
    noCohortSelected <- length(input$cohortSearchTableResults_rows_selected)
    if (noCohortSelected == 0) {
      label <- "Checkout cohorts"
    } else {
      label <- paste("Checkout", noCohortSelected, "cohorts")
    }
    
    updateActionButton(session = session, 
                       inputId =  "loadSelectedCohorts",
                       label = label)
  })
  
})
