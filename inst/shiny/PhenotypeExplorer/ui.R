shinyWidgetsPickerOptions <- shinyWidgets::pickerOptions(
  actionsBox = TRUE,
  liveSearch = TRUE,
  liveSearchNormalize = TRUE,
  size = 'auto',
  liveSearchStyle = "contains",
  liveSearchPlaceholder = "Not selected",
  virtualScroll = 50,
  # mobile = TRUE,
  selectOnTab = TRUE,
  showTick = TRUE,
  width	= 'auto',
  windowPadding = 2,
  dropdownAlignRight = TRUE,
  dropupAuto = TRUE
)

defaultHeaderbars <- 
  tags$li(
    tags$div(
      tags$li(
        tags$div(
          shiny::conditionalPanel(
            condition = "output.isHeaderbarVisible == true",
            tags$strong("Database:"),
            style = "color: white; margin-right: 10px; margin-top: 14px;"
          )),
        class = "dropdown"
      ),
      tags$li(
        tags$div(
          shiny::conditionalPanel(
            condition = "output.isHeaderbarVisible == true",
            shinyWidgets::pickerInput(
              inputId = "selectedDatabases",
              choices = NULL,
              width = 300,
              multiple = TRUE,
              options = shinyWidgetsPickerOptions
            ),
            style = "margin-top: 8px; margin-right: 10px; margin-bottom: -8px;")
        ),
        class = "dropdown"
      ),
      tags$li(
        tags$div(
          shiny::conditionalPanel(
            condition = "output.isHeaderbarVisible == true",
            tags$strong("Cohorts:"),
            style = "color: white; margin-right: 10px; margin-top: 14px;"
          )),
        class = "dropdown"
      ),
      tags$li(
        tags$div(
          shiny::conditionalPanel(
            condition = "output.isHeaderbarVisible == true",
            shinyWidgets::pickerInput(
              inputId = "selectedCohorts",
              choices = NULL,
              multiple = TRUE,
              width = 300,
              options = shinyWidgetsPickerOptions
            ),
            style = "margin-top: 8px; margin-right: 10px; margin-bottom: -8px;"
          )
        ),
        class = "dropdown"
      ),
      tags$li(
        tags$div(
          if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
            shiny::conditionalPanel(
              condition = "output.isHeaderbarVisible == true",
              tags$strong("Phenotype:"),
              style = "color: white; margin-right: 10px; margin-top: 14px;"
            )
          }),
        class = "dropdown"
      ),
      tags$li(
        tags$div(
          if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
            shiny::conditionalPanel(
              condition = "output.isHeaderbarVisible == true",
              shinyWidgets::pickerInput(
                inputId = "selectedPhenotypes",
                choices = NULL,
                multiple = TRUE,
                width = 300,
                options = shinyWidgetsPickerOptions
              ),
              style = "margin-top: 8px; margin-right: 10px; margin-bottom: -8px;"
            )
          }
        ),
        class = "dropdown"
      )
    ),
    class = "dropdown"
  )

header <-
  shinydashboard::dashboardHeader(
    title = appTitle,
    tags$li(
      tags$div(
        defaultHeaderbars
      ),
      class = "dropdown"
    )
  )

#sidebarMenu
sidebarMenu <-
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::sidebarMenuOutput(outputId = "menuItems"),
    htmltools::withTags(
      div(style = "margin-left : 10px",
          h5(appVersion)
      )
    )
  )

#Side bar code
sidebar <-
  shinydashboard::dashboardSidebar(sidebarMenu, width = NULL, collapsed = FALSE)

# Body - items in tabs --------------------------------------------------
bodyTabItems <- shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "search",
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      shiny::column(width = 10,
                    tags$div(style = "max-height: 100px; overflow-y: auto",
                             shiny::uiOutput(outputId = "cohortSearchSelectedCohort"))
      ),
      shiny::column(width = 2,
                    shiny::actionButton(
                      inputId = "loadSelectedCohorts",
                      label = "",
                      style = "background-color: #00a65a; color : white; align: right"
                    ))
    ),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      shiny::column(3),
      shiny::column(width = 6,
                    shiny::textInput(
                      inputId = "searchText",
                      label = "Search",
                      placeholder = "Type here to search"
                    )
      ),
      DT::DTOutput(outputId = "cohortSearchTableResults"),
      shiny::conditionalPanel(
        condition = "output.cohortSearchResultsCountOfSelected > 0",
        shiny::tabsetPanel(
          id = "selectedCohortAndPhenotypeDetails",
          shiny::tabPanel(
            title = "Cohort Details",
            tags$br(),
            shiny::conditionalPanel(
              condition = "output.cohortSearchResultsCountOfSelected == 2",
              shiny::radioButtons(
                inputId = "compareCohorts",
                label = "Comparision",
                choices = c(
                  "No Comparision",
                  "Difference in Logic description",
                  "Difference in JSON expression",
                  "Difference in SQL"
                ),
                selected = "No Comparision",
                inline = TRUE
              )
            ),
            shiny::uiOutput(outputId = "dynamicUIGenerationCohortDetailsOne"),
            shiny::uiOutput(outputId = "dynamicUIGenerationCohortDetailsTwo"),
            
            column(
              12,
              conditionalPanel(
                "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='Difference in Logic description'",
                diffr::diffrOutput(outputId = "logicDifferenceBetweenCohorts", width = "100%")
              )
            ),
            column(
              12,
              conditionalPanel(
                "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='Difference in JSON expression'",
                diffr::diffrOutput(
                  outputId = "jsonDifferenceBetweenCohorts",
                  width = "100%",
                  height = "50000px"
                )
              )
            ),
            column(
              12,
              conditionalPanel(
                "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='Difference in SQL'",
                diffr::diffrOutput(
                  outputId = "sqlDifferenceBetweenCohorts",
                  width = "100%",
                  height = "50000px"
                )
              )
            )
            
          )
        )
      ),
      column(
        12,
        conditionalPanel(
          "output.cohortSearchResultsCountOfSelected == 2&
                                      input.compareCohorts=='No Comparision'&
                                      output.isResolveClicked",
          shinydashboard::box(
            title = "Concept Set comparison",
            collapsible = TRUE,
            collapsed = FALSE,
            width = NULL,
            shiny::tabsetPanel(
              id = "comparingResolved",
              shiny::tabPanel(
                "Present in left but not right",
                id = "presentInLeft",
                DT::DTOutput(outputId = "presentInLeftTable")
              ),
              shiny::tabPanel(
                "Present in right but not left",
                id = "presentInReft",
                DT::DTOutput(outputId = "presentInRightTable")
              ),
              shiny::tabPanel(
                "Present in both",
                id = "presentInBoth",
                DT::DTOutput(outputId = "presentInBothTable")
              )
            )
          )
        )
      ),
      column(
        12,
        tags$hr(),
        shinydashboard::box(
          title = paste0("Notes from the ", appSignatureValue, " library", " (testing only):"),
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          uiOutput("searchCommentResults")
        ),
        shinydashboard::box(
          title = "Create New Note (testing only):",
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          column(12,
          column(4,
          shinyWidgets::pickerInput(
            inputId = "selectedCohortIdsForSearch",
            label = "Cohort IDs :",
            choices = NULL,
            multiple = TRUE,
            width = 300,
            options = shinyWidgetsPickerOptions
          ))
          # ,
          # column(4,
          #        shiny::textInput(inputId = "searchUsername",
          #                         label = "Username (Optional)",
          #                         value = "Anonymous",
          #                         width = NULL)
          #        )
          # ,
          # column(4,
          #        shiny::conditionalPanel(
          #          condition = "input.searchUsername != 'Anonymous' && input.searchUsername != ''",
          #          shiny::passwordInput(
          #            inputId = "searchPassword",
          #            label = "Password (Optional)",
          #            width = NULL
          #          )
          #        )
          # )
          ),
          markdownInput::markdownInput(
            inputId = "searchComment",
            label = "Notes:",
            value = "",
            height = "100px"
          ),
          tags$div(
            style = "text-align:right",
            shiny::actionButton(inputId = "submitSearchComment",
                                label = "Submit Comment")
          )
        )
      ),
      tags$style(
        HTML("
        #searchCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
      )
      
      
      
    )
  ),
  shinydashboard::tabItem(tabName = "about",
                          if (exists(x = "aboutText"))
                            HTML(aboutText)),
  shinydashboard::tabItem(
    tabName = "conceptSetDiagnostics",
    cohortReference("conceptSetDiagnosticsSelectedCohort"),
    shinydashboard::box(
      title = "Concept Set Diagnostics (DO NOT USE - still testing)",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("conceptSetsForUpsetTable"),
      shiny::plotOutput("upsetPlotForConceptSets",height = "700px"),
    )
  ),
  shinydashboard::tabItem(
    tabName = "cohortCounts",
    cohortReference("cohortCountsSelectedCohort"),
    shiny::radioButtons(
      inputId = "pivotCohortCount",
      label = "Pivot data over data sources with value from",
      selected = "Subjects",
      inline = TRUE,
      choices = c(
        "All",
        "Subjects",
        "Entries"
      )),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("cohortCountsTable")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("cohortCountsCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForCohortCounts",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForCohortCounts",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "cohortCountsComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitCohortCountsComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #searchCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "incidenceRate",
    cohortReference("incidenceRateSelectedCohort"),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      collapsible = FALSE,
      shiny::column(width =  3,
                    shiny::checkboxGroupInput(
                      inputId = "irStratification",
                      label = "Stratify by",
                      choices = c("Age", "Gender", "Calendar Year"),
                      selected = c("Age", "Gender", "Calendar Year"),
                      inline = TRUE
                    )
      ),
      shiny::column(width = 3,
                    shiny::conditionalPanel(condition = "input.irStratification.indexOf('Age') > -1",
                                            shinyWidgets::pickerInput(
                                              inputId = "incidenceRateAgeFilter",
                                              label = "Select Age Range",
                                              choices = NULL,
                                              inline = FALSE,
                                              multiple = TRUE,
                                              width = 'auto',
                                              options = shinyWidgetsPickerOptions
                                            ))
      ),
      shiny::column(width = 3,
                    shiny::conditionalPanel(condition = "input.irStratification.indexOf('Gender') > -1",
                                            shinyWidgets::pickerInput(
                                              inputId = "incidenceRateGenderFilter",
                                              label = "select Gender",
                                              choices = NULL,
                                              inline = FALSE,
                                              multiple = TRUE,
                                              width = 'auto',
                                              options = shinyWidgetsPickerOptions
                                            ))),
      shiny::column(width = 3,
                    shiny::conditionalPanel(condition = "input.irStratification.indexOf('Calendar Year') > -1",
                                            shinyWidgets::pickerInput(
                                              inputId = "incidenceRateCalendarFilter",
                                              label = "Select Calendar Year",
                                              choices = NULL,
                                              inline = FALSE,
                                              multiple = TRUE,
                                              width = 'auto',
                                              options = shinyWidgetsPickerOptions
                                            ))),
      shiny::column(width = 4,
                    shiny::textInput(inputId = "minPersonYear", label = "Minimum person years", value = 1000))
    ),
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      shiny::column(width = 12, 
                    shiny::checkboxInput(inputId = "irYscaleFixed", 
                                         label = "Use same y-scale across databases",
                                         value = FALSE)),
      shiny::column(width = 12,
                    ggiraph::ggiraphOutput(
                      outputId = "incidenceRatePlot",
                      width = "100%",
                      height = "100%"
                    ))
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("incidenceRateTable")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("incidenceRateCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForIncidenceRate",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForIncidenceRate",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "incidenceRateComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitIncidenceRateComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #incidenceRateCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "timeDistribution",
    cohortReference("timeDistSelectedCohort"),
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      ggiraph::ggiraphOutput(
        outputId = "timeDistributionPlot",
        width = "100%",
        height = "100%"
      )
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("timeDistributionTable")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("timeDistributionCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForTimeDistribution",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForTimeDistribution",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "timeDistributionComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitTimeDistributionComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #timeDistributionCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "inclusionRuleStats",
    cohortReference("inclusionRuleStatSelectedCohort"),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("inclusionRuleTable")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("inclusionRuleStatisticsCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForInclusionRuleStatistics",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForInclusionRuleStatistics",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "inclusionRuleStatisticsComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitInclusionRuleStatisticsComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #inclusionRuleStatisticsCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "indexEventBreakdown",
    cohortReference("indexEventBreakdownSelectedCohort"),
    shiny::radioButtons(
      inputId = "pivotIndexEventBreakDown",
      label = "Pivot data over data sources with value from",
      selected = "Subject count",
      inline = TRUE,
      choices = c(
        "All",
        "Concept count",
        "Subject count"
        # ,
        # "Percent entries",
        # "Percent persons"
      )),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("indexEventBreakDownTable")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("indexEventBreakDownCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForIndexEventBreakDown",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForIndexEventBreakDown",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "indexEventBreakDownStatisticsComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitIndexEventBreakDownComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #inclusionRuleStatisticsCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "visitContext",
    cohortReference("visitContextSelectedCohort"),
    shiny::radioButtons(
      inputId = "pivotVisitContext",
      label = "Show only",
      selected = "Percent",
      inline = TRUE,
      choices = c(
        "All",
        "Percent",
        "Subjects"
      )),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("visitContextTable")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("visitContextCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForVisitContext",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForVisitContext",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "visitContextComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitVisitContextComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #visitContextCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "cohortCharacterization",
    cohortReference("characterizationSelectedCohort"),
    shinydashboard::box(
      title = "Table 1",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      shiny::column(width = 6,
                    shinyWidgets::pickerInput(
                      inputId = "characterizationTablePrettyDtDropDownDatabase",
                      label = "Database",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 6,
                    shinyWidgets::pickerInput(
                      inputId = "characterizationTablePrettyDtDropDownCohort",
                      label = "Cohort",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::uiOutput(outputId = "characterizationTablePrettyCohortCountText"),
      shinydashboard::box(
        title = NULL,
        width = NULL,
        status = NULL,
        collapsible = TRUE,
        collapsed = FALSE,
        DT::DTOutput("characterizationTablePrettyDt")
      )
    ),
    shinydashboard::box(
      title = "Cohort Characteristics (Raw)",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      shinydashboard::box(
        title = NULL,
        width = NULL,
        status = "primary",
        collapsible = FALSE
      ),
      shiny::tabsetPanel(
        id = "characterizationTableRawTabsetPanel",
        shiny::tabPanel(
          title = "Proportion",
          value = "characterizationTableRawProportionTab",
          tags$br(),
          DT::DTOutput(outputId = "characterizationTableRawProportion")
        ),
        shiny::tabPanel(
          title = "Continuous",
          value = "characterizationTableRawContinuousTab",
          tags$br(),
          DT::DTOutput(outputId = "characterizationTableRawContinuous")
        )
      )
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("cohortCharacterizationCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForCohortCharacterization",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForCohortCharacterization",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "cohortCharacterizationComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitCohortCharacterizationComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #cohortCharacterizationCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "compareCharacterization",
    cohortReference("compareCharacterizationSelectedCohort"),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      collapsible = FALSE,
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "compareCharacterizationTableDropDownDatabase",
                      label = "Database",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "compareCharacterizationTableDropCohort1",
                      label = "Target cohort",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "compareCharacterizationTableDropCohort2",
                      label = "Comparator cohort",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "compareCharacterizationDomainChoices",
                      label = "Domain choices",
                      choices = NULL,
                      multiple = TRUE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 12,
                    shiny::uiOutput(outputId = "compareCharacterizationTargetAndComparatorCohort"))
    ),
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      ggiraph::ggiraphOutput(outputId = "compareCharacterizationTablePlot", 
                             height = "100%")
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::tabsetPanel(
        id = "compareCharacterizationTabsetPanel",
        shiny::tabPanel(
          title = "Proportion",
          value = "compareCharacterizationProportionTableDtTabPanel",
          tags$br(),
          DT::DTOutput(outputId = "compareCharacterizationProportionTableDt")
        ),
        shiny::tabPanel(
          title = "Continuous",
          value = "compareCharacterizationContinuousTableDtTabPanel",
          tags$br(),
          DT::DTOutput(outputId = "compareCharacterizationContinuousTableDt")
        )
      )
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("compareCohortCharCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForCompareCohortChar",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForCompareCohortChar",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "compareCohortCharComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitCompareCohortCharComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #compareCohortCharCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "temporalCharacterization",
    cohortReference("temporalCharacterizationSelectedCohort"),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      collapsible = FALSE,
      shiny::column(width = 4,
                    shinyWidgets::pickerInput(
                      inputId = "temporalCharacterizationAnalysisNameFilter",
                      label = "Analysis Choices",
                      choices = NULL,
                      multiple = TRUE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 4,
                    shinyWidgets::pickerInput(
                      inputId = "temporalCharacterizationDomainFilter",
                      label = "Domain Choices",
                      choices = NULL,
                      multiple = TRUE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 4,
                    shinyWidgets::pickerInput(
                      inputId = "temporalChoices",
                      label = "Temporal Choices",
                      choices = "",
                      multiple = TRUE,
                      inline = FALSE,
                      width = 300,
                      options = shinyWidgetsPickerOptions
                    ))
    ),
    shinydashboard::box(
      title = "Temporal Characterization Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      ggiraph::ggiraphOutput(
        outputId = "temporalCharacterizationPlot",
        height = "100%"
      )
    ),
    shinydashboard::box(
      title = "Temporal Characterization Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("temporalCharacterizationTable")
    ),
    shinydashboard::box(
      title = "Raw data",
      width = NULL,
      status = "primary",
      collapsible = TRUE, 
      collapsed = TRUE,
      DT::DTOutput("temporalCharacterizationTableRaw")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("temporalCharacterizationCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForTemporalCharacterization",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForTemporalCharacterization",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "temporalCharacterizationComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitTemporalCharacterizationComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #temporalCharacterizationCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(
    tabName = "cohortOverlap",
    cohortReference("cohortOverlapSelectedCohort"),
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      shiny::radioButtons(
        inputId = "overlapPlotType",
        label = "",
        choices = c("Percentages", "Counts"),
        selected = "Percentages",
        inline = TRUE
      ),
      collapsible = TRUE,
      collapsed = FALSE,
      ggiraph::ggiraphOutput(
        outputId = "cohortOverlapPlot",
        width = "100%",
        height = "100%"
      )
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("cohortOverlapData")
    ),
    shinydashboard::box(
      title = "Phenotype notes:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      uiOutput("cohortOverlapCommentResults")
    ),
    shinydashboard::box(
      title = "Create New Note:",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      column(4,
      shinyWidgets::pickerInput(
        inputId = "selectedCohortIdsForCohortOverlap",
        label = "Cohort IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      column(8,
      shinyWidgets::pickerInput(
        inputId = "selectedDatabaseIdsForCohortOverlap",
        label = "Database IDs :",
        choices = NULL,
        multiple = TRUE,
        width = 300,
        options = shinyWidgetsPickerOptions
      )),
      markdownInput::markdownInput(
        inputId = "cohortOverlapComment",
        label = "Notes:",
        value = "",
        height = "100px"
      ),
      tags$div(
        style = "text-align:right",
        shiny::actionButton(inputId = "submitCohortOverlapComment",
                            label = "Submit Comment")
      )
    ),
    tags$style(
      HTML("
        #cohortOverlapCommentResults {
                                max-height: 200px;
                                overflow: auto;
                               }
             ")
    )
  ),
  shinydashboard::tabItem(tabName = "databaseInformation",
                          DT::DTOutput("databaseInformationTable"),
                          )
)


#body
body <- shinydashboard::dashboardBody(bodyTabItems, 
                                      tags$script(HTML("$('body').addClass('fixed');")) # fixed header bar.
                                      )


#main
shinydashboard::dashboardPage(
  tags$head(tags$style(HTML(
    "
      th, td {
        padding-right: 10px;
      }
      
      #sidebarItemExpanded h5 {
        position: absolute;
        bottom: 0;
        width: 100%;
      }
    "
  ))),
  header = header,
  sidebar = sidebar,
  body = body
)
