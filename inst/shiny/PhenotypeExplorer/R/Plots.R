plotTimeDistribution <- function(data, shortNameRef = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTibble(
    x = data,
    any.missing = FALSE,
    min.rows = 1,
    min.cols = 5,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertNames(
    x = colnames(data),
    must.include = c(
      "minValue",
      "p25Value",
      "medianValue",
      "p75Value",
      "maxValue"
    ),
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  
  # plotData <-
  #   addShortName(data = data, shortNameRef = shortNameRef)
  
  data$tooltip <- c(
    paste0(
      data$shortName,
      "\nDatabase = ",
      data$databaseId,
      "\nMin = ",
      scales::comma(data$minValue),
      "\nP25 = ",
      scales::comma(data$p25Value),
      "\nMedian = ",
      scales::comma(data$medianValue),
      "\nP75 = ",
      scales::comma(data$p75Value),
      "\nMax = ",
      scales::comma(data$maxValue),
      "\nTime Measure = ",
      data$timeMetric,
      "\nAverage = ",
      scales::comma(x = data$averageValue, accuracy = 0.01)
    )
  )
  
  plot <- ggplot2::ggplot(data = data) +
    ggplot2::aes(
      x = .data$shortName,
      ymin = .data$minValue,
      lower = .data$p25Value,
      middle = .data$medianValue,
      upper = .data$p75Value,
      ymax = .data$maxValue,
      group = .data$shortName,
      average = .data$averageValue
    ) +
    ggplot2::geom_errorbar(size = 0.5) +
    ggiraph::geom_boxplot_interactive(
      ggplot2::aes(tooltip = tooltip),
      stat = "identity",
      fill = rgb(0, 0, 0.8, alpha = 0.25),
      size = 0.2
    ) +
    ggplot2::facet_grid(databaseId ~ timeMetric, scales = "free") +
    ggplot2::coord_flip() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
  height <-
    1.5 + 0.4 * nrow(dplyr::distinct(data, .data$databaseId, .data$shortName))
  plot <- ggiraph::girafe(
    ggobj = plot,
    options = list(ggiraph::opts_sizing(width = .7),
                   ggiraph::opts_zoom(max = 5)),
    width_svg = 12,
    height_svg = height
  )
}

plotIncidenceRate <- function(data,
                              shortNameRef = NULL,
                              stratifyByAgeGroup = TRUE,
                              stratifyByGender = TRUE,
                              stratifyByCalendarYear = TRUE,
                              minPersonYears = 1000,
                              yscaleFixed = FALSE) {
  
  #filtering incidence rates that are negative
  data <- data %>% 
    dplyr::filter(.data$incidenceRate >= 0)
  
  if (!is.null(minPersonYears) && minPersonYears != "") {
    data <- data %>% 
      dplyr::filter(.data$personYears > minPersonYears)
  }
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTibble(
    x = data,
    any.missing = TRUE,
    min.rows = 1,
    min.cols = 5,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = stratifyByAgeGroup,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = stratifyByGender,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = stratifyByCalendarYear,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = yscaleFixed,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertDouble(
    x = data$incidenceRate,
    lower = 0,
    any.missing = FALSE,
    null.ok = FALSE,
    min.len = 1,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  checkmate::assertDouble(
    x = data$incidenceRate,
    lower = 0,
    any.missing = FALSE,
    null.ok = FALSE,
    min.len = 1,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  
  plotData <- data %>%
    dplyr::left_join(y = cohort %>% dplyr::select(.data$cohortId, .data$cohortName), by = c('cohortId')) %>%
    dplyr::mutate(incidenceRate = round(.data$incidenceRate, digits = 3))
  
  if (nrow(plotData) == 0) {
    return(NULL)
  }
 
  plotData <- plotData %>% 
    dplyr::mutate(strataGender = !is.na(.data$gender),
                  strataAgeGroup = !is.na(.data$ageGroup),
                  strataCalendarYear = !is.na(.data$calendarYear)) %>% 
    dplyr::filter(.data$strataGender %in% !!stratifyByGender &
                    .data$strataAgeGroup %in% !!stratifyByAgeGroup &
                    .data$strataCalendarYear %in% !!stratifyByCalendarYear) %>% 
    dplyr::select(-dplyr::starts_with("strata"))
  
  aesthetics <- list(y = "incidenceRate")
  if (stratifyByCalendarYear) {
    aesthetics$x <- "calendarYear"
    xLabel <- "Calender year"
    showX <- TRUE
    if (stratifyByGender) {
      aesthetics$group <- "gender"
      aesthetics$color <- "gender"
    }
    plotType <- "line"
  } else {
    xLabel <- ""
    if (stratifyByGender) {
      aesthetics$x <- "gender"
      aesthetics$color <- "gender"
      aesthetics$fill <- "gender"
      showX <- TRUE
    } else if (stratifyByAgeGroup) {
      aesthetics$x <- "ageGroup"
      showX <- TRUE
    }
    else{
      aesthetics$x <- "cohortId"
      showX <- FALSE
    }
    plotType <- "bar"
  }
  
  newSort <- plotData %>% 
    dplyr::select(.data$ageGroup) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(as.integer(sub(pattern = '-.+$','',x = .data$ageGroup)))
  
  plotData <- plotData %>% 
    dplyr::arrange(ageGroup = factor(.data$ageGroup, levels = newSort$ageGroup), .data$ageGroup)
  
  plotData$ageGroup <- factor(plotData$ageGroup,
                              levels = newSort$ageGroup)
  plotData$tooltip <- c(paste0(plotData$shortName,
                               "\nIncidence Rate = ", scales::comma(plotData$incidenceRate, accuracy = 0.01), 
                               "\nDatabase = ", plotData$databaseId, 
                               "\nPerson years = ", scales::comma(plotData$personYears, accuracy = 0.1), 
                               "\nCohort count = ", scales::comma(plotData$cohortCount)))
  
  if (stratifyByAgeGroup) {
    plotData$tooltip <- c(paste0(plotData$tooltip, "\nAge Group = ", plotData$ageGroup))
  }
  
  if (stratifyByGender) {
    plotData$tooltip <- c(paste0(plotData$tooltip, "\nGender = ", plotData$gender))
  }
  
  if (stratifyByCalendarYear) {
    plotData$tooltip <- c(paste0(plotData$tooltip, "\nYear = ", plotData$calendarYear))
  }
  
  if (stratifyByGender) {
    # Make sure colors are consistent, no matter which genders are included:
    
    genders <- c("Female", "Male", "No matching concept")
    # Code used to generate palette:
    # writeLines(paste(RColorBrewer::brewer.pal(n = 2, name = "Dark2"), collapse = "\", \""))
    colors <- c("#D95F02", "#1B9E77", "#444444")
    colors <- colors[genders %in% unique(plotData$gender)]
    plotData$gender <- factor(plotData$gender, levels = genders)
  }
  
  plot <- ggplot2::ggplot(data = plotData, do.call(ggplot2::aes_string, aesthetics)) +
    ggplot2::xlab(xLabel) +
    ggplot2::ylab("Incidence Rate (/1,000 person years)") +
    ggplot2::theme(legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   axis.text.x = if (showX) ggplot2::element_text(angle = 90, vjust = 0.5) else ggplot2::element_blank() )
  
  if (plotType == "line") {
    plot <- plot + 
      ggiraph::geom_line_interactive(ggplot2::aes(), size = 1, alpha = 0.6) +
      ggiraph::geom_point_interactive(ggplot2::aes(tooltip = tooltip), size = 2, alpha = 0.6)
  } else {
    plot <- plot +   ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tooltip), alpha = 0.6)
  }
  if (stratifyByGender) {
    plot <- plot + ggplot2::scale_color_manual(values = colors)
    plot <- plot + ggplot2::scale_fill_manual(values = colors)
  }
  # databaseId field only present when called in Shiny app:
  if (!is.null(data$databaseId) && length(data$databaseId) > 1) {
    if (yscaleFixed) {
      scales <- "fixed"
    } else {
      scales <- "free_y"
    }
    if (stratifyByGender | stratifyByCalendarYear) {
      if (stratifyByAgeGroup) {
        plot <- plot + facet_nested(databaseId + shortName ~ plotData$ageGroup, scales = scales)
      } else {
        plot <- plot + facet_nested(databaseId + shortName ~ ., scales = scales) 
      }
    } else {
      plot <- plot + facet_nested(databaseId + shortName ~., scales = scales) 
    }
    # spacing <- rep(c(1, rep(0.5, length(unique(plotData$shortName)) - 1)), length(unique(plotData$databaseId)))[-1]
    spacing <- plotData %>%
      dplyr::distinct(.data$databaseId, .data$shortName) %>%
      dplyr::arrange(.data$databaseId) %>%
      dplyr::group_by(.data$databaseId) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::ungroup()
    spacing <- unlist(sapply(spacing$count, function(x) c(1, rep(0.5, x - 1))))[-1]
    
    plot <- plot + ggplot2::theme(panel.spacing.y = ggplot2::unit(spacing, "lines"),
                                  strip.background = ggplot2::element_blank())
  } else {
    if (stratifyByAgeGroup) {
      plot <- plot + ggplot2::facet_grid(~ageGroup) 
    }
  }
  height <- 1.5 + 1 * nrow(dplyr::distinct(plotData, .data$databaseId, .data$shortName))
  plot <- ggiraph::girafe(ggobj = plot,
                          options = list(
                            ggiraph::opts_sizing(width = .7),
                            ggiraph::opts_zoom(max = 5)),
                          width_svg = 15,
                          height_svg = height)
  return(plot)
}




plotCohortComparison <- function(balance,
                                 maxMean = 1,
                                 minMean = 0.01,
                                 domain = "all") {
  domains <- c("condition",
               "device",
               "drug",
               "measurement",
               "observation",
               "procedure")
  targetCohortName <- cohort %>% 
    dplyr::filter(.data$cohortId == balance %>% 
                    dplyr::filter(!is.na(.data$cohortId1)) %>% 
                    dplyr::pull(.data$cohortId1) %>% 
                    unique()) %>% 
    dplyr::pull(.data$cohortName)
  comparatorCohortName <- cohort %>% 
    dplyr::filter(.data$cohortId == balance %>% 
                    dplyr::filter(!is.na(.data$cohortId2)) %>% 
                    dplyr::pull(.data$cohortId2) %>% 
                    unique()) %>% 
    dplyr::pull(.data$cohortName)
  
  if (domain != 'all') {
    balance = balance %>% 
      dplyr::filter(.data$domainId %in% domain)
  }
  balance <- balance %>% 
    dplyr::mutate(domainId = dplyr::case_when(tolower(.data$domainId) %in% !!domains ~ .data$domainId,
                                                    TRUE ~ 'Other'))
  
  balance <- balance %>%
    dplyr::mutate(mean1 = dplyr::case_when(.data$mean1 < 0 ~ 0, TRUE ~ .data$mean1)) %>%
    dplyr::mutate(mean2 = dplyr::case_when(.data$mean2 < 0 ~ 0, TRUE ~ .data$mean2))
  
  balance <- balance %>% 
    dplyr::filter(.data$mean1 <= !!maxMean) %>%
    dplyr::filter(.data$mean2 <= !!maxMean) %>%
    dplyr::filter(.data$mean1 >= !!minMean) %>%
    dplyr::filter(.data$mean2 >= !!minMean) %>%
    dplyr::mutate(targetCohort = !!targetCohortName) %>% 
    dplyr::mutate(comparatorCohort = !!comparatorCohortName)
  
  # ggiraph::geom_point_interactive(ggplot2::aes(tooltip = tooltip), size = 3, alpha = 0.6)
  balance$tooltip <-
    c(
      paste0(
        "Concept Name: ",
        balance$conceptName,
        "\nDomain: ",
        balance$domainId,
        "\nMean ",
        balance$targetCohort,
        ": ",
        scales::comma(balance$mean1, accuracy = 0.1),
        "\nMean ",
        balance$comparatorCohort,
        ": ",
        scales::comma(balance$mean2, accuracy = 0.1),
        "\nStd diff.:",
        scales::comma(balance$stdDiff, accuracy = 0.1)
      )
    )
  
  # Code used to generate palette:
  # writeLines(paste(RColorBrewer::brewer.pal(n = length(domains), name = "Dark2"), collapse = "\", \""))
  
  # Make sure colors are consistent, no matter which domains are included:
  colors <-
    c("#1B9E77",
      "#D95F02",
      "#7570B3",
      "#E7298A",
      "#66A61E",
      "#E6AB02",
      "#444444")
  colors <- colors[c(domains, "other") %in% tolower(unique(balance$domainId))]
  
  balance$domain <-
    factor(tolower(balance$domainId), levels = c(domains, "other"))
  
  # targetLabel <- paste(strwrap(targetLabel, width = 50), collapse = "\n")
  # comparatorLabel <- paste(strwrap(comparatorLabel, width = 50), collapse = "\n")
  
  
  plot <-
    ggplot2::ggplot(balance,
                    ggplot2::aes(
                      x = .data$mean1,
                      y = .data$mean2,
                      color = .data$domain
                    )) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = .data$tooltip),
      size = 3,
      shape = 16,
      alpha = 0.5
    ) +
    ggplot2::geom_abline(slope = 1,
                         intercept = 0,
                         linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(name = "", limits = c(0,1)) +
    ggplot2::scale_y_continuous(name = "", limits = c(0,1)) +
    ggplot2::scale_color_manual("Domain", values = colors) +
    facet_nested(databaseId + cohortId1 ~ cohortId2) +
    ggplot2::theme(strip.background = ggplot2::element_blank())
  
  plot <- ggiraph::girafe(
    ggobj = plot,
    options = list(ggiraph::opts_sizing(width = .7),
                   ggiraph::opts_zoom(max = 5)),
    width_svg = 12,
    height_svg = 5
  )
  return(plot)
}




plotCohortOverlap <- function(data,
                              shortNameRef = NULL,
                              yAxis = "Percentages") {
  # Perform error checks for input variables
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTibble(
    x = data,
    any.missing = FALSE,
    min.rows = 1,
    min.cols = 6,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  checkmate::assertNames(
    x = colnames(data),
    must.include = c(
      "databaseId",
      "targetCohortId",
      "comparatorCohortId",
      "tOnlySubjects",
      "cOnlySubjects",
      "bothSubjects"
    ),
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  
  plotData <- data %>%
    dplyr::mutate(
      absTOnlySubjects = abs(.data$tOnlySubjects),
      absCOnlySubjects = abs(.data$cOnlySubjects),
      absBothSubjects = abs(.data$bothSubjects),
      absEitherSubjects = abs(.data$eitherSubjects),
      signTOnlySubjects = dplyr::case_when(.data$tOnlySubjects < 0 ~ '<', TRUE ~ ''),
      signCOnlySubjects = dplyr::case_when(.data$cOnlySubjects < 0 ~ '<', TRUE ~ ''),
      signBothSubjects = dplyr::case_when(.data$bothSubjects < 0 ~ '<', TRUE ~ '')
    ) %>%
    dplyr::mutate(
      tOnlyString = paste0(
        .data$signTOnlySubjects,
        scales::comma(.data$absTOnlySubjects),
        " (",
        .data$signTOnlySubjects,
        scales::percent(.data$absTOnlySubjects /
                          .data$absEitherSubjects,
                        accuracy = 1),
        ")"
      ),
      cOnlyString = paste0(
        .data$signCOnlySubjects,
        scales::comma(.data$absCOnlySubjects),
        " (",
        .data$signCOnlySubjects,
        scales::percent(.data$absCOnlySubjects /
                          .data$absEitherSubjects,
                        accuracy = 1),
        ")"
      ),
      bothString = paste0(
        .data$signBothSubjects,
        scales::comma(.data$absBothSubjects),
        " (",
        .data$signBothSubjects,
        scales::percent(.data$absBothSubjects /
                          .data$absEitherSubjects,
                        accuracy = 1),
        ")"
      )
    )  %>%
    dplyr::mutate(
      tooltip = paste0(
        "Database: ",
        .data$databaseId,
        "\n",
        "\n",
        .data$targetCohortName,
        ' only: ',
        .data$tOnlyString,
        "\nBoth: ",
        .data$bothString,
        "\n",
        .data$comparatorCohortName,
        ' only: ',
        .data$cOnlyString
      )
    ) %>%
    dplyr::select(
      .data$targetCohortName,
      .data$comparatorCohortName,
      .data$databaseId,
      .data$absTOnlySubjects,
      .data$absCOnlySubjects,
      .data$absBothSubjects,
      .data$tooltip
    ) %>%
    tidyr::pivot_longer(
      cols = c("absTOnlySubjects",
               "absCOnlySubjects",
               "absBothSubjects"),
      names_to = "subjectsIn",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      subjectsIn = dplyr::recode(
        .data$subjectsIn,
        absTOnlySubjects = "Left cohort only",
        absBothSubjects = "Both cohorts",
        absCOnlySubjects = "Top cohort only"
      )
    )
  
  plotData$subjectsIn <-
    factor(plotData$subjectsIn,
           levels = c("Top cohort only", "Both cohorts", "Left cohort only"))
  
  if (yAxis == "Percentages") {
    position = "fill"
  } else {
    position = "stack"
  }
  
  plot <- ggplot2::ggplot(data = plotData) +
    ggplot2::aes(
      fill = .data$subjectsIn,
      y = targetCohortName,
      x = .data$value,
      tooltip = .data$tooltip,
      group = .data$subjectsIn
    ) +
    ggplot2::ylab(label = "") +
    ggplot2::xlab(label = "") +
    ggplot2::scale_fill_manual("Subjects in", values = c(rgb(0.8, 0.2, 0.2), rgb(0.3, 0.2, 0.4), rgb(0.4, 0.4, 0.9))) +
    ggplot2::facet_grid(databaseId ~ comparatorCohortName) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray"),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggiraph::geom_bar_interactive(position = position,
                                  alpha = 0.6,
                                  stat = "identity")
  if (yAxis == "Percentages") {
    plot <- plot + ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    plot <- plot + ggplot2::scale_x_continuous(labels = scales::comma)
  }
  width <- 1 + 0.5 * length(unique(plotData$comparatorCohortName))
  height <-
    0.25 + 0.08 * nrow(dplyr::distinct(plotData, .data$databaseId, .data$targetCohortName))
  aspectRatio <- width / height
  plot <- ggiraph::girafe(
    ggobj = plot,
    options = list(ggiraph::opts_sizing(width = .7),
                   ggiraph::opts_zoom(max = 5)),
    width_svg = 12,
    height_svg = 12 / aspectRatio
  )
  return(plot)
}






plotTemporalCohortComparison <- function(balance,
                                         shortNameRef = NULL,
                                         maxMean = 1,
                                         minMean = 0.01,
                                         temporalChoice = "all",
                                         domain = "all") {

  domains <- c("condition",
               "device",
               "drug",
               "measurement",
               "observation",
               "procedure")
  
  temporalChoices = c("Start 0 to end 0", 
                      "Start -365 to end -31",
                      "Start -30 to end -1",
                      "Start 31 to end 365",
                      "Start 1 to end 30" )
  
  if (domain != 'all') {
    balance = balance %>% 
      dplyr::filter(.data$domainId %in% domain)
  }
  
  if (temporalChoice != 'all') {
    balance = balance %>% 
      dplyr::filter(.data$temporalChoices %in% temporalChoice)
  } else {
    balance = balance %>% 
      dplyr::filter(.data$temporalChoices %in% temporalChoices)
  }
  
  balance <- balance %>% 
    dplyr::mutate(domainId = dplyr::case_when(tolower(.data$domainId) %in% !!domains ~ .data$domainId,
                                              TRUE ~ 'Other'))
  
  balance <- balance %>%
    dplyr::mutate(mean1 = dplyr::case_when(.data$mean1 < 0 ~ 0, TRUE ~ .data$mean1)) %>%
    dplyr::mutate(mean2 = dplyr::case_when(.data$mean2 < 0 ~ 0, TRUE ~ .data$mean2))
  
  balance <- balance %>% 
    dplyr::filter(.data$mean1 <= !!maxMean) %>%
    dplyr::filter(.data$mean2 <= !!maxMean) %>%
    dplyr::filter(.data$mean1 >= !!minMean) %>%
    dplyr::filter(.data$mean2 >= !!minMean)
  
  if (nrow(balance) == 0) {
    return(NULL)
  }
  
  balance$tooltip <- c(
    paste(
      "Database: ",
      balance$databaseId,
      "\nCovariate Name:",
      balance$conceptName,
      "\nDomain:",
      balance$domainId,
      "\nTime:",
      balance$temporalChoices,
      "\nX- ",
      balance$shortName1,
      ": ",
      scales::percent(balance$mean1, accuracy = 0.01),
      "\nY- ",
      balance$shortName2,
      ": ",
      scales::percent(balance$mean2, accuracy = 0.01),
      "\nStd.Diff.",
      scales::percent(balance$stdDiff, accuracy = 0.01)
    )
  )
  
  # Code used to generate palette:
  # writeLines(paste(RColorBrewer::brewer.pal(n = length(balance$temporalChoices), name = "Dark2"), collapse = "\", \""))
  
  # Make sure colors are consistent, no matter which domains are included:
  colors <-
    c("#1B9E77",
      "#D95F02",
      "#7570B3",
      "#E7298A",
      "#66A61E",
      "#E6AB02",
      "#444444")
  colors <- colors[temporalChoices %in% unique(balance$temporalChoices)]
  
  balance$domain <-
    factor(tolower(balance$domainId), levels = c(domains, "other"))
  
  balance$rowname <- row.names(balance)
  
  plot <- ggplot2::ggplot(
    balance,
    ggplot2::aes(
      x = .data$mean1,
      y = .data$mean2,
      color = .data$temporalChoices
    )
  ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = .data$tooltip, data_id = .data$rowname),
      size = 3,
      shape = 16,
      alpha = 0.5
    ) +
    ggplot2::geom_abline(slope = 1,
                         intercept = 0,
                         linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(name = "", limits = c(0,1)) +
    ggplot2::scale_y_continuous(name = "", limits = c(0,1)) +
    ggplot2::scale_color_manual("Time Periods", values = colors) +
    facet_nested(databaseId + shortName1 ~ shortName2) +
    ggplot2::theme(strip.background = ggplot2::element_blank())
  width <- 1 + 1 * length(unique(balance$shortName2))
  height <-
    0.5 + 0.5 * nrow(dplyr::distinct(balance, .data$databaseId, .data$shortName1))
  aspectRatio <- width / height
  
  plot <- ggiraph::girafe(
    ggobj = plot,
    options = list(ggiraph::opts_sizing(width = .7),
                   ggiraph::opts_zoom(max = 5)),
    width_svg = 12,
    height_svg = 12 / aspectRatio
  )
  return(plot)
}


