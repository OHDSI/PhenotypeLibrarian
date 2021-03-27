appendToAnnotationService <-
  function(menuItem,
           annotation,
           cohortIds,
           databaseIds,
           userName = 'Anonymous',
           userPriority = 0,
           userAuthenticated = FALSE,
           commentPriority = 0,
           appSignature, 
           annotationPermission,
           annotationService) {
    if (annotationPermission %in% c("WRITE", "FULL")) {
      data <- dplyr::tibble(
        "dateTimeEntry" = Sys.time(),
        "menuItem" = !!menuItem,
        "userName" = !!userName,
        "userPriority" = !!userPriority,
        "commentPriority" = !!commentPriority,
        "userAuthenticated" = !!userAuthenticated,
        "annotation" = !!annotation,
        "appSignature" = !!appSignature
      ) %>% 
        tidyr::crossing(dplyr::tibble(cohortId = !!cohortIds)) %>% 
        tidyr::crossing(dplyr::tibble(databaseId = !!databaseIds)) %>% 
        dplyr::select(.data$appSignature,
                      .data$menuItem,
                      .data$databaseId,
                      .data$cohortId,
                      .data$userName,
                      .data$dateTimeEntry,
                      .data$annotation,
                      .data$userPriority,
                      .data$commentPriority,
                      .data$userAuthenticated)
      shiny::withProgress(message = "Saving annotation...", {
        if (annotationService == 'GoogleSheets') {
          googlesheets4::sheet_append(
            Sys.getenv('PhenotypeLibrarianCommentsGoogleSheets'),
            data = data,
            sheet = "annotation"
          )
        }
        if (annotationService == 'DBMS') {
          # TO DO. Use the same pool connection to write/read. Permission.
          data <- NULL
        }
      })
    }
  }

readFromAnnotationService <- function(menuItem, 
                                      annotationPermission,
                                      appSignatureValue,
                                      annotationService) {
  if (annotationPermission %in% c("READ","FULL")) {
    shiny::withProgress(message = "Reading annotation service..", expr = {
      if (annotationService == 'GoogleSheets') {
        data <-
          googlesheets4::read_sheet(
            Sys.getenv('PhenotypeLibrarianCommentsGoogleSheets'),
            sheet = 'annotation',
            trim_ws = TRUE
          ) %>%
          dplyr::filter(.data$appSignature == !!appSignatureValue) %>% 
          dplyr::filter(.data$menuItem == !!menuItem)
        return(data)
      }
      else if (annotationService == 'DBMS') {
        # TO DO. Use the same pool connection to retrieve.
        data <- NULL
      }
    })
  }
}
