
# Create empty objects in memory for all other tables. This is used by the Shiny app to decide what tabs to show:
isEmpty <-
  function(connection,
           resultsDatabaseSchema,
           tableName) {
    sql <-
      sprintf("SELECT 1 FROM %s.%s LIMIT 1;",
              resultsDatabaseSchema,
              tableName)
    oneRow <-
      DatabaseConnector::dbGetQuery(conn = connection, sql) %>%
      dplyr::tibble()
    return(nrow(oneRow) == 0)
  }
