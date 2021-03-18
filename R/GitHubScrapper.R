getRepositoryDetailsForGitHubOrganization <-
  function(organizations,
           githubToken,
           pages = 10,
           perPage = 100) {

    listOfRepositories <- list()
    k <- 0
    for (i in (1:length(organizations))) {
      organization <- organizations[[i]]
      for (j in (1:pages)) {
        page <- j
        httrGet <- httr::GET(url = paste0("https://api.github.com/orgs/",
                          organization,
                          "/repos?&per_page=",
                          perPage,
                          "&page=",
                          page),
                          httr::add_headers(Authorization = paste("token", githubToken)))
        httrContent <- httr::content(x = httrGet)
        if (length(httrContent) > 0) {
          for (l in (1:length(httrContent))) {
            k <- k + 1
            listOfRepositories[[k]] <- dplyr::tibble(name = httrContent[[l]]$name,
                                                     organization = organization,
                                                     defaultBranch = httrContent[[l]]$default_branch)
          }
        }
      }
    }
    output <- dplyr::bind_rows(listOfRepositories)
    return(output)
}


getListOfFilesInGitHubRepositories <- function(repo,
                                               githubToken,
                                       branch) {
  req <- httr::GET(paste0("https://api.github.com/repos/",
                          repo,
                          "/git/trees/",
                          branch,
                          "?recursive=1"),
                   httr::add_headers(Authorization = paste("token", githubToken)))
  if (!httr::status_code(req) %in% c('404')) {
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
    return(filelist)
  }
}
