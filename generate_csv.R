library(NCmisc)
library(qdap)
library(stringi)
library(dplyr)

functions_for_author <- function(author_repo_path) {
  funtions_by_repo <- list()
  author_repos <- list.files(author_repo_path, full.names = TRUE)
  for(repo in author_repos) {
    print(repo)
    file_paths <- list.files(repo, pattern=glob2rx("*.R"), full.names = TRUE)
    funtions_by_repo[[repo]] = lapply(file_paths, function(file_path) {
      res = NULL
      tryCatch(
        {
          res = list.functions.in.file(file_path)
        },
        error = function(e) {
          print(paste("error in file", file_path))
        })
      return(res)
    })
  }
  return(funtions_by_repo)
}

functions_for_author("B_analysts_sources_github/bcaffo")

sources_folder_path = "B_analysts_sources_github"
authors <- list.files(sources_folder_path)
functions_by_author <- list()

for(author in authors) {
  print(author)
  functions_by_author[[author]] = functions_for_author(file.path(sources_folder_path, author))
}

