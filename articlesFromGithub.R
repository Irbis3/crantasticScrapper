library(httr)
library(dplyr)
library(rvest)

authors = c()
for( i in 1:10) {
github_url <- gsub("page_num", i, "https://github.com/search?l=&p=page_num&q=language%3AR&type=Users")
authors <- c(authors, read_html(github_url) %>% html_nodes(".user-list-info a") %>% html_text())
}

download_R_source_from_github <- function(package_name, author_name) {
  repo_url <- gsub("author", author_name, "https://github.com/author/package") %>% gsub("package", package_name, .)
  files <- read_html(repo_url) %>% html_nodes(".content a") %>% html_attr("title")
  if("DESCRIPTION" %in% files) {
    print(c(author_name, package_name))
    return()
  }
  
  url <- paste(repo_url, "/archive/master.zip", sep="")
  print(url)
  out <- tryCatch(download.file(url, destfile = "temp.zip"), error = function(e) { 
    print("no source on github") 
    return(NULL)
  })
  if(is.null(out)) {
    return()
  }
  
  tryCatch({
    files <- grep('(.*)\\.R$', unzip("temp.zip", list=TRUE)$Name, ignore.case=TRUE, value=TRUE)
    unzip("temp.zip", files=files, exdir = paste("sources/", author_name, "/", package_name, sep=""), junkpaths = TRUE)
    file.remove("temp.zip")
    },
           error = function(e) {
             print("could not unzip") 
             return(NULL)
           })
  

}


download_articles_for_author <- function(author) {
  print(author)
  repos_url <- gsub("author", author,"https://github.com/author?utf8=%E2%9C%93&tab=repositories&q=&type=source&language=r")
  author_repos <- read_html(repos_url) %>% html_nodes(".mb-1 a") %>% html_text() %>% gsub("^\\s+|\\s+$", "", .)
  lapply(author_repos, download_R_source_from_github, author)
}


lapply(authors, function(author) {
  if (!dir.exists(file.path("sources", author))) {
    download_articles_for_author(author)
  }
})

