library(rvest)
library(httr)
library(dplyr)
base_crantastic_link <- "https://crantastic.org"
base_github_link <- "https://https://github.com/cran"

# Parse crantastic.org for R package author.
# Returns authors and link to profile on crantastic.
get_authors <- function() {
  if(exists("authors_data_frame")) {
    return (authors_data_frame)
  }
  crantastic_authors <- read_html(paste(base_crantastic_link, "/authors", sep = ""))
  authors <- html_nodes(crantastic_authors, "a")
  author_names <- html_text(authors)
  author_names
  author_packages_link <- html_attr(authors, "href")
  author_packages_link
  authors_data_frame <<- data.frame(name = author_names, link = author_packages_link)
  return(authors_data_frame)
}

download_R_source_from_github <- function(package_name, author_name) {
  url <- gsub("package_name", package_name, "https://github.com/cran/package_name/archive/master.zip")
  out <- tryCatch(download.file(url, destfile = "temp.zip"), error = function(e) { 
    print("no source on github") 
    return(NULL)
  })
  if(is.null(out)) {
    return()
  }
  files <- grep('/R/(.*)\\.R$', unzip("temp.zip", list=TRUE)$Name, ignore.case=TRUE, value=TRUE)
  unzip("temp.zip", files=files, exdir = paste("sources/", author_name, "/", package_name, sep=""), junkpaths = TRUE)
  file.remove("temp.zip")
}

download_packages_for_author <- function(author_name) {
  print(as.character(author_name))
  author_crantastic <- get_authors() %>% filter(name == author_name)
  author_crantastic_link <- author_crantastic$link
  crantastic_author <- read_html(paste(base_crantastic_link, author_crantastic_link, sep = "" ))
  html_nodes(crantastic_author, "#main ul:nth-of-type(1) li a") %>% 
    html_text() -> author_package_names
  sapply(author_package_names, download_R_source_from_github, author_crantastic_link)
  return(list(as.character(author_name), as.character(author_crantastic_link), author_package_names))
}

# Usage - following code could be used to download packages from 100 authors
names <- (get_authors() %>% select(name))[1101:1200,]
lapply(names, download_packages_for_author)

authors <- get_authors()
id <- substr(authors$link, 10, 20)
authors_with_id <- cbind(authors, id)



author_dirs <- list.dirs("output/sources/authors", recursive = FALSE)
authors_packages <- list.dirs(author_dirs[1], recursive = FALSE)


#analyze package
package_dir <- authors_packages[1]
filenames <- list.files(package_dir, pattern="*.R", full.names=TRUE)
filenames[1]
#analyze file
con = file(filenames[1], "r")
lines <- readLines(con)

lines_num <- length(lines)
lines_length <- unlist(lapply(lines, nchar))
functions_num <- sum(grepl("function\\(", lines))
comments_num <- sum(grepl("^\\s*#", lines))
