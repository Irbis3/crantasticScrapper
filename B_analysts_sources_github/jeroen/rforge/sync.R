# sudo apt-get install subversion git-svn curl
Sys.setenv(GITHUB_PAT = readLines('token.txt')[1])

# These fail for whatever reason. 
# Usually either disabled on r-forge or has files bigger than 100MB
blacklist <- 
  c("adrminer", "arules", "bayessdeevol", "bbmm-analysis", "biocep-distrib",
  "casper", "dcgor", "dnet", "dtrees2", "dtw", "estimators4nfi",
  "flr", "fpt", "fxregime", "gamboostlss", "genabel", "gsdesign",
  "gsif", "gsmoothr", "gwidgets", "hmmr", "htda", "ihelp", "ipeglim",
  "jmr", "laicpms", "metasem", "mmsa", "morgan-rtools", "mpdir",
  "msgl", "multivarseg", "nem", "neuroim", "oem", "omearalab",
  "open-tfdea", "optbiomarker", "patchwork", "pmoments", "polyploid",
  "ptauxpc", "rftestproject12", "r-survey", "seqinr", "sprint",
  "teatime", "toxcast", "trajectory-sim", "treevo", "tuner", "waveslim",
  "wavetiling", "hyperspec")

# For scraping repositories
# library(rvest)

sys <- function(name, args){
  res <- system2(name, args)
  if(res != 0)
    stop(sprintf("Command failed: %s, %s", name, paste(args, collapse = " ")))
}

github_pat <- function () {
  pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  if(is.na(pat))
    stop("Need to set GITHUB_PAT")
  return(pat)
}

find_pages <- function(){
  main <- xml2::read_html("https://r-forge.r-project.org/softwaremap/full_list.php?page=0")
  links <- rvest::html_nodes(main, 'a[href^="/softwaremap/full_list.php?page="]')
  paste0("https://r-forge.r-project.org/", unique(rvest::html_attr(links, "href")))
}

find_projects <- function(page){
  main <- xml2::read_html(page)
  links <- rvest::html_nodes(main, 'a[href^="https://r-forge.r-project.org/projects/"]')
  cat(sprintf("Found %d projects in %s\n", length(links), basename(page)))
  rvest::html_attr(links, "href")  
}

# All projects, including long dead ones
find_all_projects <- function(){
  pages <- find_pages()
  projects <- lapply(pages, find_projects)
  sort(basename(unlist(projects)))
}

# Projects with any activity after Sep 15, 2011
find_maintained_projects <- function(){
  projects <- find_projects('http://r-forge.r-project.org/top/mostactive.php')
  sort(basename(unlist(projects)))  
}

# Projects with activity in the past week
find_active_projects <- function(){
  projects <- find_projects('http://r-forge.r-project.org/top/mostactive.php?type=week')
  sort(basename(projects))
}

gh <- function(){
  httr::add_headers("Authorization" = paste("token", github_pat()))
}

make_repo <- function(project){
  req <- httr::GET(sprintf("https://api.github.com/repos/rforge/%s", project), gh())
  if(req$status_code == 404){
    payload <- list(
      name = project,
      has_issues = FALSE,
      has_wiki = FALSE,
      has_downloads = FALSE,
      homepage = paste0("https://r-forge.r-project.org/projects/", project),
      description = sprintf("Read-only mirror of \"%s\" from r-forge SVN.", project)
    )
    req <- httr::POST("https://api.github.com/user/repos", body = payload, encode = "json", gh())
    httr::stop_for_status(req)
  }
}

update_repo <- function(project){
  #Sys.setenv(GIT_SSH_COMMAND = "ssh -oStrictHostKeyChecking=no -i ~/rforge/rforge.key")
  olddir <- getwd()
  on.exit(setwd(olddir))
  sys("git", c("svn", "clone", sprintf("svn://svn.r-forge.r-project.org/svnroot/%s", project), project))
  setwd(project)
  sys("git", c("remote", "add", "origin", sprintf("https://rforge:%s@github.com/rforge/%s.git", github_pat(), project)))
  sys("git", c("push", "-f", "origin", "--mirror"))
  setwd("..")
  unlink(project, recursive = TRUE)
}

sync_repos <- function(repos){
  repos <- repos[!(repos %in% blacklist)]
  done_ok <- character()
  done_fail <- character()
  logfile <- file(sprintf("/home/jeroen/rforge/sync-%s.log", as.character(Sys.Date())), open = "at")
  on.exit(close(logfile))
  writeLines(sprintf("START SYNC OF %d AT: %s", length(repos), as.character(Sys.time())), con = logfile)
  lapply(repos, function(project){
    out <- try({
      make_repo(project);
      update_repo(project);
    })
    if(inherits(out, "try-error")){
      done_fail <<- c(done_fail, project)
      writeLines(sprintf("error: %s - %s", project, as.character(out)), con = logfile)
    } else {
      done_ok <<- c(done_ok, project)
      writeLines(sprintf("success: %s", project), con = logfile)
    }
    flush(logfile)
  })
  cat(sprintf("DONE!\n SUCCESS: %s\n FAILED:%s\n\n", 
      paste(done_ok, collapse = ", "), paste(done_fail, collapse = ", ")))
  writeLines(sprintf("DONE AT: %s", as.character(Sys.time())), con = logfile)
}

sync_all <- function(){
  repos <- find_maintained_projects()
  sync_repos(repos)
}

sync_active <- function(){
  repos <- find_active_projects()
  sync_repos(repos)
}

sync_retry <- function(){
  lines <- grep("error:", readLines("sync.log"), value = TRUE)
  repos_error <- sort(unique(substring(sapply(strsplit(lines, " -"), `[[`, 1), 8)))
  lines <- grep("success:", readLines("sync.log"), value = TRUE)
  repos_success <- sort(unique(substring(sapply(strsplit(lines, " -"), `[[`, 1), 10)))
  repos <- repos_error[!(repos_error %in% repos_success)]
  sync_repos(repos)
}

## Cleaning old repos
list_repos <- function(){
  req <- httr::GET("https://api.github.com/users/rforge", gh())
  httr::stop_for_status(req)
  total <- httr::content(req, "parsed")$public_repos
  pages <- ceiling(total / 100)
  repos <- character()
  for(i in seq_len(pages)){
    req <- httr::GET(paste0("https://api.github.com/user/repos?per_page=100&page=", i), gh())
    httr::stop_for_status(req)
    data <- httr::content(req, "parsed", simplifyVector = TRUE)
    names <- gsub("rforge/", "", grep("rforge/", data$full_name, value = TRUE))
    repos <- c(repos, names)
    cat("success at page", i, "\n")
  }
  sort(repos)
}

clean_repos <- function(){
  lapply(list_repos(), function(project){
    req <- httr::GET(sprintf("https://r-forge.r-project.org/projects/%s/", project))
    httr::stop_for_status(req)
    text <- httr::content(req, "text")
    if(grepl("Permission denied. No project was chosen", text)){
      req <- httr::DELETE(sprintf("https://api.github.com/repos/rforge/%s", project), gh())
      httr::stop_for_status(req)
      cat(sprintf("DELETED: %s\n", project))
    } else {
      cat(sprintf("OK: %s\n", project))
    }
  })
  invisible()
}

# Extra: gpg mirror
sync_gpg <- function(){
  req <- httr::GET("https://api.github.com/orgs/gpg/repos", gh())
  httr::stop_for_status(req)
  repos <- httr::content(req, "parsed", simplifyVector = TRUE)$name
  lapply(sort(repos), function(project){
    sys("git", c("clone", "--mirror", sprintf("git://git.gnupg.org/%s.git", project)))
    dirname <- paste0(project, ".git")
    setwd(dirname)
    sys("git", c("remote", "add", "github", sprintf("https://rforge:%s@github.com/gpg/%s.git", github_pat(), project)))
    sys("git", c("push", "--mirror", "github"))
    setwd("..")
    unlink(dirname, recursive = TRUE)
  })
}

##RUN
setwd(tempdir())
sync_gpg()
sync_active()
#sync_all()
#clean_repos()
