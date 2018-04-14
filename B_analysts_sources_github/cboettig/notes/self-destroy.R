library("analogsea")

#' Clone REPO, cd into REPO/PATH and runs Rscript SCRIPT, then pushes results 
#' back up to the remote repository.
#' 
#' @param REPO the name of a Github repository
#' @param PATH a path in the repo to where the SCRIPT should be run, e.g. 
#'  "inst/examples" NB: path should not start with a leading '/'.
#' @param SCRIPT a character vector with the argument to RScript, e.g.
#'  the name of a .R file found in REPO/PATH
#' @param USER the Github username. By default will guess the System 
#'  environmental variable  value for USER.
#' @param GH_TOKEN the authentication token or user password (The former
#'  is required if 2-factor auth is on for Github)
#' @param EMAIL the email address for the git account. Does not have
#'  to be a valid email, be default will use USER(at)USER.com
#' @param IMG the name of the Docker image used to run the commands.
#'   the image must have git and R installed. The default is good 
#    for most uses.
#' @return the string for a command to `docklet_run`

task <- function(REPO, PATH, SCRIPT, USER = Sys.getenv("USER"), 
                 GH_TOKEN = Sys.getenv("GH_TOKEN"), 
                 EMAIL = paste0(USER, "@", USER, ".com"),
                 IMG = "rocker/hadleyverse"){
  paste(
  paste0("-it ", IMG, " bash -c \"", "git config --global user.name ", USER),
  paste0("git config --global user.email ", EMAIL),
  paste0("git clone ", "https://", USER, ":", GH_TOKEN, "@github.com/", USER, "/", REPO, ".git"),
  paste0("cd ", REPO),
  paste0("cd ", PATH),
  paste0("Rscript ", SCRIPT),
  "git add -A",
  "git commit -a -m 'runs from droplet'",
  "git push origin master",
  "\"",
  sep="; ")
}


# A local test
#system(paste("docker run --rm cboettig/strata", task("template")))

img <- "cboettig/strata"
tsk <- task("template", "inst/examples", "knit.R", IMG=img)

docklet_create() %>%
  docklet_run(tsk) %>%
  droplet_delete()


d[[2]] %>%  
  docklet_run(tsk) %>%
  droplet_delete()


