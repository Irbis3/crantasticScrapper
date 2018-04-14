# Resubmit a swirl log

library(swirl)
library(base64enc)

if(file.exists("~/.Rprofile")){
  source("~/.Rprofile")
}

swirl_user <- getOption("swirl_user")
sdd <- swirl:::swirl_data_dir()

if(is.null(swirl_user)){
  swirl_user <- select.list(choices = list.dirs(sdd, full.names = FALSE, recursive = FALSE),
              title = "Which of the following is your user name?")
}

user_data_path <- file.path(sdd, swirl_user)
swlogs <- list.files(user_data_path, pattern = "swlog$", full.names = TRUE)
log_choices <- rep(NA, length(swlogs))

for(i in seq_along(swlogs)){
  log_ <- readRDS(swlogs[i])
  course <- log_$course_name
  lesson <- log_$lesson_name
  dt <- file.info(swlogs[i])$mtime
  log_choices[i] <- paste(course, lesson, dt)
}

log_selection <- select.list(log_choices, 
                             title = "Which log would you like to resubmit?")

log_to_submit_path <- swlogs[log_selection == log_choices]

submit_log <- function(){
  
  # Please edit the link below
  pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSem5iBG8baQIXt-rJmgEqSCtaEl8zjoAPuAtOevbCWTrvTKzw/viewform?entry.205631584"
  
  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }
  
  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}
  
  temp <- tempfile()
  log_ <- readRDS(log_to_submit_path)
  nrow_ <- max(unlist(lapply(log_, length)))
  log_tbl <- data.frame(user = rep(log_$user, nrow_),
                        course_name = rep(log_$course_name, nrow_),
                        lesson_name = rep(log_$lesson_name, nrow_),
                        question_number = p(log_$question_number, nrow_, NA),
                        correct = p(log_$correct, nrow_, NA),
                        attempt = p(log_$attempt, nrow_, NA),
                        skipped = p(log_$skipped, nrow_, NA),
                        datetime = p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  browseURL(paste0(pre_fill_link, encoded_log))
}

submit_log()