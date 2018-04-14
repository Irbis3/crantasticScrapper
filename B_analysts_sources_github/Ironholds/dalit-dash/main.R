# Load dependencies
source("build_titles.R")
source("ores_data.R")
source("page_history.R")

# Get system date
system_run_date <- Sys.Date()

# Check for and include data. If it's not there,
# define everything cleanly
if(!dir.exists("../data/") | !file.exists("../public_html/projects/dalit_data.RData")){
  dir.create("../data/", showWarnings = FALSE)
  quality_data <- data.frame()
  count_data <- data.frame()
} else {
  load(file = "../public_html/projects/dalit_data.RData")
}

# For handling data backlogs
backlog_func <- function(start_date, end_date){
  
  # Work out the range of dates
  dates <- seq(start_date, end_date, by = "day")
  
  # Get the pages
  titles <- get_titles()
  
  # For each date...
  dispose <- lapply(dates, function(date, titles){
    
    # Identify those pages that existed at the time
    existing_pages <- check_existence(titles, date)
    
    # Grab the latest-at-the-time revision for them
    last_revisions <- get_last_revisions(existing_pages, date)
    
    # Calculate ORES scores
    ores_scores <- na.omit(get_quality(edits = last_revisions))
    
    # Calculate count data
    count_data <<- rbind(count_data,
                         data.frame(date = date,
                                    count_data = length(existing_pages)
                        )
    )
    
    # Generate an ORES aggregate
    aggregate_scores <- as.data.frame(table(ores_scores$prediction),
                                      stringsAsFactors = FALSE)
    names(aggregate_scores) <- c("quality_class", "count")
    aggregate_scores$date <- date
    aggregate_scores <- aggregate_scores[,c("date", "quality_class", "count")]
    quality_data <<- rbind(quality_data, aggregate_scores)
    
  }, titles = titles)
  
  # Write out files and quit
  save(quality_data, count_data, file = "../public_html/projects/dalit_data.RData")
  
  return(invisible())
}

# For handling daily runs
day_func <- function(start_date = system_run_date){
  
  # Grab title data
  titles <- get_titles()
  
  # Calculate count data
  count_data <- rbind(count_data,
                      data.frame(date = system_run_date,
                                 count_data = length(titles)
                      )
  )
  
  # Get ORES scores
  ores_scores <- get_quality(titles)
  
  # Generate an aggregate
  aggregate_scores <- as.data.frame(table(ores_scores$prediction),
                                    stringsAsFactors = FALSE)
  names(aggregate_scores) <- c("quality_class", "count")
  aggregate_scores$date <- system_run_date
  aggregate_scores <- aggregate_scores[,c("date", "quality_class", "count")]
  quality_data <- rbind(quality_data, aggregate_scores)
  
  # Write out files and quit
  save(quality_data, count_data, file = "../public_html/projects/dalit_data.RData")
}

