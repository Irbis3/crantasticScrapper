library(lubridate)
library(dplyr)
library(readr)
source("functions.R")  # This way we move functions to another file
toy <- read.csv("event_date_id_toyas.csv", stringsAsFactors = FALSE)
results <- toy %>% 
			rowwise() %>% 
			mutate(published_day = day_from_date(article_date), 
    			   text_day = return_days(toyas), 
    		       guessing_date_in_text = day_in_text(article_date, published_day, text_day))

write_csv(results, path = "results.csv") 
