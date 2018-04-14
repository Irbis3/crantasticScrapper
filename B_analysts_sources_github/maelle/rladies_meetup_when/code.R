# code for getting meetups from Lucy
doc.raw <- RCurl::getURL("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.md")
meetups <- stringr::str_match_all(doc.raw, "www.meetup.com/(.*?)/")[[1]][,2]
meetups <- unique(meetups)
meetups <- meetups[!meetups %in% c("Spotkania-Entuzjastow-R-Warsaw-R-Users-Group-Meetup",
                                   "Taiwan-R")]

get_event_starts <- function(chapter){
  print(chapter)
  upcoming <- try(meetupr::get_events(chapter,
                                  api_key = Sys.getenv("MEETUP_KEY"),
                                  event_status = "upcoming"),
                  silent = TRUE)
  Sys.sleep(1)
  
  if(is(upcoming, "try-error")){
    upcoming <- try(meetupr::get_events(chapter,
                                        api_key = Sys.getenv("MEETUP_KEY"),
                                        event_status = "upcoming"),
                    silent = TRUE)
    Sys.sleep(1)
  }
  
  past <- try(meetupr::get_events(chapter,
                                  api_key = Sys.getenv("MEETUP_KEY"),
                                  event_status = "past"),
              silent = TRUE)
  Sys.sleep(1)
  
  if(is(past, "try-error")){
    past <- try(meetupr::get_events(chapter,
                                    api_key = Sys.getenv("MEETUP_KEY"),
                                    event_status = "past"),
                silent = TRUE)
    Sys.sleep(1)
  }

  if(is(upcoming, "try-error")){
    events <- past
  }else{
    events <- c(upcoming, past)
  }
  
  if(is(events, "try-error")){
    return(NULL)
  }else{times <- purrr::map_dbl(events, get_time)
  offsets <- purrr::map_dbl(events, get_utc_offset)
  starts <- as.POSIXct(times/1000 + offsets / 1000,
                       origin = "1970-01-01")
  tibble::tibble(start = lubridate::with_tz(starts, "UTC"),
                 chapter = chapter)}
  
  
}


get_time <- function(list){
  return(list$time)
}

get_utc_offset <- function(list){
  return(list$utc_offset)
}

starts <- purrr::map_df(meetups, get_event_starts)

starts <- dplyr::mutate(starts,
                        hour = lubridate::hour(start),
                        day = lubridate::wday(start, label = TRUE, 
                                              abbr = FALSE))
readr::write_csv(starts, path = "starts.csv")
library("ggplot2")
library("hrbrthemes")

ggplot(starts) +
  geom_bar(aes(hour)) +
  facet_grid(day ~ .) +
  theme_ipsum(base_size = 20,
              axis_title_size = 20)
ggsave(file = "hours.png",
       width = 10, height = 10)