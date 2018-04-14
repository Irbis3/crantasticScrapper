# This function takes a formatted date as Y-m-d and returns weekday
day_from_date <- function(adate = NULL) {
    if (!is.null(adate)) {
        day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
            "Saturday")
        which(grepl(weekdays(as.Date(adate, "%Y-%m-%d")), day_list))
    }
}

# This function takes some text and returns the position on the weekday list
# So return_days("Monday Tuesday Friday")
# will return 2 (for Monday)
return_days <- function(text) {
    if(is.character(text)) {
    day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
        "Saturday")
    res <- as.numeric(which(sapply(day_list, grepl, text, ignore.case = TRUE)))
    if (length(res) == 0) {
        NULL
    } else {
        res[1] # we can add more handling here to deal with multiple weekday mentions
    }
 }    
}

# Function takes weekday difference between article_date and text_date and
# calculates approximate date.
day_in_text <- function(article_day, aday, pdate) {
    date_diff <- aday - pdate
    if (date_diff > 0) {
        as.Date(article_day, "%Y-%m-%d") - date_diff
    } else {
        dd <- ifelse(date_diff != 0, date_diff + 7, 0)
        as.Date(article_day, "%Y-%m-%d") - dd
    }
}
