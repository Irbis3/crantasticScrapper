source("parse.R")
src <- src_sqlite("airbox.db", create = TRUE)

date.list <- 
  airbox.history %>%
  select(date) %>%
  distinct() %>%
  extract2("date") %>%
  head(-1) %>%
  tail(-1)

for(i in seq_along(date.list)) {
  date.target <- date.list[i]
  tb <- format(date.target, "airbox%Y%m%d")
  tryCatch({
    if (!db_has_table(src$con, tb)) {
      df <- filter(airbox.history, date == date.target) %>%
        left_join(airbox.device)
      stopifnot(is.na(df$gps_alt) %>% sum() == 0)
      copy_to(src, df, tb, temporary = FALSE)
    }
  }, error = function(e) {
    message(sprintf("During processing data with date: %s", date.target))
    message(conditionMessage(e))
  }, finally = {
  })
}
