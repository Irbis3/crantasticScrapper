library(magrittr)
library(jsonlite)

dst.path <- function(type = c("device", "history"), date = Sys.Date() - 1) {
  file.path("cache", sprintf("%s-%s.gz", type, format(date)))
}
get.data <- function(type = c("device", "history"), date = Sys.Date() - 1) {
  url.device <- "https://tpairbox.blob.core.windows.net/blobfs/AirBoxDevice.gz"
  url.history <- "https://tpairbox.blob.core.windows.net/blobfs/AirBoxData_history.gz"
  url <- get(sprintf("url.%s", type))
  if (date == Sys.Date() - 1) {
    dst <- dst.path(type, date)
    tryCatch({
      is.update <- difftime(Sys.time(), file.info(dst)$ctime, units = "hours") %>%
        as.numeric() >
        1
      if (is.update) stop("update")
    }, error = function(e) {
      download.file(url = url, destfile = dst.path(type, date))
    })
  }
  dst.path(type, date) %>% 
    gzfile() %>%
    fromJSON() %>%
    extract2(2)
}