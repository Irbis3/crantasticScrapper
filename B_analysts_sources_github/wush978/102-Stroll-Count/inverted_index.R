inverted_index <- function(x) {
  retval <- list()
  for(i in seq_along(x)) {
    if (!paste(x[i]) %in% names(retval)) {
      retval[[paste(x[i])]] <- c()
    }
    retval[[paste(x[i])]] <- append(retval[[paste(x[i])]], x[i])
  }
  retval
}