#'@title Transformate Data to the \code{recurrent-data} Object
#'@aliases create_recurrent_data
#'@param src data.frame. This is the raw data frame.
#'@param id character value. The column name in \code{src} for \emph{id}.
#'@param time character value. The column name of event time.
#'@param time_type character value. The type of event.
#'@param indicator character value. The column name of the event indicator.
#'@param indicator_value list of named value. \code{list("recurrence" = a, "censoring" = b, "failure" = c)}. Indicates
#'corresponding value in \code{time_type}.
#'@param covariate character value. The covariate of the instances.
#'@param T_0 numeric value. The upper bound of the research interest.
#'@return S4 Object of \code{recurrent-data}
#'@export
#'@examples
#'\dontrun{
#'library(survrec)
#'data(MMC)
#'obj <- create_recurrent_data.data.frame(
#'  MMC, id = "id", time = "time", time_type = "relatively",
#'  indicator = "event", indicator_value = list("recurrence" = 1, "censoring" = 0, "failure" = -1),
#'  covariate = "group"
#')
#'}
create_recurrent_data.data.frame <- function(src, id, time, time_type = c("absolutely", "relatively"),
                                             indicator, indicator_value, covariate, T_0 = NULL) {
  spec <- list(id = id, time = time, time_type = time_type, 
               indicator = indicator, indicator_value = indicator_value, 
               covariate = covariate, T_0 = T_0)
  id.index <- split(seq_len(nrow(src)), src[[spec$id]])
  id.group <- lapply(id.index, function(i) src[i,])
  y <- as.vector(sapply(USE.NAMES=FALSE, id.group, function(df) {
    stopifnot(sapply(id.group, function(df) all(df[-nrow(df), spec$indicator] %in% spec$indicator_value$recurrence)))
    stopifnot(df[nrow(df),spec$indicator] %in% c(spec$indicator_value$censoring, spec$indicator_value$failure))
    switch(
      spec$time_type,
      "absolutely" = {
        stopifnot(df[[spec$time]] >= 0)
        stopifnot(diff(df[[spec$time]]) >= 0)
        df[nrow(df),spec$time]
      },
      "relatively" = {
        stopifnot(df[[spec$time]] >= 0)
        sum(df[[spec$time]])
      },
      stop("Invalid time_type")
    )
  }))
  T_0 <- if(is.null(spec$T_0)) max(y) else spec$T_0
  D <- as.vector(sapply(USE.NAMES = FALSE, id.group, function(df) {
    isTRUE(df[nrow(df), spec$indicator] == spec$indicator_value$failure)
  }))
  t <- lapply(id.group, function(df) {
    switch(
      spec$time_type,
      "absolutely" = {
        head(df[[spec$time]], -1)
      },
      "relatively" = {
        head(cumsum(df[[spec$time]]), -1)
      }
    )
  })
  formula <- as.formula(paste("~", paste(covariate, collapse="+")))
  W <- do.call(rbind, lapply(id.group, function(df) {
    r <- model.matrix(formula, df)
    r[1,-1, drop=FALSE]
  }))
  create_recurrent_data.numeric(y, D, t, T_0, W)
}
