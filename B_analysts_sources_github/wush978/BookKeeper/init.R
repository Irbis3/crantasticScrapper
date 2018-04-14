library(shiny)
library(shinyExt)

source("config.R")

get_path <- function(name, ...) {
  normalizePath(sprintf("%s/%s", root_path, name), ...)
}
extra_label <- c("無")

list_util <- function(src_name, id, label) {
  src <- read.csv(get_path(src_name, mustWork=TRUE), stringsAsFactors=FALSE)
  list(
    selectInput(inputId=id, label=label, choices=c(src$Name, extra_label))
  )
}

get_data <- function(input) {
  query_date <- tryCatch({
    as.Date(input$date)
  }, error = function(e) {
    NA
  })
  if (is.na(query_date)) stop("日期格式必須為yyyy-mm-dd")
  file_name <- get_path(format(query_date, "%Y-%m.csv"), mustWork=FALSE)
  if (file.exists(file_name)) {
    retval <- read.csv(file=file_name, stringsAsFactors=FALSE)
  } else {
    stop("沒有本月資料")
  }
}

# list_account <- function(id, label) {
#   account <- read.csv(get_path("account.csv", mustWork=TRUE), stringsAsFactors=FALSE)
#   list(
#     selectInput(inputId=id, label=label, choices=c(account$Name, extra_label)),
#     textInput(inputId=sprintf("%s-extra", id), label=sprintf("%s-備注", label), value="")
#   )
# }
# 
# 
# type <- read.csv(get_path("type.csv", mustWork=TRUE), stringsAsFactors=FALSE)
# list(
#   selectInput(inputId="type", label="類別", choices=c(type$Name, extra_label), multiple=FALSE),
#   conditionalPanel(
#     sprintf("input.type_extra == '%s'", extra_label[1]),
#     textInput("type_extra", label="類別-備注", value="")
#   )
# )
