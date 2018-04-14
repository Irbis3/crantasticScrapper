suppressPackageStartupMessages({
  library(pbdMPI)
  library(logging)
  source("bparser.R")
  library(pbdMPILogging)
  basicConfig()
  convert_to_parallel_logger(getLogger())
})
.rank <- comm.rank()
.size <- comm.size()
# if (.rank == 0) {
#   all_dir <- dir("tenders", "gz$", recursive = TRUE, full.names = TRUE)
#   # all_dir <- head(all_dir, 50)
#   loginfo(sprintf("There are total %d files to process...", length(all_dir)))
#   invisible(bcast(all_dir))
# } else {
#   all_dir <- bcast(character(0))
#   loginfo(sprintf("Receiving total %d files to process...", length(all_dir)))
# }
all_dir <- readRDS("all_dir.Rds")
jid <- pbdMPI::get.jid(length(all_dir))
loginfo(sprintf("I have %d files to process", length(jid)))
url_format <- "http://web.pcc.gov.tw/tps/main/pms/tps/atm/atmAwardAction.do?newEdit=false&searchMode=common&method=inquiryForPublic&pkAtmMain=%s&tenderCaseNo=%s&contentMode=%d"
pattern <- "tenders/(.*)/([^/]+).html.gz$"
retval.path <- sprintf("parallel_parse_tenders-%03d.Rds", .rank)
if (file.exists(retval.path)) {
  retval <- readRDS(retval.path)
} else {
  retval <- list()
}
barrier()
save_progress <- function() {
  tmp.path <- sprintf("%s.tmp", retval.path)
  saveRDS(globalenv()$retval, tmp.path)
  file.rename(tmp.path, retval.path)
}
options(error = function() {
  save_progress()
})
for(.i in seq_along(jid)) {
  d <- all_dir[jid[.i]]
  if (!is.null(retval[[d]])) next
  is.retry <- FALSE
  is.done <- FALSE
  while (!is.done) {
    .element <- NULL
    tryCatch({
      .element <- tryCatch({
        get_content(d)
      }, error = function(e) {
        logwarn(sprintf("Using default get_content to parse %s encounter: %s", d, conditionMessage(e)))
        get_content(d, TRUE)
      })
      is.done <- TRUE
      next
    }, error = function(e) {
      logerror(sprintf("Error is encoutered when I am processing %s ...", d))
      logerror(sprintf("The error message is: %s", conditionMessage(e)))
      if (is.retry) {
        quit("no", status = 1)
      }
      is.retry <<- TRUE
      { # retry
        get_param <- local({
          tmp <- regmatches(d, regexec(pattern, d))[[1]]
          list(pkAtmMain = gsub("/", "", tmp[2]), tenderCaseNo = tmp[3], content_mode = 0L)
        })
        is.request_for_get_complete <- TRUE
        while(is.request_for_get_complete) {
          url <- sprintf(url_format, get_param$pkAtmMain, get_param$tenderCaseNo, get_param$content_mode)
          logwarn(sprintf("Retrying download page from %s...", url))
          res <- httr::GET(url)
          if (res$status_code == 500) {
            Sys.sleep(rpois(1, 5))
            next
          }
          stop_for_status(res)
    #       writeBin(content(res, "raw"), .tmp.html <- tempfile(fileext=".html"))
    #       browseURL(.tmp.html)
          html_text <- content(res, as = "text")
          if (grepl("請使用\"顯示完整資料\"察看詳細資料。", html_text, fixed = TRUE)) {
            get_param$content_mode <- get_param$content_mode + 1L
            next
          }
          .dst.html <- gsub(".gz", "", d, fixed = TRUE)
          .tmp.html <- sprintf("%s.tmp.html", .dst.html)
          writeBin(content(res, "raw"), .tmp.html)
          if (interactive()) browseURL(.tmp.html)
          .element <<- tryCatch({
            get_content(.tmp.html)
          }, error = function(e) {
            logwarn(sprintf("Using default get_content to parse %s encounter: %s", d, conditionMessage(e)))
            tryCatch({
              get_content(.tmp.html, TRUE)
            }, error = function(e) {
              logerror(sprintf("Processing %s is failed to recover from error: %s", d, conditionMessage(e)))
              stop(conditionMessage(e))
            })
          })
          file.rename(.tmp.html, .dst.html)
          is.request_for_get_complete <- FALSE
        }
        is.done <<- TRUE
      }
    })
    if (!is.done) loginfo(sprintf("Trying to process %s again...", d))
  }
  stopifnot(!is.null(.element))
  retval[[d]] <- .element
  if (.i %% 100 == 0) {
    loginfo(sprintf("Progress: (%d/%d)", .i, length(jid)))
    save_progress()
  }
}
save_progress()
pbdMPI::finalize()
