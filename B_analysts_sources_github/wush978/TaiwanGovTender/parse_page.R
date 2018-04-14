library(optparse)
opt <- parse_args(OptionParser(option_list = list(
  make_option("--month", type = "character", default = format(Sys.Date(), "%Y-%m"))
)))
library(lubridate)
start <- as.Date(sprintf("%s-01", opt$month))
end <- local({
  tmp <- start
  month(tmp) <- month(tmp) + 1
  tmp - 1
})
if (end > Sys.Date()) end <- Sys.Date()
post_format <- function(date) {
  year(date) <- year(date) - 1911
  sprintf("%d/%02d/%02d", year(date), month(date), day(date))
}
library(httr)
library(magrittr)
library(XML)
source("post_obj.R")
post_obj$awardAnnounceStartDate <- post_format(start)
post_obj$awardAnnounceEndDate <- post_format(end)
res <- POST(url = "http://web.pcc.gov.tw/tps/pss/tender.do?searchMode=common&searchType=advance",
            body = post_obj,
            encode = "form",
            verbose())
stop_for_status(res)
doc <- content(res, "parsed")
trs <- XML::getNodeSet(doc, "//div[@id='print_area']//tr")

last_tr <- tail(trs, 1)[[1]]
last_index <- which(last_tr[["td"]] %>%
  xmlChildren() %>%
  lapply(xmlValue) == "最後一頁")
last_a <- last_tr[["td"]][[last_index]]
stopifnot(xmlValue(last_a) == "最後一頁")
last_href <- xmlAttrs(last_a)["href"]
last_page_index <- local({
  m <- regexec("pageIndex=(\\d+)", last_href)
  regmatches(last_href, m)[[1]][2]
}) %>% as.integer

target_href <- file.path("http://web.pcc.gov.tw/tps/pss", 
  sapply(sprintf("pageIndex=%d", 2:last_page_index), 
       gsub, pattern = "pageIndex=\\d+$",
       x = last_href) %>%
    sapply(function(x) substring(x, 3, nchar(x)))
)

retval <- vector(mode = "list", length = last_page_index)

dst <- format(start, "%Y-%m")
if (!dir.exists(dst)) dir.create(dst)

retval[[1]] <- lapply(trs, function(node) {
  a <- node[[10]][["a"]]
  if (is.null(a)) NULL else {
    data.frame(stringsAsFactors = FALSE,
      href = xmlAttrs(a, "href"),
      name = xmlValue(a[["u"]])
    )
  }
}) %>%
  Filter(f = function(x) !is.null(x)) %>%
  do.call(what = rbind)

dump_csv <- function(df, dst, index) {
  dst.path <- file.path(dst, sprintf("%05d-page.csv.gz", index))
  write.csv(df, gzfile(tmp.path <- tempfile(tmpdir = dst)),
    quote = TRUE, row.names = FALSE)
  file.rename(tmp.path, dst.path)
}

dump_csv(retval[[1]], dst, 1)

library(logging)
basicConfig()
for(i in seq_along(target_href)) {
  loginfo(sprintf("Parsing page index: %d", i + 1))
  res <- GET(target_href[i])
  stop_for_status(res)
  doc <- content(res, "parsed")
  trs <- XML::getNodeSet(doc, "//div[@id='print_area']//tr")
  retval[[i+1]] <- lapply(trs, function(node) {
    a <- node[[10]][["a"]]
    if (is.null(a)) NULL else {
      data.frame(stringsAsFactors = FALSE,
        href = xmlAttrs(a, "href"),
        name = xmlValue(a[["u"]])
      )
    }
  }) %>%
    Filter(f = function(x) !is.null(x)) %>%
    do.call(what = rbind)
  dump_csv(retval[[i + 1]], dst, i + 1)
  Sys.sleep(rpois(1, lambda = 10) + 1)
}
