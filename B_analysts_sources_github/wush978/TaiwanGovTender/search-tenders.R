library(RSelenium)

init <- function() {
  RSelenium::startServer()
  assign("remDr", remoteDriver(remoteServerAddr = "localhost" 
                        , port = 4444
                        , browserName = "firefox"
                        ), envir = globalenv())
  remDr$open()
  .Last <- function() {
    remDr$closeall()
  }
}

init()

search_date <- function(start, end) {
  remDr$navigate("http://web.pcc.gov.tw/tps/pss/tender.do?method=goSearch&searchMode=common&searchType=advance&searchTarget=ATM")
  set_str <- function(id, str, length = 10) {
    ele <- remDr$findElement(using = "id", value = id)
    ele$sendKeysToElement(rep(list("\b"), length))
    ele$sendKeysToElement(list(str))
  }
#   lapply(c("GPA", "ANZTEC", "ASTEP"), function(value) {
#     ele <- remDr$findElement(using = "xpath", value = sprintf("//td/input[@value='%s']", value))
#     if (!ele$isElementSelected()[[1]]) {
#       ele$clickElement()
#     }
#   })
  set_str("awardAnnounceStartDate", start)
  set_str("awardAnnounceEndDate", end)
  submit <- remDr$findElement(using = "id", value = "btnQuery")
  submit$clickElement()
}

search_date("104/11/01", "104/11/30")
i <- 1
write(remDr$getPageSource()[[1]], file = sprintf("search_result/%05d.html", i))
for(i in 2:16948) {
  a <- remDr$findElements(using = "xpath", value = "//td[@class='T11']//a")
  .i <- which(as.character(i) == sapply(a, function(obj) obj$getElementText()))
  a[[.i]]$clickElement()
  Sys.sleep(60)
  write(remDr$getPageSource()[[1]], file = sprintf("search_result/%05d.html", i))
}
