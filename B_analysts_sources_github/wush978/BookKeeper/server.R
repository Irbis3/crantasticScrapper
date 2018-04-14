source("init.R")

result_history <- function(input) {
  renderUI({
    tabsetPanel(
      tabPanel(
        title="歷史紀錄",
        HTML(renderTable({
          retval <- get_data(input)
          return(retval)
        })())
      ),
      tabPanel(
        title="花費總結",
        plotOutput("summary-pie-chart")
      )
    )
  })
}
result_insert <- function(input) {
  renderTable({
    query_date <- tryCatch({
      as.Date(input$date)
    }, error = function(e) {
      NA
    })
    if (is.na(query_date)) stop("日期格式必須為yyyy-mm-dd")
    file_name <- get_path(format(query_date, "%Y-%m.csv"), mustWork=FALSE)
    if (is.na(as.integer(input$value))) stop("請於「金額」欄位填入整數")
    if (! "data_type_selector" %in% names(input)) stop("啟動")
    retval <- list("日期"=format(query_date), "類別"=input[["data_type_selector"]], "金額"=as.integer(input$value), "輸出帳戶"=input[["out_account_selector"]], "輸入帳戶" = input[["in_account_selector"]], "手續費" = input$fee, "其他備注" = input$remark)
    retval <- data.frame(retval, check.names=FALSE, stringsAsFactors=FALSE)
    if (file.exists(file_name)) {
      retval <- rbind(retval, read.csv(file=file_name, stringsAsFactors=FALSE))
    }
    write.csv(retval, file=file_name, row.names=FALSE, fileEncoding="utf-8")
    return(retval)
  })
}


shinyServer(function(input, output) {
  output[["data_type"]] <- renderUI({
    list_util("type.csv", "data_type_selector", "類別")
  })
  output[["out_account"]] <- renderUI({
    list_util("account.csv", "out_account_selector", "輸出帳戶")
#     list_account("out_account_selector", "輸出帳戶")
  })
  output[["in_account"]] <- renderUI({
    list_util("account.csv", "in_account_selector", "輸入帳戶")
#     list_account("in_account_selector", "輸入帳戶")
  })
  output[["result"]] <- function() {
    switch(
      input$mode,
      "data" = result_insert(input),
      "statistics" = result_history(input)
    )()
  }
  output[["summary-pie-chart"]] <- renderPlot({
    data <- get_data(input)
    data.aggr <- aggregate(金額~類別, sum, data=data)
    pie(data.aggr[["金額"]], sprintf("%s (%4.2f %%)", data.aggr[["類別"]], 100*data.aggr[["金額"]]/sum(data.aggr[["金額"]])), main="花費比例")
  })
})
