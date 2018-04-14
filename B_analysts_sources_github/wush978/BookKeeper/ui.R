source("init.R")
submit_button <- submitButton(text="送出")

shinyUI({
  pageWithSidebar(
    headerPanel("R 記帳本"),
    sidebarPanel(
      tabsetPanel(
        id = "mode",
        tabPanel(
          title="輸入資料", 
          value="data",
          datePicker("date", label="日期", default=format(Sys.Date()), format="yyyy-mm-dd"),
          htmlOutput("data_type"),
          textInput("value", label="金額", value=""),
          htmlOutput("out_account"),
          htmlOutput("in_account"),
          textInput("fee", label="手續費", value="0"),
          textInput("remark", label="其他備注", value="無"),
          submit_button
        ),
        tabPanel(
          title="統計",
          value="statistics",
          submit_button
        )
      )
    ),
    mainPanel(
      htmlOutput("result")
    )
  )
})