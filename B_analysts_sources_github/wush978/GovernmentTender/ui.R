library(shiny)

graphs <- c(公司地址="graph/Graph_addr", 公司董監事="graph/Graph_dupboss", 投資關係="graph/Graph_comivst")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("公司相似度查詢"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("pkAtmMain", label = "pkAtmMain", value = "50893171"),
      selectInput("graph", label = "關係來源", choices = graphs, selected = graphs[2], multiple = FALSE),
      sliderInput("threshold",
                  "過濾比率(%)",
                  min = 0,
                  max = 100,
                  value = 50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("投標公司資料", dataTableOutput("tendererData")),
        tabPanel("相關公司資料", dataTableOutput("tendererRelatedData")),
        tabPanel("鄰圖", plotOutput("neighborPlot"))
      )
    )
  )
))