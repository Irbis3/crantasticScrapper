library(shiny)
source("modules.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dynamicSelectOutput("test1", label = "Dynamic Select")
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  the_data <- reactive({
    mtcars
  })
  
  choice <- shiny::callModule(dynamicSelect, "test1",
                              dataframe = isolate(the_data()),
                              col.choice = "gear",
                              external_session = session)
  
  output$table <- renderDataTable({
    shiny::validate(
      need(choice(),"Choice output"),
      need(the_data(), "The data"),
      need(input$test1, "Need selection")
    )
    dd <- the_data()
    choice <- input$test1
    
    browser()
    
    dd[dd$gear == choice,]
    
  })
}

shinyApp(ui, server)
