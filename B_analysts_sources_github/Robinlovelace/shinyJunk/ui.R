library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(

  #  Application title
  titlePanel("Sliders"),

  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(

      sliderInput(min = as.Date(strptime("2014-04-19", format = "%Y-%m-%d")),
                  max = as.Date(strptime("2015-10-03", format = "%Y-%m-%d")),
                  value = c(as.Date(strptime("2015-07-27", format = "%Y-%m-%d")),
                            as.Date(strptime("2015-10-03", format = "%Y-%m-%d"))),
                  inputId = "date", label = "Date")

    ),

    # Show a table summarizing the values entered
    mainPanel(
      plotOutput("values")
    )
  )
))