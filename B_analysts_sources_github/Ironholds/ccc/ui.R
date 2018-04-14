library(shiny)
library(markdown)

shinyUI(fluidPage(

  # Application title
  titlePanel("Chaos Crochetting Club"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("rows",
                  "Number of rows:",
                  min = 1,
                  max = 50,
                  value = 2),
      sliderInput("stitches",
                  "Stitches per row:",
                  min = 2,
                  max = 50,
                  value = 10),
      includeMarkdown("content.md"),
      downloadButton('download_data', 'Download')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("knit_table")
    )
  )
))
