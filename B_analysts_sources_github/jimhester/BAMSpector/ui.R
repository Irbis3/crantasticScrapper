library(shiny)
library(Homo.sapiens)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("BAMSpector: Reads Supporting Gene Models"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins", "Width of bin:",
                   min = 1, max = 10000, value = 30),

      ## input gene symbol (fancy: select from available)
       selectInput("symbol", "Gene Symbol", choices = keys(Homo.sapiens, keytype='SYMBOL')),


      ## input paths to local BAM files
       textInput("bam", "Bam Files")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("tracksPlot")
    )
  )
))
