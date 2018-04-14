
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Jeff Leek Chat Bot"),
  mainPanel(
          verbatimTextOutput("answer"),
          textInput(inputId = "text",
                    label = h3("Talk to the Bot:")),
          submitButton("Send chat!", icon("refresh"))
  )
))
