
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  fluidRow(
    column(3,
           selectInput('species',"species",c('Human','Mouse'))),
    column(3,
           textInput('gene','Gene:')),
    column(1,
           submitButton("Query"))
  ),
  fluidRow(
    column(12,DT::dataTableOutput('dataTable'))
    )
  ))
  