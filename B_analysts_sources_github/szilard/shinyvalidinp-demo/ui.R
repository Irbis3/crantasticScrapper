
library(shiny)

shinyUI(fluidPage(
  titlePanel("Shiny input validation - demo"),
  helpText("This shows the difference between validating inputs or not in a Shiny",
           "app that queries a SQL database. Assume the user is supposed to access",
           "data for 2 input values (CA/NY) only. However, it is easy to manipulate the",
           "browser (e.g. with Chrome developer tools) so that it send any input,",
           "for example the 3rd option in the dropdown below (even if that is not",
           "included in the dropdown!). See what happens for",
           "various inputs in a application that does not validate inputs (left below)",
           "vs an application that does (right below)."),
  selectInput("inp_abbr", "Choose input:", choices=c("CA","NY","' or 1=1 --")),
  hr(),
  fluidRow(
    column(4, helpText("Output without input validation (vulnerable to SQL injection):")),
    column(4, helpText("Output with input validation",
                       "(safe, it gives error if input not valid):"))
  ),    
  fluidRow(
      column(4, tableOutput("outp_unsafe")),
      column(4, tableOutput("outp_safe"))
  )      
))
