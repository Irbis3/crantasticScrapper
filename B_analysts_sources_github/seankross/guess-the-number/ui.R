library(shiny)
library(shinyjs)

fluidPage(
  useShinyjs(),
  column(6, offset = 2,
         h2("Please enter a number."),
         p("Wait for submission confirmation."),
         numericInput("guess", "Guess a number between 0 and 10.", value = 5,
                      min = 0, max = 10, step = 1),
         actionButton("action", label = "Submit"),
         hidden(textOutput("response"))
  )
)