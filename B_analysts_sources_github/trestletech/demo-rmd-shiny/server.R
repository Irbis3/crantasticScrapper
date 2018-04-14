library(shiny)
library(shinyAce)
library(knitr)

#' Define server logic required to generate simple ace editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  output$knitDoc <- renderUI({
    input$eval
    writeLines(input$rmd, "out.Rmd")
    knit2html(input="out.Rmd", fragment.only = TRUE, quiet = TRUE)
    return(isolate(HTML(
      readLines("out.html")
    )))
  })  
})