##  server setup
##  - load required libraries / shared objects (e.g., exonsBy(, "tx") ?
library(shiny)
library(Gviz)
library(Homo.sapiens)



# Define server logic required to display gene models
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  output$tracksPlot <- renderPlot({

      model <- models[sym2txname[["ADPRH"]]]
      y <- unlist(model)
      y$group  = rep(letters[seq_along(model)], elementLengths(model))
      model <- relist(y, model)
      track <- AnnotationTrack(model, feature="exon")
      plotTracks(track)

    ## extract the relevant gene model, from input$symbol

    ## query input$bam files for coverage

    ## prepare tracks

    ## use plotTracks() to display 
  })
})
