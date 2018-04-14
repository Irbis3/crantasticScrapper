library(shiny)

shinyServer(function(input, output) {
  
  output$knit_table <- renderDataTable({
    
    # Generate the actual data
    value_length <- input$rows*input$stitches
    values <- sample(c("k", "p"), size = value_length, replace = TRUE)
    
    # Turn it into a data.frame
    knit_sequence <<- data.frame(matrix(data = values, nrow = input$rows, ncol = input$stitches), stringsAsFactors = FALSE)
    
    # Done!
    return(knit_sequence)}, list(paging = FALSE, searching = FALSE, sort = FALSE)
  )
  
  output$download_data <- downloadHandler(
    filename = tempfile(fileext="csv"),
    content = function(file){write.csv(knit_sequence, file)}
  )

})
