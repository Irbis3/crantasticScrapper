
library(shiny)
library(RSQLite)
library(shinyvalidinp)

shinyServer(function(input, output) {

  con <- dbConnect(SQLite(), ":memory:") 
  states <- data.frame(abbr = state.abb, name = state.name)
  dbWriteTable(con, "states", states)
    
  output$outp_unsafe <- renderTable({
      abbr <- input$inp_abbr
      dbGetQuery(con, paste0("select * from states where abbr = '",abbr,"'"))
  })

  output$outp_safe <- renderTable({
      abbr <- validinp_character(input$inp_abbr, pattern="^((CA)|(NY))$")
      dbGetQuery(con, paste0("select * from states where abbr = '",abbr,"'"))
  })
  
})
