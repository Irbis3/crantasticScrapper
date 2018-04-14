library(shiny)
library(DT)
library(nycflights13)
data(flights)

shinyServer(function(input, output, session) {
  action = dataTableAjax(session, flights)
  widget = datatable(flights, server = TRUE, 
  extensions = c('Scroller','Responsive'), options = list(
    deferRender = TRUE,
    dom = "frtiS",
    scrollY = 500,
    scrollCollapse = TRUE,
    ajax = list(url = action)
  ),
  filter="bottom")
  output$tbl = DT::renderDataTable(widget)
})
