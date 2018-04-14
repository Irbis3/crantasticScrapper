## @knitr server.R
library(shiny)
library(rCharts)
#options(RCHART_WIDTH = 800)
shinyServer(function(input, output) {
  
  output$check <- renderPrint({
    #dat <- getDat()
    dat
  })
  
  output$chart1 <- renderChart({
#     if (!is.null(input$position)) {
#       currentPos <- input$
#     } else currentPos <- "qb"
    currentPos <- input$position
    if (!is.null(input$ppr)){
      if (input$ppr) {
        showPPR <- "Y"
      } else showPPR <- "N"
    } else showPPR <- "N"
    dat1 <- subset(dat, category == currentPos & ppr == showPPR)
    p1 <- rPlot(ave~stddev, data=dat1, type='point',
            tooltip="function(item){return item.player +'\n' + item.matchup +'\n' + item.best +'\n' + item.worst +'\n' + item.category}")
    m <- max(dat1$ave)
    h <- m+.5*m
    p1$addParams(height = 400, dom = 'chart1')
    p1$guides(x = list(title = "Standard Deviation", min=0))
    p1$guides(y = list(title = "Average Rank", max = h, min=-5))
    return(p1)
  })
  
})