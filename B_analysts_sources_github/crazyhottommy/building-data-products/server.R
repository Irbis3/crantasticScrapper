library(shiny)
server <- function(input, output) {

  # load the data and get the data matrix

  library(ISLR)
  ncidat = t(NCI60$data)
  colnames(ncidat) = NCI60$labs
  # use rafalib package to color the clusters
  library(rafalib)
  clusters <- reactive({
    Dmat = dist(t(ncidat))
    hc<- hclust(Dmat,method=input$linkages)
  })

  output$plot1 <- renderPlot({
    par(mar = c(4.1, 3.1, 3.1, 4.1))
    myplclust(clusters(), labels=colnames(ncidat), lab.col=as.fumeric(colnames(ncidat)), main = input$linkages)
    rect.hclust(clusters(), k= input$clusters)
  })

}


