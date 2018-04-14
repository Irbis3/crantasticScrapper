library(shiny)
ui <- fluidPage(
  titlePanel('Different linkage for Hierarchical clustering'),
  sidebarLayout(
    sidebarPanel(
      "Hierachical clustering is used to cluster high-dimentional data. There are different linkage
      agglomeration methods to use to cluster samples. This app can check the effects of using different
      linkage method on the clustering. The data are microarray gene expression data for different cancer
      types, choose linkage methods from the drop-down list and select the cluster number, it will give you the dendrogram
      and the clusters.",
      h5("details of the data set", a("Link", href="http://rpubs.com/crazyhottommy/PCA_MDS")),
      selectInput('linkages', 'linkages distance', c("ward.D", "ward.D2", "single", "complete", "average")),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 15),
      submitButton("Submit")
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
)

