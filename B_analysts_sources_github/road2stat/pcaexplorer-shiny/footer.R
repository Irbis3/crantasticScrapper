footer <- function() {
  tags$div(
    class = "footer",
    style = "text-align:center",
    tags$div(
      class = "foot-inner",
      list(
        hr(),
        "pcaExplorer is a project developed by Federico Marini in the Bioinformatics division of the ",
        tags$a(href = "http://www.unimedizin-mainz.de/imbei", "IMBEI"),
        br(),
        "Development of the pcaExplorer package is on ",
        tags$a(href = "https://github.com/federicomarini/pcaExplorer", "GitHub"),
        br(),
        "Deployable app is on",
        tags$a(href = "https://github.com/road2stat/pcaexplorer-shiny", "GitHub")
      )
    )
  )
}
