shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("paper"),
    h3("StockfishR vs Stockfisher"),
    fluidRow(
      column(
        4,
        sliderInput("depth1", "Depth for white", min = 1, max = 5, value = 2),
        sliderInput("depth2", "Depth for black", min = 1, max = 5, value = 3)
        ),
      column(4, chessboardjsOutput("board"))
      # column(4, verbatimTextOutput("console"))
      )
    )
  )