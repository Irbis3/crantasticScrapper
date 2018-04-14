library(shiny)

legtype <- function(seatvector, w = .50) {
    ## function to take a vector of seats and to return the legislative type
    ## argument w is a proportion for the legislative decision rule
    # drop any empty or missing seats from the vector
    seatvector <- seatvector[seatvector>0 & !is.na(seatvector)]
    # test if the supplied argument is all integers
    S <- seatvector
    storage.mode(S) <- "integer"
    if (sum(S != seatvector))
        stop("Can only supply integers as seat vectors.")
    # sort in decreasing seat size
    S <- sort(S, decreasing=TRUE)
    totalseats <- as.integer(sum(S))
    totalparties <- as.integer(length(S))
    W <- as.integer((totalseats * w) + 1)
    legtype <- 
        ifelse(S[1] >= W, "A",
               ifelse((S[1] + S[3] >= W) & (S[2] + S[3] < W), "B",
                      ifelse(S[2] + S[3] >= W, "C",
                             ifelse((S[1] + S[2] >= W) & (S[1] + S[3] < W), "D",
                                    ifelse(S[1] + S[2] < W, "E",
                                           NA)))))
    if (legtype=="B") {
        if (S[1] + S[totalparties] >= W) legtype <- "B*"
    }
    names(S) <- paste("S", 1:totalparties, sep="")   # name the seat vector
    # return(list(legtype=legtype, totalseats=totalseats, W=W, S=S))
    return(legtype)
}

ui = shinyUI(pageWithSidebar(
    headerPanel("Legislative Type Calculator"),
    sidebarPanel(
        numericInput(inputId = "w", label = "Winning threshold", 0.5),
        textInput(inputId = "seats", label = "Input seats, separated by commas",
                  "50, 45, 5"),
        submitButton(text = "Enter")
    ),
    mainPanel(
        textOutput("totalseats"),
        h4(textOutput("computedtype"))
    )
))


server = shinyServer(function(input, output) {

    seatvec <- reactive({
        as.numeric(read.csv(textConnection(input$seats), header = FALSE),
                   w = input$w)
    })
    
#    sumseats <- reactive(sum(seatvec()))

    output$totalseats <- renderText(
        paste("Total seats: ", sum(seatvec()))
    )
    output$computedtype <- renderText({
        paste("Legislative Type:", legtype(seatvec()))
    })
    
})

shinyApp(ui, server)
