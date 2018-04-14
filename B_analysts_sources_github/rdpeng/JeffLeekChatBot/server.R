
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(httr)
library(JeffLeekMeme)
library(jsonlite)

chat <- function(input) {
        encoded <- URLencode(input)
        api <- paste0("https://api.wit.ai/message?v=20170420&q=",
                      encoded)
        output <- GET(api, add_headers(Authorization = server_token))
        outputList <- suppressMessages(fromJSON(as.character(output)))
        if(length(outputList$entities) == 0L)
                return(list(text = "Sorry, I didn't understand you"))
        if(outputList$entities$intent["confidence"] < 0.8)
                return(list(text = "Sorry, I didn't understand you"))
        result <- outputList$entities$intent["value"]
        txt <- if(result == "fact")
                capture.output(JeffLeek())
        else if(result == "weakness")
                "Sorry, Jeff Leek has no weaknesses"
        invisible(list(text = txt, output = output))
}


server_token <- "Bearer [INSERT SERVER TOKEN HERE]"

function(input, output) {
        convo <- NULL
        returnChat <- reactive({
                if(nchar(input$text) == 0L)
                        NULL
                else {
                        convo <<- c(convo, paste("YOU:", input$text))
                        out <- chat(input$text)
                        convo <<- c(convo, paste("BOT:", out$text))
                        paste(convo, collapse = "\n")
                }
        })
        output$answer <- renderText({
                returnChat()
        })
}



