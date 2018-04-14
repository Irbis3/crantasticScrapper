library(shiny)
#library(shinyIncubator)
library(randomForest)
load("fittedModels.rda")

shinyServer(
    function(input, output) {  
        output$rfPrediction <- renderPrint({
            #if (input$go == 0) return(cat(""))
            #else{
                simplifieddepth <- 1 - (input$codedlesloc == "0")
                newdata <- data.frame(
                            race = factor(input$race, levels = as.character(c(0, 1, 2, 3, 5, 6))),
                            gender = factor(input$gender, levels = c("0", "1")),
                            GCS = input$GCS,
                            TFC = input$TFC,
                             codedinj = factor(input$codedinj, levels = c("0", "1", "3", "4")),
                             dichsurgint = factor(input$dichsurgint, levels = c("0", "1")),
                             dichskullfx = factor(input$dichskullfx, levels = c("0", "1")),
                             dichopen = factor(input$dichopen, levels = c("0", "1")),
                             trichsidelesion = factor(input$trichsidelesion, levels = c("0", "1", "2")),
                             codedlesloc = factor(input$codedlesloc, levels = c("0", "1", "2","3", "4") ),
                             simplifieddepth = simplifieddepth,
                             dichhema = factor(input$dichhema, levels = c("0", "1")),
                             dichpreinjurydx = factor(input$dichpreinjurydx, levels = c("0", "1")),
                             age = input$age)
               isolate(cat( predict(bestFit, newdata = newdata), simplifieddepth ))
            #}
            })
    }
)        
