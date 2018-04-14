# import libraries
library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
    
  ## Tests PiePlot
  output$piePlot <- renderPlot({
    dice <- as.numeric(input$dice)
    obs <- input$obs
    values <- sample(1:dice, obs, replace=TRUE)
    vect <- as.data.frame(table(values))$Freq
    pie(vect, main = paste("Probabilités de" , obs, "lancés de dés"))      
  })
  
  ## Tests BarPlot
  output$barPlot <- renderPlot({
    obs <- input$obs
    dice <- as.numeric(input$dice)
    values <- sample(1:dice, obs, replace=TRUE)
    barVect <- as.data.frame(table(values))$Freq
    names(barVect) <- 1:dice
    
    bar <- barplot(barVect, ylim = c(0, max(barVect) * 1.2),
            main = paste("Probabilités de" , obs, "lancés de dés"),
            xlab="Valeurs",
            ylab="Récurrences")
    text(bar, barVect * 1.1, labels = barVect, col = "black")
  })
  
  outputOptions(output, "piePlot", suspendWhenHidden=FALSE)
  outputOptions(output, "barPlot", suspendWhenHidden=FALSE)
  
})