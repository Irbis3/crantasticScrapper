library(shiny)

# http://shiny.rstudio.com/gallery/tabsets.html
 
# Define server logic for random distribution application
shinyServer(function(input, output) {
        
        # Reactive expression to generate the requested distribution.
        # This is called whenever the inputs change. The output
        # functions defined below then all use the value computed from
        # this expression
        thedata <- reactive({
                # display variable importance on a +/- scale 
                results <- results_global[order(results_global$Weight),]
                #results <- results[(results$Weight != 0),]
                results <- results[results$Weigh <= input$posimp,]
                results <- results[results$Weigh >= input$negimp,]
                return (results)
        })
        
        # Generate a plot of the data. Also uses the inputs to build
        # the plot label. Note that the dependencies on both the inputs
        # and the data reactive expression are both tracked, and
        # all expressions are called in the sequence implied by the
        # dependency graph
        output$plot <- renderPlot({
                theresults <- thedata()
                xx <- barplot(theresults$Weight, width = 0.85, 
                              main = paste("Variable Importance - Data Set: Titanic"), horiz = T, 
                              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
                              col = ifelse((theresults$Weight > 0), 'blue', 'red')) 
                axis(2, at=xx, labels=theresults$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  
                  
        })
        
        # Generate a summary of the data
        output$summary <- renderPrint({
                print('Data Set Scaled Values:')
                print(paste('Positive Cutoff:', input$posimp))
                print(paste('Negative Cutoff:', input$negimp))
                print('----------------------------')
                print(thedata())
                 
        })

      
})