# require(shiny)
# folder_address = '/Volumes/NO NAME/classes/Hopkins/Developing Data Products'
# runApp(folder_address, launch.browser=TRUE)

library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Using Summary Function to Calculate Variable Importance"),
        
        # Sidebar with controls to select the random distribution type
        # and number of observations to generate. Note the use of the
        # br() element to introduce extra vertical spacing
        sidebarLayout(
                sidebarPanel(
                        sliderInput("posimp", 
                                    "Positive Importance Cutoff:", 
                                    value = max(results_global$Weight),
                                    min = 0, 
                                    max = max(results_global$Weight)),
                        
                        br(),
                        sliderInput("negimp", 
                                    "Negative Importance Cutoff:", 
                                    value = min(results_global$Weight),
                                    min = min(results_global$Weight),
                                    max = 0)
                ),
                
                # Show a tabset that includes a plot, summary, and table view
                # of the generated distribution
                mainPanel(
                        tabsetPanel(type = "tabs", 
                                    tabPanel("Plot", plotOutput("plot")), 
                                    tabPanel("Data", verbatimTextOutput("summary"))
                        )
                )
        )
))