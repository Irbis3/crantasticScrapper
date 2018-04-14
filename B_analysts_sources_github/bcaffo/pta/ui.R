# Define UI for PTA prediction
shinyUI(
    pageWithSidebar(  
        # Application title
        headerPanel("Emergence from pediatric post traumatic amnesia calculator"),
        sidebarPanel(
          h4("Enter subject characteristics"),
           selectInput("race", "Race",
                       list( 
                            "White 1" = "1", 
                            "Black 2" = "2")
                       ),
            selectInput("gender", "Gender",
                       list("Female = 0 " = "0", 
                            "Male = 1" = "1")
                       ),
            numericInput("age", "Age in years", 11, min = 3, max = 19, step = .1),
            numericInput("GCS", "Initial Glasgow Coma Scale", 5, min = 3, max = 15, step = 1),
            numericInput("TFC", "Time to follow a motor command twice in 24 hours (please input days)", 9, min = 0, max = 130, step = 1),
            selectInput("codedinj", "Mechanism of injury",
                            list(
                                     "Pedestrian struck by motor vehicle" =  "0",
                                     "Driver or passenger in MVC" =  "1",
                                     "What is 3 ?" = "3",
                                     "Fall" = "4"
                            )
                        ),
            selectInput("dichsurgint", "Required urgent neuro-surgical intervention (placement of ICP monitor alone does not count)",
                        list(
                                "No" = "0",
                                "Yes" = "1"
                            )
                ),
            selectInput("dichskullfx", "Skull fracture",
                        list(
                                "No" = "0",
                                "Yes" = "1"
                            )
                        ),
            selectInput("dichopen", "Penetrating traumatic brain injury (at minimum with a dural tear present)",
                        list(
                                "No" = "0",
                                "Yes" = "1"
                            )
                        ),
           selectInput("trichsidelesion", "Lesion location",
                        list(
                                "Right hemisphere" = "0",
                                "Left hemisphere" = "1",
                                "Bilateral" = "2"
                            )
                        ),
            selectInput("codedlesloc", "Depth of deepest lesion",
                        list(
                                "Frontotemporal cortex" = "0",
                                "Corpus callosum" = "1",
                                "Basal ganglia" = "2",
                                "Thalamus" = "3",
                                "Cerebellum / brainstem" = "4"
                            )
                        ),
#            selectInput("simplifieddepth", "Simplied depth - depth of deepest lesion",
#                        list(
#                                "Cortical only" = "0",
#                                "Corpus callosum, basal ganglia, thalamus or cerebellum / brainstem" = "1"
#                            )
#                        ),
            selectInput("dichhema", "Presence or absence of intracranial bleed",
                        list(
                                "No" = "0",
                                "Yes" = "1"
                            )
                        ),
            selectInput("dichpreinjurydx", "Presence or absence of pre-injury learning, behavior, or developmental concerns",
                        list(
                                "No" = "0",
                                "Yes" = "1"
                            )
                        )
        ),
        mainPanel(
            #actionButton("go", "Click here to get the prediction"),
            h3('Estimated days from the day of first command following to emergence from post traumatic amnesia'),
            verbatimTextOutput("rfPrediction")
          )
    )
)