library('shiny')
library('shinyAce')
library('dscr')

source('ui-core.R', local = TRUE)

shinyUI(navbarPage(title = 'ShinyDSC', 
                   theme = 'cerulean.css', 
                   inverse = TRUE, 
                   fluid = TRUE, 
                   id = 'mainnavbar', 
                   
                   tabPanel(title = 'Home',
                            
                            fluidRow(
                              column(10, offset = 1L,
                                     div(class = 'jumbotron',
                                         h1('ShinyDSC'),
                                         h3('Dynamic statistical comparisons, in your browser'),
                                         p('Click the button to learn how to conduct dynamic statistical comparisons using this web app'),
                                         col.rev.actionButton('learnmore', 'Learn More', icon('search'))
                                     ),
                                     
                                     tags$blockquote("Change the way that our communities compare statistical methods: Reproducible, Extensible, Dynamic. ", 
                                                     tags$br(), 
                                                     "Enhance Scientific Discovery through Statistical Methods Development, Analysis, and Cultural Change.",
                                                     tags$small('Matthew Stephens in Gene Regulation and Dynamic Statistical Comparisons (July 2014)'))
                                     
                              ))
                            
                            
                   ),
                   tabPanel(title = 'New DSC',
                            
                            mainPanel(width = 12L, 
                                      h2(strong('Add New DSC')), 
                                      hr(), 
                                      
                                      sidebarPanel(width = 3L, 
                                                   p("1. Fill the meta data for the DSC"), 
                                                   hr(), 
                                                   
                                                   textInput("dsctitle", "DSC Title:", "Letters and numbers"),
                                                   
                                                   hr(), 
                                                   p("2. Please write the code in the right panel, then click 'Submit DSC'"), 
                                                   hr(), 
                                                   
                                                   col.rev.actionButton(inputId = 'submitdscButton', 
                                                                   label = 'Submit DSC', 
                                                                   icon = icon('upload'))
                                                   
                                      ),
                                      
                                      mainPanel(width = 8L,
                                        
                                        h3("/= Data Maker =/"), 
                                        
                                        aceEditor("editor1", 'datamaker = function(seed, args) {
  
  set.seed(seed)

  nsamp = args$nsamp
  disttype = args$disttype

  # start meat of the function
  if (disttype == "normal") {
    input = list(x = rnorm(nsamp, 0, 1))
    meta  = list(truemean = 0)
  }

  if (disttype == "uniform") {
    input = list(x = runif(nsamp, -1, 1))
    meta  = list(truemean = 0)
  }

  if (disttype == "Cauchy") {
    input = list(x = rt(nsamp,df = 1))
    meta  = list(truemean = 0)
  }
  # end meat of function

  data = list(meta = meta,input = input)

  return(data)
}', mode="r",
                                                  theme = 'tomorrow_night_eighties',
                                                  wordWrap = TRUE, debounce = 10, height = 460, 
                                                  autoComplete = 'live', fontSize = 14),
                                        
                                        h3("/= Scenarios =/"),
                                        
                                        aceEditor("editor2", 'scenarios = list()
scenarios[[1]] = list(name = "normal", fn = datamaker, 
                      args = list(disttype = "normal", nsamp = 1000), 
                      seed = 1:10)
scenarios[[2]] = list(name = "uniform", fn = datamaker, 
                      args = list(disttype = "uniform", nsamp = 1000), 
                      seed = 1:10)
scenarios[[3]] = list(name = "Cauchy", fn = datamaker, 
                      args = list(disttype = "Cauchy", nsamp = 1000), 
                      seed = 1:10)', mode = "r",
                                                  theme = 'tomorrow_night_eighties',
                                                  wordWrap = TRUE, debounce = 10, height = 200, 
                                                  autoComplete = 'live', fontSize = 14),
                                        
                                        h3("/= Methods =/"), 
                                        
                                        aceEditor("editor3", 'mean.wrapper = function(input, args){
  return(list(meanest = mean(input$x)))  
}

median.wrapper = function(input, args){
  return(list(meanest = median(input$x)))    
}

winsor.wrapper = function(input, args) {
  library("psych")
  return(list(meanest = winsor.mean(input$x, trim = 0.2)))
}

methods = list()
methods[[1]] = list(name = "mean", fn = mean.wrapper, args = NULL)
methods[[2]] = list(name = "median", fn = median.wrapper, args = NULL)
methods[[3]] = list(name = "winsor", fn = winsor.wrapper, args = NULL)', mode = "r",
                                                  theme = 'tomorrow_night_eighties',
                                                  wordWrap = TRUE, debounce = 10, height = 240, 
                                                  autoComplete = 'live', fontSize = 14),
                                        
                                        h3("/= Scoring Function =/"), 
                                        
                                        aceEditor("editor4", 'score = function(data, output) {
  return(list(squared_error = (data$meta$truemean-output$meanest)^2, 
              abs_error = abs(data$meta$truemean-output$meanest)))
}', mode = 'r',
                                                  theme = 'tomorrow_night_eighties',
                                                  wordWrap = TRUE, debounce = 10, height = 100, 
                                                  autoComplete = 'live', fontSize = 14)
                                        
                                        )
                            )
                            
                            ),
                   tabPanel(title = 'Run DSC',
                            
                            mainPanel(width = 12L, 
                                      h2(strong('Select DSC to run')), 
                                      hr(), 
                                      
                                      sidebarPanel(width = 3L, 
                                                   uiOutput("select_dm_out"),
                                                   uiOutput("select_sc_out"),
                                                   uiOutput("select_me_out"),
                                                   uiOutput("select_sf_out"),
                                                   
                                                   col.rev.actionButton(inputId = 'rundscButton', 
                                                                   label = 'Run DSC', 
                                                                   icon = icon('flag'))
                                                   
                                                   ),
                                                   
                                                   mainPanel(width = 9L,
                                                             
                                                             h3("/= Data Maker =/"), 
                                                             verbatimTextOutput("pre_dm"),
                                                             h3("/= Scenarios =/"),
                                                             verbatimTextOutput("pre_sc"),
                                                             h3("/= Methods =/"), 
                                                             verbatimTextOutput("pre_me"),
                                                             h3("/= Scoring Function =/"), 
                                                             verbatimTextOutput("pre_sf")
                                                             
                                                             )
                            )
                            
                            
                            
                            ),
                   
                   tabPanel(title = 'DSC Report',
                            
                            mainPanel(width = 12,
                                      h2('Dynamic Statistical Comparison Report'),
                                      hr(),
                                      h3('Results of running all the methods on all the scenarios:'), 
                                      dataTableOutput('restable1'),
                                      hr(),
                                      p('Save the result as an Excel (.xlsx) file or a comma-separated values (.csv) file: '), 
                                      downloadButton('downloaddf1XLSX', 'Download Excel XLSX', class = 'btn btn-success'), 
                                      downloadButton('downloaddf1CSV', 'Download CSV', class = 'btn btn-info'), 
                                      hr(), 
                                      h3('Summary of the results (absolute error):'), 
                                      dataTableOutput('restable2'),
                                      hr(),
                                      p('Save the result as an Excel (.xlsx) file or a comma-separated values (.csv) file: '), 
                                      downloadButton('downloaddf2XLSX', 'Download Excel XLSX', class = 'btn btn-success'), 
                                      downloadButton('downloaddf2CSV', 'Download CSV', class = 'btn btn-info'), 
                                      hr(), 
                                      h3('Summary of the results (squared error):'), 
                                      dataTableOutput('restable3'),
                                      hr(),
                                      p('Save the result as an Excel (.xlsx) file or a comma-separated values (.csv) file: '), 
                                      downloadButton('downloaddf3XLSX', 'Download Excel XLSX', class = 'btn btn-success'), 
                                      downloadButton('downloaddf3CSV', 'Download CSV', class = 'btn btn-info'),
                                      hr()
                            )
                            ),
                   
                   tabPanel(title = 'Help',
                            mainPanel(width = 12L, 
                                      includeMarkdown('help/help.md')
                            ))
                   
))
