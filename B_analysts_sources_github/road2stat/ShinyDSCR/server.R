library('shiny')
library('dscr')

source('server-core.R', local = TRUE)

shinyServer(function(input, output, session) {
  
  observe({
    # Change the selected tab.
    # Note that the tabset container must have been created with an 'id' argument
    if ( input$learnmore != 0L ) {
      updateTabsetPanel(session, "mainnavbar", selected = "Help")
    }
  })
  
  observe({
    if ( !is.null(input$dsctitle) & input$submitdscButton != 0L ) {
      updateTabsetPanel(session, "mainnavbar", selected = "Run DSC")
    }
  })
  
  observe({
    if ( !is.null(input$select_dm) & !is.null(input$select_sc) & 
           !is.null(input$select_me) & !is.null(input$select_sf) & 
           input$rundscButton != 0L ) {
      updateTabsetPanel(session, "mainnavbar", selected = "DSC Report")
    }
  })
  
  # R code auto completion
  editor1Ob = aceAutocomplete("editor1")
  editor2Ob = aceAutocomplete("editor2")
  editor3Ob = aceAutocomplete("editor3")
  editor4Ob = aceAutocomplete("editor4")
  observe({
    editor1Ob$resume()
    editor2Ob$resume()
    editor3Ob$resume()
    editor4Ob$resume()
  })
  
  writeDSC = reactive({
    
    if ( !is.null(input$dsctitle) & input$submitdscButton != 0L ) {
      
      # TODO: store all the dsc in databases in the future

      finaltitle = gsub(' ', '_', x = input$dsctitle)
      dscfolder = paste0(paste0(getwd(), '/dscs/'), finaltitle)
      x = dir.create(dscfolder)
      
      write(input$editor1, file = paste0(paste0(dscfolder, '/'), 'datamaker.R'))
      write(input$editor2, file = paste0(paste0(dscfolder, '/'), 'scenarios.R'))
      write(input$editor3, file = paste0(paste0(dscfolder, '/'), 'methods.R'))
      write(input$editor4, file = paste0(paste0(dscfolder, '/'), 'scoring.R'))
      
      return(finaltitle)
      
    }
    
  })
  
  output$select_dm_out = renderUI({
    void = writeDSC()
    selectizeInput("select_dm", "Select Data Maker:", 
                   multiple = TRUE,
                   choices = list.dirs(paste0(getwd(), '/dscs/'), 
                                       full.names = FALSE, recursive = FALSE))
  })
  
  output$select_sc_out = renderUI({
    selectizeInput("select_sc", "Select Scenerios:", 
                   multiple = TRUE,
                   choices = list.dirs(paste0(getwd(), '/dscs/'), 
                                       full.names = FALSE, recursive = FALSE))
  })
  
  output$select_me_out = renderUI({
    selectizeInput("select_me", "Select Methods:", 
                   multiple = TRUE,
                   choices = list.dirs(paste0(getwd(), '/dscs/'), 
                                       full.names = FALSE, recursive = FALSE))
  })
  
  output$select_sf_out = renderUI({
    selectizeInput("select_sf", "Select Scoring Function:", 
                   multiple = TRUE,
                   choices = list.dirs(paste0(getwd(), '/dscs/'), 
                                       full.names = FALSE, recursive = FALSE))
  })
  
  output$pre_dm = reactive({
    if ( is.null(input$select_dm) ) {
      pre_dm_code = ''
    } else {
      pre_dm_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_dm), '/datamaker.R')), collapse = '\n')
    }
    return(pre_dm_code)
  })
  
  output$pre_sc = reactive({
    if ( is.null(input$select_sc) ) {
      pre_sc_code = ''
    } else {
      pre_sc_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_sc), '/scenarios.R')), collapse = '\n')
    }
    return(pre_sc_code)
  })
  
  output$pre_me = reactive({
    if ( is.null(input$select_me) ) {
      pre_me_code = ''
    } else {
      pre_me_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_me), '/methods.R')), collapse = '\n')
    }
    return(pre_me_code)
  })
  
  output$pre_sf = reactive({
    if ( is.null(input$select_sf) ) {
      pre_sf_code = ''
    } else {
      pre_sf_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_sf), '/scoring.R')), collapse = '\n')
    }
    return(pre_sf_code)
  })
  
  evalDSC = reactive({
    
    if ( input$rundscButton != 0L ) {
      
      pre_dm_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_dm), '/datamaker.R')), collapse = '\n')
      pre_sc_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_sc), '/scenarios.R')), collapse = '\n')
      pre_me_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_me), '/methods.R')), collapse = '\n')
      pre_sf_code = paste(readLines(paste0(paste0(paste0(getwd(), '/dscs/'), input$select_sf), '/scoring.R')), collapse = '\n')
      
      eval(parse(text = pre_dm_code))
      eval(parse(text = pre_sc_code))
      eval(parse(text = pre_me_code))
      eval(parse(text = pre_sf_code))
      
      df1 = run_dsc(scenarios, methods, score)
      df2 = aggregate(abs_error ~ method + scenario, df1, mean)
      df3 = aggregate(squared_error ~ method + scenario, df1, mean)
      
    } else {
      df1 = data.frame()
      df2 = data.frame()
      df3 = data.frame()
    }
    
    list('df1' = df1, 'df2' = df2, 'df3' = df3)
    
  })
  
  output$restable1 = renderDataTable({
    evalDSC()$'df1'
  }, options = list(lengthMenu = c(10, 25, 50, 100), 
                    pageLength = 10, 
                    orderClasses = TRUE))
  
  output$downloaddf1CSV = downloadHandler(
    filename = function() { paste('DSC-All-', gsub(' ', '-', gsub(':', '-', Sys.time())), '.csv', sep = '') },
    content = function(file) {
      res1 = evalDSC()$'df1'
      write.table(res1, file, sep = ',', row.names = FALSE, quote = FALSE)
    }
  )
  
  output$downloaddf1XLSX = downloadHandler(
    filename = function() { paste('DSC-All-', gsub(' ', '-', gsub(':', '-', Sys.time())), '.xlsx', sep = '') },
    content = function(file) {
      res1 = evalDSC()$'df1'
      xn.write.xlsx(res1, file, sheetName = "DSC.Report.All", col.names = TRUE, row.names = FALSE)
    }
  )
  
  output$restable2 = renderDataTable({
    evalDSC()$'df2'
  }, options = list(lengthMenu = c(10, 25, 50, 100), 
                    pageLength = 10, 
                    orderClasses = TRUE))
  
  output$downloaddf2CSV = downloadHandler(
    filename = function() { paste('DSC-Absolute-Error-', gsub(' ', '-', gsub(':', '-', Sys.time())), '.csv', sep = '') },
    content = function(file) {
      res2 = evalDSC()$'df2'
      write.table(res2, file, sep = ',', row.names = FALSE, quote = FALSE)
    }
  )
  
  output$downloaddf2XLSX = downloadHandler(
    filename = function() { paste('DSC-Absolute-Error-', gsub(' ', '-', gsub(':', '-', Sys.time())), '.xlsx', sep = '') },
    content = function(file) {
      res2 = evalDSC()$'df2'
      xn.write.xlsx(res2, file, sheetName = "DSC.Report.Absolute.Error", col.names = TRUE, row.names = FALSE)
    }
  )
  
  output$restable3 = renderDataTable({
    evalDSC()$'df3'
  }, options = list(lengthMenu = c(10, 25, 50, 100), 
                    pageLength = 10, 
                    orderClasses = TRUE))
  
  output$downloaddf3CSV = downloadHandler(
    filename = function() { paste('DSC-Squared-Error-', gsub(' ', '-', gsub(':', '-', Sys.time())), '.csv', sep = '') },
    content = function(file) {
      res3 = evalDSC()$'df3'
      write.table(res3, file, sep = ',', row.names = FALSE, quote = FALSE)
    }
  )
  
  output$downloaddf3XLSX = downloadHandler(
    filename = function() { paste('DSC-Squared-Error-', gsub(' ', '-', gsub(':', '-', Sys.time())), '.xlsx', sep = '') },
    content = function(file) {
      res3 = evalDSC()$'df3'
      xn.write.xlsx(res3, file, sheetName = "DSC.Report.Squared.Error", col.names = TRUE, row.names = FALSE)
    }
  )
  
  
  
})

# If knitr would be involved in the future:
# 
#   output$dscreport = renderUI({
#     # input$eval
#     return(isolate(HTML(knit2html(text = paste(as.character(input$dsctitle)), 
#                                   fragment.only = TRUE, quiet = TRUE))))
#   })
