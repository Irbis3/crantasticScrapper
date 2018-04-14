shinyServer(function(input, output, session) {
  
  logjs("in session")

  chss <- Chess$new()
  console <<- ""
  autoInvalidate <- reactiveTimer(2000)
  
  observe({
    autoInvalidate()
    
    logjs(.Platform$OS.type)
    
    handle <- spawn_process(ifelse(.Platform$OS.type == "unix", "/usr/games/stockfish", "stockfish_8_x64.exe"))
    logjs("handle")
    
    process_write(handle, sprintf("position fen %s\n", chss$fen()))
    logjs(process_read(handle, PIPE_STDOUT, timeout = TIMEOUT_INFINITE))
    
    process_write(handle, sprintf("go depth %s\n", ifelse(chss$turn() == "w", input$depth1, input$depth2)))
    while(TRUE) {
      message("loop")
      out <- process_read(handle, PIPE_STDOUT, timeout = 1000)
      console <<- out
      print(last(out))
      if(any(str_detect(out, "bestmove"))) break()
      
    }
    
    logjs("out")
    logjs(out)
    
    process_kill(handle)
    
    mv <- str_split(last(out), " ")[[1]][[2]]
    
    logjs("mv")
    logjs(mv)
    
    mvsan <- chss$moves(verbose = TRUE) %>% 
      mutate(ft = paste0(from, to)) %>% 
      filter(ft == mv) %>% 
      {.$san}
    
    chss$move(move = mvsan)
    
  })
  
  output$board <- renderChessboardjs({
    autoInvalidate()
    chss$plot()
  })
  
  output$console <- renderPrint({
    # autoInvalidate()
    console
  })
  
})
