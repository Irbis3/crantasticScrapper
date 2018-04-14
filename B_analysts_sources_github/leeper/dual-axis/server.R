library("shiny")

shinyServer(function(input, output) {
    set.seed(1)
    x <- seq(0, 50, by = 1L)
    y1 <- numeric(length(x))
    y2 <- numeric(length(x))
    y1[1L] <- rnorm(1, 80)
    y2[1L] <- rnorm(1, 120)
    for (i in seq_along(x)[-1L]) {
        y1[i] <- y1[i-1] + rnorm(1, 0, 10)
        y2[i] <- y2[i-1] + rnorm(1, 0, 10)
    }

    output$plot <- renderPlot({
        par(mar=c(2,2,2,2))
        slope <- diff(c(input$left[2L], input$left[1L]))/diff(c(input$right[2L], input$right[1L]))
        
        # first line
        plot(x, y1, type = "l", col = "red", ylim = c(input$left[1L], input$left[2L]), ylab = "", yaxt = "n", bty = "n")
        
        # second line
        lines(x, input$left[1L] + (y2-input$right[1L])*slope, col = "blue")
        
        # axes
        axis(2, at = seq(input$left[1L], input$left[2L], length.out = 6), las = 2, col = "red", col.ticks = "red")
        axis(4, at = seq(input$left[1L], input$left[2L], length.out = 6), 
                labels = seq(input$right[1L], input$right[2L], length.out = 6), 
                las = 2, col = "blue", col.ticks = "blue")
    })
    output$datatable <- renderTable(cbind(x,y1,y2)[seq.int(0,50,5),])
})
