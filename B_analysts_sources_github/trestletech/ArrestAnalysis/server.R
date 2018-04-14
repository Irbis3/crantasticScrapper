library(shiny)

computePrecipWindow <- function(actByDay, wd, windowSize=2){
  rollPrecip <- wd
  for (i in nrow(wd):windowSize){
    rollPrecip[i,2] <- sum(rollPrecip[(i-windowSize+1):i,2], na.rm = FALSE)
  }
  
  # Trim first two rows off, for which we don't have data.
  rollPrecip <- rollPrecip[-windowSize+1:-1,]
  
  start <- max(min(actByDay$Date), min(rollPrecip$date))
  end <- min(max(actByDay$Date), max(rollPrecip$date))
  
  # Align dates
  actByDay <- actByDay[actByDay$Date <= end & actByDay$Date >= start,]
  rollPrecip <- rollPrecip[rollPrecip$date <= end & rollPrecip$date >= start,]
  wd <- wd[wd$date <= end & wd$date >= start,]
  actByDay$RollPrecip <- rollPrecip$precip
  
  actByDay$Precip <- wd$precip
  
  actByDay
}

wd <- readRDS("wd.Rds")
abd <- readRDS("abd.Rds")

shinyServer(function(input, output) {
  rollPrecip <- reactive({
    computePrecipWindow(abd, wd, input$windowSize)
  })
  
  output$plot <- renderPlot({
    plot(abd$Date, abd$Activity, type="l")
    lines(rollPrecip()$Date, rollPrecip()$RollPrecip, col=2)
  })  
  
  output$model <- renderPrint({
    rp <- rollPrecip()
    mod <- lm(rp$Activity ~ rp$Precip + rp$RollPrecip)
    print(mod)
  })
})
