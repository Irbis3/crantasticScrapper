
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)

shinyServer(function(input, output, session) {
  filtered_crime <- reactive({
    input$date1
    
    isolate({
      crime %>%
        filter(CrimeDate >= input$date1[1]) %>%
        filter(CrimeDate <= input$date1[2])
    })
  })
  
  observe({
    input$date1
    
    updateDateRangeInput(session, "date2",
                         "Select dates to visualize.",
                         start = input$date1[1],
                         end = input$date1[2],
                         min = min(crime$CrimeDate), max = max(crime$CrimeDate))
  })
  
  observe({
    input$date2
    
    updateDateRangeInput(session, "date1",
                         "Select dates to visualize.",
                         start = input$date2[1],
                         end = input$date2[2],
                         min = min(crime$CrimeDate), max = max(crime$CrimeDate))
  })
  
  output$crime_map <- renderLeaflet({
    filtered_crime() %>%
      leaflet() %>%
      setView(lng = "-76.6204859", lat = "39.2847064", zoom = 12) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(),
                 popup = ~content)
  })
  
  output$daily_plot <- renderPlot({
    daily_crime <- filtered_crime() %>%
      group_by(CrimeDate) %>%
      summarize(Crimes_Per_Day = n())
    
    ggplot(daily_crime, aes(CrimeDate, Crimes_Per_Day)) + geom_line()
  })
  
  output$desc_plot <- renderPlot({
    desc_crime <- filtered_crime() %>%
      group_by(Description) %>%
      summarize(Total = n())
    
    ggplot(desc_crime, aes(Description, Total)) + geom_bar(stat = "identity") + coord_flip()
  })
  
  output$total_crimes <- renderText({
    as.character(nrow(filtered_crime()))
  })
  
  output$common_crime <- renderText({
    names(tail(sort(table(filtered_crime()$Description)), 1))
  })
  
  output$weekday_crime <- renderText({
    names(tail(sort(table(wday(filtered_crime()$CrimeDate, label = TRUE, abbr = FALSE))), 1))
  })
  
})
