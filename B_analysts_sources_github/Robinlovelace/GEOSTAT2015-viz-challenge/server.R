library(shiny)
library(leaflet)
library(sp)
library(lubridate)
library(dplyr)
library(raster)
library(gstat)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(tidyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  source("setup.R")
  source("R/krige-setup.R")

  dIn <- reactive({
    sel <- d$time > input$end_date[[1]] & # times more than the start time and..
      d$time < input$end_date[[2]] & # times less than the end time
      grepl(input$ptype, d$precip_type) &
      !is.na(d$precip_type) &
      d$altitude_m > input$alt[[1]] & d$altitude_m < input$alt[[2]]
    d[sel, ]
  })

  output$distPlot <- renderPlot({
    d <- dIn()
    d$dt <- round_date(d$time, unit = "month")
    group_by(d@data, dt, rdb_id) %>%
      summarise(Monthly_precip = mean(precip_mm), Altitude = mean(altitude_m), na.rm = T) ->
      dag
#     lmod <- lm(Monthly_precip ~ dt, data = dag)
#     ndt <- as.numeric(dag$dt - min(dag$dt))
#     cmod <- loess(Monthly_precip ~ ndt, data = dag,span=input$span)
#     nlp <- predict(cmod,newdata=seq(min(ndt),max(ndt),l=100))
#     par(bg='lightyellow')

    dag$Altitude_bin <- cut(dag$Altitude, c(0, 100, 500, 2000),
      labels = c("0 - 100m", "100 - 500m", "500 - 2000m"))

    # create rolling mean

    tag <- group_by(dag, dt, Altitude_bin) %>%
      summarise(x = mean(Monthly_precip, na.rm = T))
    tag <- spread(tag, Altitude_bin, x)

    if(ncol(tag) > 2){
      tag[2:ncol(tag)] <- apply(tag[2:ncol(tag)], 2, RcppRoll::roll_mean, n = input$span, fill = NA)
      tag <- gather(tag, Altitude_bin, Monthly_precip, -dt)
    } else{
      names(tag)[2] <- "Monthly_precip"
    }



    # tag$pz <- zoo(tag$dt, tag$`0 - 100m`)
    # tag$m.av <- rollmean(tag$pz, 6, fill = list(NA, NULL, NA))

#     # base graphics way
#     ylim <- quantile(dag$Monthly_precip, seq(0, 1, 0.1), na.rm = T)[10] # the 9th decile
#     coplot(dag$Monthly_precip ~ dag$dt | dag$Altitude, ylab='Monthly Precipitation', xlab='Date',
#       pch = 16, col = adjustcolor(col = "blue", alpha.f = 0.15), cex = 1.5, cex.lab = 0.7,
#       rows = 1, cex.axis = 0.6, number = 3, ylim = c(0, 50),
#       panel = panel.smooth)

    # ggplot2 way
    ggplot(dag, aes(dt, Monthly_precip)) +
      geom_point() +
      geom_line(data = tag, aes(dt, Monthly_precip), col = "red", size = 3) +
      # stat_smooth(method = "lm", formula = 10 ^ Monthly_precip ~ ) +
      facet_grid(~ Altitude_bin) +
      # geom_hline(aes(intercept = mp)) +
      scale_y_log10() +
      scale_x_datetime(breaks = date_breaks("year"), labels = date_format("%y")) +
      theme_bw()
  })

  output$map <- renderLeaflet({
    d <- dIn()

    d_uniq <- group_by(d@data, rdb_id) %>%
      summarise(x = mean(lonWGS84),
        y = mean(latWGS84),
        Mean_precip = mean(precip_mm, na.rm = T))

    d_uniq <- as.data.frame(d_uniq)
    coordinates(d_uniq) <- ~x+y
    d_uniq <- d_uniq[!is.na(d_uniq$Mean_precip),] # remove na's

    # create raster layer
    m <- vgm(.59, "Sph", 874, .04)
    kr <- krige(formula = Mean_precip~ 1, locations = d_uniq, newdata = gddf)
    r <- raster(kr)
    proj4string(r) <- CRS("+init=epsg:4326")
    colfun <- function(x) colorRamp(brewer.pal(6,'YlOrRd'), interpolate='spline')(x^0.333)
    qpal <- colorNumeric(palette = colfun, domain = d_uniq$Mean_precip, na.color = "grey")
    qpal2 <- colorNumeric(palette = "YlOrRd", domain = values(r), na.color = "grey")

    if(input$dtype == "Points"){

      leaflet() %>%
        addProviderTiles("Esri.WorldTerrain") %>%
        addCircles(data = d_uniq, color = ~qpal(Mean_precip), opacity = 0.6, radius = 8, group = "Points") %>%
        addLegend(pal = qpal, values = d_uniq$Mean_precip, title = "mm/day")
    } else{
      leaflet() %>%
        addProviderTiles("Esri.WorldTerrain") %>%
        addRasterImage(r, opacity = 0.5, colors = qpal2(r@data@values), group = "Raster") %>%
        addLegend(pal = qpal2, values = values(r))
    }

    })

  output$text <- renderText(paste(input$end_date[[1]], input$end_date[[2]]))
})