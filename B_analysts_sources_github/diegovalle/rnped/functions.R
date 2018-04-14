
findAnomalies <- function(munvec, dis){
  anomalies <- data.frame()
  for(munname in munvec){
    df <- subset(dis, name == munname)
    df <- df[order(df$date),]
    hom <- df
    hom$date  <- as.POSIXlt(hom$date, tz = "UTC")
    max_date <- max(hom$date)
    if(!all(hom$count == 0)) {
      #hom$rate[is.na(hom$rate)] <- mean(hom$rate, na.rm = TRUE)
      #breakout(hom$rate, min.size = 2, method = 'multi', beta=0.001, plot=TRUE)
      anoms <- AnomalyDetectionTs(hom[,c("date", "count")], 
                                  max_anoms=0.02, direction='both',
                                  threshold = 'p95')$anoms$timestamp
      if(!is.null(anoms))
        if(anoms[length(anoms)] >= as.POSIXlt(as.Date("2014-07-28"))) {
          if(df[which(df$date == as.Date(anoms)),]$count >= 3) {
            print(munname)
            anomalies <- rbind(anomalies, df)
          }
        }
    }
    
  }
  return(anomalies)
}


theme_bare <- function() {theme(axis.line=element_blank(),
                                axis.text.x=element_blank(),
                                axis.text.y=element_blank(),
                                axis.ticks=element_blank(),
                                axis.title.x=element_blank(),
                                axis.title.y=element_blank(),
                                panel.background=element_blank(),
                                panel.border=element_blank(),
                                panel.grid.major=element_blank(),
                                panel.grid.minor=element_blank())
}

sm_theme <- function() {
  theme(
    legend.position = "top", legend.title = element_text(family = "Lato Black", 
                                                         colour = "#0D0D0D", size = 10),
    legend.background = element_rect(fill = "#C7B470"),
    legend.key = element_rect(fill = "#C7B470", colour = "#C7B470"),
    legend.text = element_text(family = "Fugaz One", colour = "#0D0D0D", size = 10),
    plot.background = element_rect(fill = "#C7B470", colour = "#C7B470"),
    panel.background = element_rect(fill = "#C7B470"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#0D0D0D", family = "Muli", size = 6),
    plot.title = element_text(colour = "#361413", face = "bold", size = 24, vjust = 1, 
                              family = "Fugaz One"),
    axis.title = element_text(colour = "#0D0D0D", face = "bold", size = 14, 
                              family = "PT Sans Narrow"),
    panel.grid.major.y = element_line(colour = "#555555", size=0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Lato Black", colour = "white", size = 7),
    strip.background =  element_rect(fill = "#7B6824"),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 70, hjust = 1)
  )
}

infographic_theme2 <- function() {
  theme(
    legend.position = "top", legend.title = element_text(family = "Lato Black", 
                                                         colour = "#0D0D0D", size = 10),
    legend.background = element_rect(fill = "#C7B470"),
    legend.key = element_rect(fill = "#C7B470", colour = "#C7B470"),
    legend.text = element_text(family = "Fugaz One", colour = "#0D0D0D", size = 10),
    plot.background = element_rect(fill = "#C7B470", colour = "#C7B470"),
    panel.background = element_rect(fill = "#C7B470"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#0D0D0D", family = "Muli", size = 8),
    plot.title = element_text(colour = "#361413", face = "bold", size = 24, vjust = 1, 
                              family = "Fugaz One"),
    axis.title = element_text(colour = "#0D0D0D", face = "bold", size = 14, 
                              family = "PT Sans Narrow"),
    panel.grid.major.y = element_line(colour = "#444444", size=0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Lato Black", colour = "white", size = 14),
    strip.background = element_rect(fill = "#0D0D0D"),
    axis.ticks = element_line(colour = "#0D0D0D")
  )
}