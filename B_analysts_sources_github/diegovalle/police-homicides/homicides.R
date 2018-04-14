smallMPlot <- function(df, states = NULL,  title = "",
                       ncol = NULL,
                       minor = NULL, major = NULL){
  if(!is.null(states))
    df <- subset(df, State %in% states)
  
  df <- ddply(df, .(State), transform,
        order = mean(rate[(length(rate)-32):length(rate)],
          na.rm = TRUE))
  df$State <- reorder(df$State, -df$order)
  ggplot(df, aes(date, rate, group = State)) +
    geom_line() +
    #geom_point(aes(size = Homicides)) +
    scale_x_date(minor = minor, major = major) +
    ylim(0, max(df$rate, na.rm = TRUE)) +
    opts(title = title) +
    ylab("annualized homicide rate") +
    #scale_y_log10() +
    facet_wrap(~ State, ncol = ncol) 
}

labelChart <- function(chart, df, y) {
 chart +  
    geom_vline(data = df, aes(xintercept = date), alpha = .7,
               linetype = 2) +
    geom_text(aes(date,y, label = label),
            data = df,
            size = 3.2, hjust = 1.03, vjust = 0)
#    geom_point(aes(size = Homicides))
}
    
plotEvents <- function(df, events, states, title = "",
                       ncol = NULL){
  p <- smallMPlot(df,
                  states = states,
                  title = title, ncol)
  labelChart(p, subset(events, State %in% states))
}

savePlotEvents <- function(filename, df, events,
                           states, title = "",
                           height = 5, width = 8,
                           ncol = NULL) {
  plotEvents(df, events, states, title = "", ncol)
  filename <- str_c("graphs/", filename)
  ggsave(filename, dpi = 100, height = height, width = width)
}


#Plot all data to see how good it is
ggplot(hom, aes(date, rate, group = State)) +
  geom_line() +
  scale_x_date() +
  facet_wrap(~ State)
ggsave("graphs/all.png", dpi = 100, height = 5, width = 8)


#A plot of National Rates with the Drug War in red
dwplot <- ggplot(hom.nat, aes(date, rate, group = State)) +
    geom_rect(xmin = as.numeric(as.Date("2006-12-15")),
              xmax = Inf,
              ymin=0, ymax=21, alpha = .01, fill = "red") +
    geom_line() +
    #geom_point(aes(size = Homicides)) +
    scale_x_date(minor = "3 months", major = "2 years") +
    ylim(0, max(hom.nat$rate, na.rm = TRUE)) +
    annotate("text", x =  as.numeric(as.Date("2007-12-15")),
                       y = 16.9, label = "Drug War") +
    opts(title = "Homicide Rate in Mexico (Jan 1997 - Aug 2010)") +
    ylab("annualized homicide rate") +
    theme_bw()
ggsave("graphs/total.png", dpi = 100, height = 5, width = 8)



smallMPlot(hom.dw)
all <- smallMPlot(hom.dw, c("Sonora","Baja California",
                       "Chihuahua", "Durango",
                       "Sinaloa", "Nayarit",
                       "Nuevo Leon", "Tamaulipas",
                       "Michoacan", "Jalisco",
                       "Guerrero", "Morelos",
                       "Coahuila", "Colima",
                       "Quintana Roo", "Guanajuato"),
           "Homicide Rates in States Affected by the Drug War",
                  minor = "3 months", major = "2 years")
ggsave("graphs/narco-states.png", dpi = 100,
       height = 7, width = 10)

chih <- plotEvents(hom.dw, important.dates,
            c("Chihuahua"),
           "Sinaloa Cartel vs Juárez Cartel")

sin.dur <- plotEvents(hom.dw, important.dates,
            c("Sinaloa", "Durango"),
           "Sinaloa Cartel vs Zetas and Beltrán Leyvas", ncol=1)

z.vs.cdg <- plotEvents(hom.dw, important.dates,
           c("Nuevo Leon", "Tamaulipas", "Coahuila"),
           "Zetas vs Gulf Cartel")

bl <- plotEvents(hom.dw, important.dates,
           c("Colima", "Nayarit", "Jalisco", "Quintana Roo"),
           "Other States where the Beltran Leyva Cartel and Zetas Operate")

b.vs.bl <- plotEvents(hom.dw, important.dates,
            c("Guerrero", "Morelos", "Sonora"),
           "Sinaloa vs Beltrán Leyvas")

mich <- plotEvents(hom.dw, important.dates,
            c("Michoacan"),
           "La Familia Michoacana vs Zetas")

smallMPlot(hom.dw,
            c("Zacatecas", "Aguascalientes", "Guanajuato",
              "Chiapas", "Mexico"),
           "Miscellaneous")

smallMPlot(hom.dw,
            c("Nayarit"),
           "Nayarit")


hom.dw$op.tj <- ifelse(hom.dw$date < as.Date("2007-01-03"),
                         TRUE, FALSE)
bc <- plotEvents(hom.dw, important.dates,
            c("Baja California"),
           "Sinaloa Cartel vs Tijuana Cartel") +
  geom_smooth(method = MASS::rlm,
                             aes(group = op.tj))
ggsave("graphs/bc.png", dpi = 100, height = 5, width = 8)

#States with the highest homicide rates during Calderon's Presidency
ddply(subset(hom, date <= as.Date("2010-08-15") &
             date >= as.Date("2006-12-15")),
      .(State), function(df) mean(df$rate, na.rm = TRUE))

# A csv file for OpenHeatMap
annual <- ddply(subset(hom.dw,
                       date <= as.Date("2010-08-15") &
                       date >= as.Date("2009-08-15")),
      .(State), function(df) log(mean(df$rate, na.rm = TRUE)))
write.csv(annual, "annual-log.csv", row.names = FALSE)


tryCatch({
    setwd("reports/homicides")
    Sweave("homicides.Rnw",driver=RweaveAsciidoc)
  },
  finally = setwd("../..")
)
