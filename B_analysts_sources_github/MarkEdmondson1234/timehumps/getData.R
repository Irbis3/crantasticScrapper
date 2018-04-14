library(googleAnalyticsR)
library(ggplot2)

ga_auth(no_auto = TRUE)

al <- google_analytics_account_list()
gaid <- 81416156
start <- "2016-01-01"
end <- "2017-01-26"

gadata <- google_analytics_4(gaid,
                             date_range = c(start,end),
                             metrics = "sessions",
                             dimensions = c("nthMinute","sourceMedium","hostname","landingPagePath"),
                             max = -1)

gadata$fullURL <- paste0(gadata$hostname, gadata$landingPagePath)
gadata$timestamp <- as.POSIXct(as.numeric(gadata$nthMinute)*60, 
                               origin = as.POSIXct(as.Date(start)))

# 124 URLs
the_urls <- unique(gadata$fullURL)

work_data <- gadata[,c("timestamp","sourceMedium","fullURL")]


## list per URL of sources/min
ref_list <- split(work_data, work_data$fullURL)
ref_list <- setNames(ref_list, 
                     vapply(ref_list, 
                            function(x) x[["fullURL"]][[1]], 
                            character(1), 
                            USE.NAMES = FALSE))

example_data <- ref_list[["code.markedmondson.me/real-time-GTM-google-cloud-r-shiny-1/"]]
example_data$bins <- as.numeric(cut(as.numeric(example_data$timestamp), breaks = 30))


timehump_heatmap <- function(the_data,
                             bins = 30){
  
  if(nrow(the_data) < 5){
    return(NULL)
  }
  ## change order of factor levels to order they appear
  the_data$name <- factor(the_data$sourceMedium, 
                          levels = unique(the_data$sourceMedium[order(the_data$timestamp)]))
  
  # make plot
  gg <- ggplot(the_data, aes(x = timestamp, y = name)) + theme_minimal()
  gg <- gg + geom_bin2d(bins = bins)
  gg <- gg + ggtitle(the_data$fullURL[[1]])
  gg <- gg + guides(fill = guide_legend(title = "Session \ncount"))
  gg <- gg + xlab("Date of session") + ylab("Source / Medium")
  gg <- gg + scale_fill_gradientn(colours = c("#bdc9e1","#ffffbf", "#ca0020"))
  
  gg
}

focus_data <- example_data[example_data$timestamp < as.POSIXct(as.Date("2017-01-19")),]
focus_data <- focus_data[focus_data$sourceMedium %in% unique(focus_data$sourceMedium)[1:6],]
                           
timehump_heatmap(focus_data, 50)

## make all the plots
lapply(ref_list, timehump_heatmap)


## if want to avoid ggplot doing the binning for you:
library(dplyr)

example_data$name <- example_data$sourceMedium
example_plot <- example_data %>% 
  select(bins, name) %>% group_by(bins, name) %>% 
  summarise(value = n())

ggplot(example_plot, aes(x = bins, y = name, fill = value)) + geom_tile()

