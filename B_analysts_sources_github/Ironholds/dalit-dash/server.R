library(shinydashboard)
library(dygraphs)
library(xts)
library(reshape2)
library(RColorBrewer)

count_xts <- NULL
quality_xts <- NULL

# Data refresh function
renew_data <- function(){
  
  # Load data
  load(url("https://ironholds.org/projects/dalit_data.RData"))
  
  # Generate XTS objects
  dropped_count <- count_data[,2, drop=FALSE]
  names(dropped_count) <- "articles"
  count_xts <<- xts(dropped_count, count_data$date)
  
  casted_quality <- dcast(quality_data, date ~ quality_class, value.var = "count")
  casted_quality <- casted_quality[,c("date", "FA", "GA", "B", "C", "Start", "Stub")]
  quality_xts <<- xts(casted_quality[,2:ncol(casted_quality)], casted_quality$date)
  
}

# Start off the data
current_date <- Sys.Date()
renew_data()

server <- function(input, output) {
  
  # Check for data being outdated and refresh it if so
  if(Sys.Date() != current_date){
    current_date <<- Sys.Date()
    renew_data()
  }
  
  count_dygraph <- dygraph(data = count_xts, main = "Article count",
                           xlab = "Date", ylab = "Number of articles") %>%
    dyAxis(name = "y", valueRange = c(0, max(count_xts)+50)) %>%
    dyOptions(strokeWidth = 2.5, colors = RColorBrewer::brewer.pal(6, "Set2")) %>%
    dyLegend(show = "always") %>%
    dyCSS(css = "inverse.css")

  quality_dygraph <- dygraph(quality_xts, main = "Article Class",
                             xlab = "Date", ylab = "Number of articles") %>%
    dyAxis(name = "y", valueRange = c(0, max(quality_xts)+30)) %>%
    dyOptions(strokeWidth = 2.5, colors = RColorBrewer::brewer.pal(6, "Set2")) %>%
    dyLegend(show = "always") %>%
    dyCSS(css = "inverse.css")
  
  # Render output
  output$count_graph <- renderDygraph(count_dygraph)
  
  
  output$class_graph <- renderDygraph(quality_dygraph)
}