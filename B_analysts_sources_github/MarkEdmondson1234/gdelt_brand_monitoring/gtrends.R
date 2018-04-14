# install.packages("gtrendsR")
library(gtrendsR)

## set in .Renviron
# GOOGLE_USER = "mark@iihnordic.com"
# GOOGLE_PASSWORD = 'pw'
# GOOGLE_AUTOCONNECT = TRUE
options(google.user = Sys.getenv("GOOGLE_USER"))
options(google.password = Sys.getenv("GOOGLE_PASSWORD"))
gconnect()

brand <- c("IKEA","Beoplay","fullrate")
geography <- "DK"
## weekly breakdown when greater than 3 months
## daily breakdown when less
start <- as.character(Sys.Date() - 90)
end <- as.character(Sys.Date())

trend_data <- gtrends(brand, geography, start_date = start, end_date = end)
trend_data_long <- gtrends(brand, geography, start_date = "2004-01-01", end_date = end)
