# Load in test data from the BC Air Quality Network
require(lubridate)
df <- read.csv("Abbotsford A Columbia Street-2011-2012-MON.csv", header = TRUE)
df$Date.Time <- parse_date_time(as.character(df$Date.Time), "%m%d%y %H%M")
df$Date.Time <- as.POSIXct(df$Date.Time, tz = "America/Vancouver")

# Rename rows to contain units; remove 1st row with unit labels
for (i in 2:(ncol(df))) {
  colnames(df)[i] <- paste(colnames(df)[i],".",gsub(" ","",as.character(df[1,i])), sep = "")
}
df <- df[-1, ]
rm(i)

# Extract year from Date.Time column
df$year <- year(df$Date.Time)

# Remove Date.Time column
df <- df[,-1]

# Function for hourly percentiles for particulate matter
hourly.pm.stats <- function(df, year, pm = NULL, pm10 = NULL, pm25 = NULL,
                            estimate.pm = TRUE, estimate.pm10 = TRUE, estimate.pm25 = TRUE,
                            ratios = c(0.47, 0.072), percentiles = c(100, 99, 98, 95, 90, 75, 50)) {
  
  # Determine whether any inputs for pm, pm10, or pm25 were provided
  any.cols.defined <- ifelse(is.null(pm) & is.null(pm10) & is.null(pm25),
     FALSE, TRUE)
  
  # If data frame is a vector, make it a data frame
  if (is.vector(df)) {
    df <- data.frame(x = df)
  }

  # Test if data has a PM/TSP column
  PM.col <- ifelse(!is.na(pmatch("PM", colnames(df))), pmatch("PM", colnames(df)), 0)
  TSP.col <- ifelse(!is.na(pmatch("TSP", colnames(df))), pmatch("TSP", colnames(df)), 0)
  pm.col <- ifelse(!is.na(pmatch("pm", colnames(df))), pmatch("pm", colnames(df)), 0)
  tsp.col <- ifelse(!is.na(pmatch("tsp", colnames(df))), pmatch("tsp", colnames(df)), 0)
  PM.cols <- sum(c(PM.col, TSP.col, pm.col, tsp.col))
  any.PM.col <- PM.cols > 0 
  allNA.any.PM.col <- if (any.PM.col == TRUE & all(is.na(PM.cols)) == TRUE ) {
                      TRUE
                      } else if (any.PM.col == TRUE & all(is.na(PM.cols)) == FALSE) {
                      FALSE
                      } else {TRUE}

  # Test if data has a PM10 column
  PM10.col <- ifelse(!is.na(pmatch("PM10", colnames(df))), pmatch("PM10", colnames(df)), 0)
  pm10.col <- ifelse(!is.na(pmatch("pm10", colnames(df))), pmatch("pm10", colnames(df)), 0)
  PM10.cols <- sum(c(PM10.col, pm10.col))
  any.PM10.col <- PM10.cols > 0
  allNA.any.PM10.col <- if (any.PM10.col == TRUE & all(is.na(PM10.cols)) == TRUE ) {
                        TRUE
                        } else if (any.PM10.col == TRUE & all(is.na(PM10.cols)) == FALSE) {
                        FALSE
                        } else {TRUE}

  # Test if data has a PM25 column
  PM25.col <- ifelse(!is.na(pmatch("PM25", colnames(df))), pmatch("PM25", colnames(df)), 0)
  pm25.col <- ifelse(!is.na(pmatch("pm25", colnames(df))), pmatch("pm25", colnames(df)), 0)
  PM25.cols <- sum(c(PM25.col, pm25.col))
  any.PM25.col <- PM25.cols > 0
  allNA.any.PM25.col <- if (any.PM25.col == TRUE & all(is.na(PM25.cols)) == TRUE ) {
                        TRUE
                        } else if (any.PM25.col == TRUE & all(is.na(PM25.cols)) == FALSE) {
                        FALSE
                        } else {TRUE}
  
  # Break function if no columns defined and if the PM data columns detected all contain NAs
  if (any.cols.defined == FALSE &
      allNA.any.PM.col == TRUE &
      allNA.any.PM10.col == TRUE &
      allNA.any.PM25.col == TRUE) {
    break("No data available")
    } else {NULL}

  # Determine whether PM data should be estimated
  get.estimate.PM <- ifelse(any.PM.col == FALSE & estimate.pm == TRUE, TRUE, FALSE)
  get.estimate.PM10 <- ifelse(any.PM10.col == FALSE & estimate.pm10 == TRUE, TRUE, FALSE)
  get.estimate.PM25 <- ifelse(any.PM25.col == FALSE & estimate.pm25 == TRUE, TRUE, FALSE)
  
  # Detect if there is a column of class "POSIXct"
  posix_time <- mat.or.vec(ncol(df),1)
  for (i in 1:(ncol(df))) {
    posix_time[i] <- ifelse(class((df)[,i])[1] == "POSIXct", 1, NA)
  }
  posix_col <- match(1, posix_time)
  
  # Detect if there is a column that contains year data
  year_col <- mat.or.vec(ncol(df),1)
  for (i in 1:(ncol(df))) {
    year_col[i] <- ifelse(mean(as.numeric(df[,i]), na.rm = TRUE) <= (year(Sys.time()) + 1) &
                          mean(as.numeric(df[,i]), na.rm = TRUE) > 1950, 1, NA)
  }
  year_col <- match(1, year_col)
  
  if (!is.na(posix_col)) {
      # Create subset of data frame based on POSIX formatted objects and year selected 
    data_year <- df[which(year(df[,posix_col]) == year), ]
  } else if (!is.na(year_col)) {
    data_year <- df[which(as.numeric(df[,year_col]) == year), ]
  } else { stop("Year information cannot be found in data frame") }
    
  # Determine how large to make the empty data frame for the summary statistics
  number_of_percentiles <- length(percentiles)
  
  # Construct the data frame, provide column names
  hourly_percentiles <- as.data.frame(matrix(nrow = (number_of_percentiles), ncol = 6))
  colnames(hourly_percentiles) <- c("year", "percentile", "type", "pm", "pm10", "pm25")
  
  # Generate vectors of percentiles for each of pm, pm10, and pm25
  if (any.PM.col == TRUE) {
  pm_percentiles <- quantile(as.numeric(as.character(data_year[,max(PM.cols)])),
                             probs = percentiles/100, na.rm = TRUE)
  } else {
  pm_percentiles <- rep(NA, times = length(percentiles))
  }
  
  if (any.PM10.col == TRUE) {
  pm10_percentiles <- quantile(as.numeric(as.character(data_year[,max(PM10.cols)])),
                               probs = percentiles/100, na.rm = TRUE)
  } else {
    pm10_percentiles <- rep(NA, times = length(percentiles))
  }
  
  if (any.PM25.col == TRUE) {
  pm25_percentiles <- quantile(as.numeric(as.character(data_year[,max(PM25.cols)])),
                               probs = percentiles/100, na.rm = TRUE)
  } else {
    pm25_percentiles <- rep(NA, times = length(percentiles))
  }
  
  # Place data into data frame
  hourly_percentiles$year <- year
  hourly_percentiles$percentile <- percentiles
  hourly_percentiles$type <- "hourly"
  hourly_percentiles$pm <- round(pm_percentiles, digits = 2)
  hourly_percentiles$pm10 <- round(pm10_percentiles, digits = 2)
  hourly_percentiles$pm25 <- round(pm25_percentiles, digits = 2)
  
  # Make estimates of percentiles when measurement data for either PM, PM10, or PM25 are missing
  pm.est <- mat.or.vec(nrow(hourly_percentiles),1)
  for (i in 1:(nrow(hourly_percentiles))) {
    pm.est[i] <- ifelse(get.estimate.PM == TRUE &
                        sum(hourly_percentiles$pm10) > 0,
                        round((1/ratios[1]) * hourly_percentiles$pm10[i], digits = 2),
                            ifelse(get.estimate.PM == TRUE &
                                   sum(hourly_percentiles$pm25) > 0,
                                   round((1/ratios[2]) * hourly_percentiles$pm25[i], digits = 2),      
                                   NA))
  }
  hourly_percentiles$pm.est <- pm.est

  pm10.est <- mat.or.vec(nrow(hourly_percentiles),1)
  for (i in 1:(nrow(hourly_percentiles))) {
    pm10.est[i] <- ifelse(get.estimate.PM10 == TRUE &
                          sum(hourly_percentiles$pm) > 0,
                          round((ratios[1]) * hourly_percentiles$pm[i], digits = 2),
                              ifelse(get.estimate.PM10 == TRUE &
                                     sum(hourly_percentiles$pm25) > 0,
                                     round((ratios[1]/ratios[2]) * hourly_percentiles$pm25[i],
                                           digits = 2),
                                     NA))
  }
  hourly_percentiles$pm10.est <- pm10.est


  pm25.est <- mat.or.vec(nrow(hourly_percentiles),1)
  for (i in 1:(nrow(hourly_percentiles))) {
  pm25.est[i] <- ifelse(get.estimate.PM25 == TRUE &
                        sum(hourly_percentiles$pm) > 0,
                        round((ratios[2]) * hourly_percentiles$pm[i], digits = 2),
                            ifelse(get.estimate.PM25 == TRUE &
                                   sum(hourly_percentiles$pm10) > 0,
                                   round((ratios[2]/ratios[1]) * hourly_percentiles$pm10[i],
                                         digits = 2),
                                   NA))
  }
  hourly_percentiles$pm25.est <- pm25.est

  # Remove NA columns from 'hourly_percentiles' data frame
  hourly_percentiles <- hourly_percentiles[,colSums(is.na(hourly_percentiles)) <
                        nrow(hourly_percentiles)]
  
  # Break function if there are no PM, PM10, or PM25 columns
  if ("pm" %in% colnames(hourly_percentiles) == FALSE &&
      "pm.est" %in% colnames(hourly_percentiles) == FALSE &&
      "pm10" %in% colnames(hourly_percentiles) == FALSE &&
      "pm10.est" %in% colnames(hourly_percentiles) == FALSE &&
      "pm25" %in% colnames(hourly_percentiles) == FALSE &&
      "pm25.est" %in% colnames(hourly_percentiles) == FALSE ){
    stop("No data available for evaluation")
  } else { NULL }
  
  
  # Reorder columns by name
  hourly_percentiles <- hourly_percentiles[c("year", "percentile", "type",
                        ifelse("pm" %in% colnames(hourly_percentiles), "pm", 
                          ifelse("pm.est" %in% colnames(hourly_percentiles),"pm.est", "")),
                        ifelse("pm10" %in% colnames(hourly_percentiles), "pm10",
                          ifelse("pm10.est" %in% colnames(hourly_percentiles),"pm10.est", "")),
                        ifelse("pm25" %in% colnames(hourly_percentiles), "pm25",
                          ifelse("pm25.est" %in% colnames(hourly_percentiles),"pm25.est", "")))]
  
  # Print the hourly stats data frame
  print(hourly_percentiles)
}


# Function for sequential, daily percentiles
daily.seq.pm.stats <- function(df, year, pm = NULL, pm10 = NULL, pm25 = NULL,
                               estimate.pm = FALSE, estimate.pm10 = FALSE, estimate.pm25 = FALSE,
                               ratios = c(0.47, 0.072),
                               percentiles = c(100, 99, 98, 95, 90, 75, 50)) {  
  
  #data <- dust_data_2010_2012
  #tsp <- "dust"  #character
  #pm10 <- "pm10" #character
  #pm25 <- "pm25" #character
  #year <- 2012   #numeric
  #percentiles <- c(100, 99, 98, 95, 90, 75, 50)  #numeric vector
  
  # Determine whether any inputs for pm, pm10, or pm25 were provided
  any.cols.defined <- ifelse(is.null(pm) & is.null(pm10) & is.null(pm25),
                             FALSE, TRUE)
  
  # If data frame is a vector, make it a data frame
  if (is.vector(df)) {
    df <- data.frame(x = df)
  }
  
  # Test if data has a PM/TSP column
  PM.col <- ifelse(!is.na(pmatch("PM", colnames(df))), pmatch("PM", colnames(df)), 0)
  TSP.col <- ifelse(!is.na(pmatch("TSP", colnames(df))), pmatch("TSP", colnames(df)), 0)
  pm.col <- ifelse(!is.na(pmatch("pm", colnames(df))), pmatch("pm", colnames(df)), 0)
  tsp.col <- ifelse(!is.na(pmatch("tsp", colnames(df))), pmatch("tsp", colnames(df)), 0)
  PM.cols <- sum(c(PM.col, TSP.col, pm.col, tsp.col))
  any.PM.col <- PM.cols > 0 
  allNA.any.PM.col <- if (any.PM.col == TRUE & all(is.na(PM.cols)) == TRUE ) {
                      TRUE
                      } else if (any.PM.col == TRUE & all(is.na(PM.cols)) == FALSE) {
                      FALSE
                      } else {TRUE}
  
  # Test if data has a PM10 column
  PM10.col <- ifelse(!is.na(pmatch("PM10", colnames(df))), pmatch("PM10", colnames(df)), 0)
  pm10.col <- ifelse(!is.na(pmatch("pm10", colnames(df))), pmatch("pm10", colnames(df)), 0)
  PM10.cols <- sum(c(PM10.col, pm10.col))
  any.PM10.col <- PM10.cols > 0
  allNA.any.PM10.col <- if (any.PM10.col == TRUE & all(is.na(PM10.cols)) == TRUE ) {
    TRUE
  } else if (any.PM10.col == TRUE & all(is.na(PM10.cols)) == FALSE) {
    FALSE
  } else {TRUE}
  
  # Test if data has a PM25 column
  PM25.col <- ifelse(!is.na(pmatch("PM25", colnames(df))), pmatch("PM25", colnames(df)), 0)
  pm25.col <- ifelse(!is.na(pmatch("pm25", colnames(df))), pmatch("pm25", colnames(df)), 0)
  PM25.cols <- sum(c(PM25.col, pm25.col))
  any.PM25.col <- PM25.cols > 0
  allNA.any.PM25.col <- if (any.PM25.col == TRUE & all(is.na(PM25.cols)) == TRUE ) {
    TRUE
  } else if (any.PM25.col == TRUE & all(is.na(PM25.cols)) == FALSE) {
    FALSE
  } else {TRUE}
  
  # Break function if no columns defined and if the PM data columns detected all contain NAs
  if (any.cols.defined == FALSE &
        allNA.any.PM.col == TRUE &
        allNA.any.PM10.col == TRUE &
        allNA.any.PM25.col == TRUE) {
    break("No data available")
  } else {NULL}
  
  # Determine whether PM data should be estimated
  get.estimate.PM <- ifelse(any.PM.col == FALSE & estimate.pm == TRUE, TRUE, FALSE)
  get.estimate.PM10 <- ifelse(any.PM10.col == FALSE & estimate.pm10 == TRUE, TRUE, FALSE)
  get.estimate.PM25 <- ifelse(any.PM25.col == FALSE & estimate.pm25 == TRUE, TRUE, FALSE)
  
  posix_time <- mat.or.vec(ncol(df),1)
  for (i in 1:(ncol(df))) {
    posix_time[i] <- ifelse(class((df)[,i])[1] == "POSIXct", 1, NA)
  }
  posix_col <- match(1, posix_time)
  
  # Make all column data except POSIX data factors
  number_cols <- ncol(df)
  for (i in 1:number_cols) {
    df[,i] <- ifelse(class(df[,i]) == "factor", as.numeric(levels(df[,i]))
  }
  
  # Detect if there is a column that contains year data
  year_col <- mat.or.vec(ncol(df),1)
  for (i in 1:(ncol(df))) {
    year_col[i] <- ifelse(mean(as.numeric(df[,i]), na.rm = TRUE) <= (year(Sys.time()) + 1) &
                            mean(as.numeric(df[,i]), na.rm = TRUE) > 1950, 1, NA)
  }
  year_col <- match(1, year_col)
  
  if (!is.na(posix_col)) {
    # Create subset of data frame based on POSIX formatted objects and year selected 
    data_year <- df[which(year(df[,posix_col]) == year), ]
  } else if (!is.na(year_col)) {
    data_year <- df[which(as.numeric(df[,year_col]) == year), ]
  } else { stop("Year information cannot be found in data frame") }
  
  # Determine how large to make the empty data frame for the summary statistics
  number_of_percentiles <- length(percentiles)  
  
  # Construct the data frame, provide column names
  daily_percentiles <- as.data.frame(matrix(nrow = (number_of_percentiles), ncol = 6))
  colnames(daily_percentiles) <- c("year", "tsp", "pm10", "pm25", "percentile", "type")
  
  # Attach factor of day of year for each record in subset
  if (!is.na(year_col)) {
    timestamp <- mat.or.vec(ncol(df),1) #finish this: need to ID month, day, and hour cols
    timestamp <- ISOdatetime(data_year[,year_col],
                             data_year[,month_col],
                             data_year[,day_col],
                             data_year[,hour_col],
                             min = 0, sec = 0, tz = "")
    data_year$timestamp <- timestamp
    data_year$yday <- yday(data_year$timestamp)
  } else if (!is.na(posix_col)) {
    data_year$yday <- yday(data_year[,posix_col])
  } else { stop("Time information cannot be determined in this data frame") }
  
  # Initialize empty vectors for daily averages
  pm_daily_mean <- as.numeric(matrix(nrow = max(data_year$yday), ncol = 1))
  pm10_daily_mean <- as.numeric(matrix(nrow = max(data_year$yday), ncol = 1)) 
  pm25_daily_mean <-  as.numeric(matrix(nrow = max(data_year$yday), ncol = 1))
  
  # Generate daily averages for each of tsp, pm10, and pm25
  for (i in 1:max(data_year$yday)) {
    data_day <- data_year[which(data_year$yday == i), ]
    pm_daily_mean[i] <- mean(data_day$dust, na.omit = TRUE)
    pm10_daily_mean[i] <- mean(data_day$pm10, na.omit = TRUE)
    pm25_daily_mean[i] <- mean(data_day$pm25, na.omit = TRUE)
  }
  
  # Generate vectors of percentiles for each of tsp, pm10, and pm25
  pm_percentiles <- quantile(pm_daily_mean, probs = percentiles/100, na.rm = TRUE)
  pm10_percentiles <- quantile(pm10_daily_mean, probs = percentiles/100, na.rm = TRUE)
  pm25_percentiles <- quantile(pm25_daily_mean, probs = percentiles/100, na.rm = TRUE)
  
  # Place data into data frame
  daily_percentiles$year <- year
  daily_percentiles$pm <- pm_percentiles
  daily_percentiles$pm10 <- pm10_percentiles
  daily_percentiles$pm25 <- pm25_percentiles
  daily_percentiles$percentile <- percentiles
  daily_percentiles$type <- "daily, sequential"
  
  # Print the hourly stats data frame
  print(daily_percentiles)
  print(data_day)
  print(pm10_daily_mean)
}

# Create function for annual geometric mean
