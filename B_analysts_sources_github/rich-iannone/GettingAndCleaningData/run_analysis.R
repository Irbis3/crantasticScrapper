
##
## Read in and process test data set
##

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE)
colnames(subject_test) <- "subject"

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", stringsAsFactors = FALSE, sep = "\r")
colnames(X_test) <- "data"

Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt", stringsAsFactors = FALSE, sep = "\r")
colnames(Y_test) <- "activity_code"

# Obtain mean values from each line in the test dataset
for (i in 1:nrow(X_test)){
  if (i == 1) test_mean <- vector(mode = "numeric", length = 0)
  
  mean <- mean(na.omit(as.numeric(unlist(strsplit(X_test[i,], "[ ]")))))
  
  test_mean <- c(test_mean, mean)
  
  if (i == nrow(X_test)) {
    test_mean <- as.data.frame(test_mean)
    colnames(test_mean) <- "mean"
  }
}

# Obtain standard deviation values from each line in the test dataset
for (i in 1:nrow(X_test)){
  if (i == 1) test_sd <- vector(mode = "numeric", length = 0)
  
  sd <- sd(na.omit(as.numeric(unlist(strsplit(X_test[i,], "[ ]")))))
  
  test_sd <- c(test_sd, sd)
  
  if (i == nrow(X_test)) {
    test_sd <- as.data.frame(test_sd)
    colnames(test_sd) <- "standard_deviation"
  }
}

# Clean up objects
rm(mean, sd, i, X_test)

# Column bind the test dataset objects into a single data frame
test_data <- cbind(subject_test, Y_test, test_mean, test_sd)


##
## Read in and process training data set
##

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE)
colnames(subject_train) <- "subject"

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", stringsAsFactors = FALSE, sep = "\r")
colnames(X_train) <- "data"

Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt", stringsAsFactors = FALSE, sep = "\r")
colnames(Y_train) <- "activity_code"

# Obtain mean values from each line in the training dataset
for (i in 1:nrow(X_train)){
  if (i == 1) train_mean <- vector(mode = "numeric", length = 0)
  
  mean <- mean(na.omit(as.numeric(unlist(strsplit(X_train[i,], "[ ]")))))
  
  train_mean <- c(train_mean, mean)
  
  if (i == nrow(X_train)) {
    train_mean <- as.data.frame(train_mean)
    colnames(train_mean) <- "mean"
  }
}

# Obtain standard deviation values from each line in the training dataset
for (i in 1:nrow(X_train)){
  if (i == 1) train_sd <- vector(mode = "numeric", length = 0)
  
  sd <- sd(na.omit(as.numeric(unlist(strsplit(X_train[i,], "[ ]")))))
  
  train_sd <- c(train_sd, sd)
  
  if (i == nrow(X_train)) {
    train_sd <- as.data.frame(train_sd)
    colnames(train_sd) <- "standard_deviation"
  }
}

# Clean up objects
rm(mean, sd, i, X_train)

# Column bind the training dataset objects into a single data frame
train_data <- cbind(subject_train, Y_train, train_mean, train_sd)

##
## Combine the partially-processed test and training datasets
##

# Row bind the test and training data frames into a single data frame

combined_data <- rbind(test_data, train_data)

##
## Use descriptive activity names to name the activities in the data set
##

# Get table of activity codes

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",
                              stringsAsFactors = FALSE, sep = "\r")

for (i in 1:nrow(activity_labels)){
  if (i == 1) {
    activities <- as.data.frame(mat.or.vec(nr = nrow(activity_labels), nc = 2))
    colnames(activities) <- c("activity_code", "activity")
  }
  
  activities[i,1] <- unlist(strsplit(activity_labels[i,], " "))[1]
  activities[i,2] <- unlist(strsplit(activity_labels[i,], " "))[2]
  
}

# Remove some objects from the global environment

rm(activity, activity_labels,
   subject_test, subject_train,
   test_data, test_mean, test_sd,
   train_data, train_mean, train_sd,
   Y_test, Y_train)

combined_data <- merge(combined_data, activities)

##
## Create a second data set with the average of each variable for each activity and
## each subject
##

for (i in 1:length(unique(combined_data$activity_code))){
  
  if (i == 1) tidy_summary <- as.data.frame(mat.or.vec(nc = 5, nr = 0))
  if (i == 1) colnames(tidy_summary) <- c("activity_code", "activity",
                                          "subject", "average_mean",
                                          "average_sd")
  if (i == 1) activity_code <- unique(combined_data$activity_code)
  
  df.activity <- subset(combined_data, activity_code == i)
  
  for (j in 1:length(unique(df.activity$subject))){
    
    if (j == 1) subject <- sort(unique(df.activity$subject))
    
    df.activity.subject <- subset(df.activity, subject == j)
    
    tidy_row <- as.data.frame(mat.or.vec(nc = 5, nr = 1))
    colnames(tidy_row) <- c("activity_code", "activity",
                            "subject", "average_mean",
                            "average_sd")
    
    tidy_row[1,1] <- unique(df.activity.subject$activity_code)
    tidy_row[1,2] <- unique(df.activity.subject$activity)
    tidy_row[1,3] <- unique(df.activity.subject$subject)
    tidy_row[1,4] <- mean(df.activity.subject$mean)
    tidy_row[1,5] <- mean(df.activity.subject$standard_deviation)
    
    tidy_summary <- rbind(tidy_summary, tidy_row)
    
    rm(tidy_row, df.activity.subject)
    
  }
  
  rm(df.activity)
  
}

# Write out combined data table as a comma-delimited text file
write.table(combined_data, "combined_data.txt", sep = ',', row.names = FALSE)

# Write out the summary data table as a comma-delimited text file
write.table(tidy_summary, "data_summary.txt", sep = ',', row.names = FALSE)

