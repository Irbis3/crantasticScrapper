getwd()
setwd("/Users/Tammy/online_courses/data_science/UCI_HAR_Dataset/")
current_dir<- "/Users/Tammy/online_courses/data_science/UCI_HAR_Dataset/"
# check what are in the directory. or list.files()
dir()

## read in the test data
test<- read.table(paste(current_dir, "test/X_test.txt", sep="/"), header=F) 

## how many rows and columns? 
dim(test)

## read in the subject ID number for the test data 
subject<- read.table(paste(current_dir,"test/subject_test.txt", sep="/"), header=F)
names(subject)<- "subject_ID"
## read in the activity label for the test data
activity_label_ID<- read.table(paste(current_dir,"test/y_test.txt", sep="/"), header=F)
names(activity_label_ID)<- "activity_label"
### now combine the subject ID, activity_label_ID with the test data
test_data<- data.frame(subject, activity_label_ID, test)

############################################
### Now read in the training data the same as above for the test data

train<- read.table(paste(current_dir, "train/X_train.txt", sep="/"), header=F) 
subject2<- read.table(paste(current_dir,"train/subject_train.txt", sep="/"), header=F)
names(subject2)<- "subject_ID"
activity_label_ID2<- read.table(paste(current_dir,"train/y_train.txt", sep="/"), header=F)
names(activity_label_ID2)<- "activity_label"
train_data<- data.frame(subject2, activity_label_ID2, train)

########################################################
# the subject ID of people in test and train data
unique(test_data$subject_ID)
unique(train_data$subject_ID)

## confirming that 30 * 70% =21 in train data, 30 * 30% =9 in test data

### now combine test and train data 
whole_data<- rbind(train_data, test_data)

### add the feature label to the combined dataframe
features<- read.table(paste(current_dir,"features.txt", sep="/"), header=F, stringsAsFactors=F)
names(whole_data)<- c("subject_ID", "activity_label",features$V2)

### extract only the measurements on the mean and standard deviation for each measurement.
measure_mean<- grep("mean", names(whole_data))
measure_std<- grep("std", names(whole_data))

# including the first 2 columns and the columns contain mean and std
whole_data<- whole_data[,c(1,2,measure_mean,measure_std)]
##############################################
### read in the activity_labels dataframe
activity_label<- read.table(paste(current_dir,"activity_labels.txt", sep="/"), header=F, stringsAsFactors=F)
mapping<- activity_label$V2

## initiate an empty character vector, and add the descriptive names
descriptive_names<-character()
for (i in whole_data$activity_label) { 
        descriptive_names<-c(descriptive_names,mapping[i])}
## change the dataframe activity label to descriptive names
whole_data$activity_label<- descriptive_names

whole_data$subject_ID<- as.factor(whole_data$subject_ID)
whole_data$activity_label<- as.factor(whole_data$activity_label)

library(dplyr)
tidy_data<- whole_data %>% group_by(subject_ID, activity_label) %>% summarise_each(funs(mean))
write.table(tidy_data, "tidy_data.txt", sep="\t", row.name=F)


