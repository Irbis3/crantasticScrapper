require(RCurl)
binData <- getBinaryURL("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",
                        ssl.verifypeer=FALSE)

conObj <- file("dataset_diabetes.zip", open = "wb")
writeBin(binData, conObj)
# don't forget to close it
close(conObj)

# open diabetes file
files <- unzip("dataset_diabetes.zip")
diabetes <- read.csv(files[1], stringsAsFactors = FALSE)

# drop useless variables
diabetes <- subset(diabetes,select=-c(encounter_id, patient_nbr))

# transform all "?" to 0s
diabetes[diabetes == "?"] <- NA

# remove zero variance - ty James http://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame
diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]

# prep outcome variable to those readmitted under 30 days
diabetes$readmitted <- ifelse(diabetes$readmitted == "<30",1,0)

# generalize outcome name
outcomeName <- 'readmitted'

# drop large factors
diabetes <- subset(diabetes, select=-c(diag_1, diag_2, diag_3))

# binarize data
charcolumns <- names(diabetes[sapply(diabetes, is.character)])
for (colname in charcolumns) {
        print(paste(colname,length(unique(diabetes[,colname]))))
        for (newcol in unique(diabetes[,colname])) {
                if (!is.na(newcol))
                        diabetes[,paste0(colname,"_",newcol)] <- ifelse(diabetes[,colname]==newcol,1,0)
        }
        diabetes <- diabetes[,setdiff(names(diabetes),colname)]
}

# remove all punctuation characters in column names after binarization that could trip R
colnames(diabetes) <- gsub(x =colnames(diabetes), pattern="[[:punct:]]", replacement = "_" )

# check for zero variance
diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]

# transform all NAs into 0
diabetes[is.na(diabetes)] <- 0 

# split data set into training and testing
set.seed(1234)
split <- sample(nrow(diabetes), floor(0.5*nrow(diabetes)))
traindf <- diabetes[split,]
testdf <-  diabetes[-split,]

predictorNames <- setdiff(names(traindf), outcomeName)
fit <- lm(readmitted ~ ., data = traindf)
preds <- predict(fit, testdf[,predictorNames], se.fit = TRUE)

library(pROC)
print(auc(testdf[,outcomeName], preds$fit))

# parallel   ---------------------------------------------------------
library(foreach)
library(doParallel)

#setup parallel back end to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

# divide row size by 20, sample data 400 times 
# code based on Vik Paruchuri's blog entry: http://www.vikparuchuri.com/blog/build-your-own-bagging-function-in-r/
length_divisor <- 20
predictions<-foreach(m=1:400,.combine=cbind) %dopar% { 
        # using sample function without seed
        sampleRows <- sample(nrow(traindf), size=floor((nrow(traindf)/length_divisor)))
        fit <- lm(readmitted ~ ., data = traindf[sampleRows,])
        predictions <- data.frame(predict(object=fit, testdf[,predictorNames], se.fit = TRUE)[[1]])
} 
stopCluster(cl)

library(pROC)
print(auc(testdf[,outcomeName], rowMeans(predictions)))