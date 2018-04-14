## How Much Does It Rain II competition on Kaggle

## Code: Base Model - XGBoost
## Authors: Pietro Marinelli and Rohan Rao

## loading libraries
library(data.table)
library(fastmatch)
library(zoo)


## setting parameters (edit these to your paths)
path_train <- "./train.csv"
path_test <- "./test.csv"


## function for time difference
time_difference <- function(times, num_per_segment = 60) {
  n <- length(times)
  valid_time <- vector(mode="numeric", length = n)
  valid_time[1] <- times[1]
  valid_time[-1] <- diff(times, 1)
  valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)
  valid_time <- valid_time / num_per_segment
  valid_time
}


## function for fast matching
`%fin%` <- function(x, lkup) {
  fmatch(x, lkup, nomatch = 0L) > 0L
}


## function for marshall palmer
marshall_palmer <- function(dbz) {
  ((10**(dbz/10))/200) ** 0.625
}


## loading train data
train <- fread(path_train)

# removing outliers
valid_vals <- 0.254 * 1:250
train <- train[round(Expected, 4) %fin% valid_vals]

# creating all the features
train$dt <- time_difference(train$minutes_past)
train$mp <- marshall_palmer(train$Ref)

train <- train[, .(
  target = log1p(mean(Expected, na.rm = T)),
  ref = mean(Ref, na.rm = T),
  maxref = max(Ref, na.rm = T),
  minref = min(Ref, na.rm = T),
  sdref = sd(Ref, na.rm = T),
  ref1 = mean(RefComposite, na.rm = T),
  maxref1 = max(RefComposite, na.rm = T),
  minref1 = min(RefComposite, na.rm = T),
  sdref1 = sd(RefComposite, na.rm = T),
  mp = mean(mp, na.rm = T),
  rd = mean(radardist_km, na.rm = T),
  records = .N
), Id]

train <- subset(train, !is.na(ref))

# merging the sample dask features
sd <- fread("./Data/train_ref.csv")
sd1 <-  fread("./Data/train_ref1.csv")
sd5 <-  fread("./Data/train_ref5.csv")
sd9 <-  fread("./Data/train_ref9.csv")
sdc <- fread("./Data/train_refcomp.csv")
sdc1 <-  fread("./Data/train_refcomp1.csv")
sdc5 <-  fread("./Data/train_refcomp5.csv")
sdc9 <-  fread("./Data/train_refcomp9.csv")

names(sd)[2] <- "sd"
names(sd1)[2] <- "sd1"
names(sd5)[2] <- "sd5"
names(sd9)[2] <- "sd9"
names(sdc)[2] <- "sdc"
names(sdc1)[2] <- "sdc1"
names(sdc5)[2] <- "sdc5"
names(sdc9)[2] <- "sdc9"

train <- merge(train, sd, by="Id")
train <- merge(train, sd1, by="Id")
train <- merge(train, sd5, by="Id")
train <- merge(train, sd9, by="Id")
train <- merge(train, sdc, by="Id")
train <- merge(train, sdc1, by="Id")
train <- merge(train, sdc5, by="Id")
train <- merge(train, sdc9, by="Id")

# preparing the train dataframe
y <- train$target
train_ids <- train$Id

train <- as.data.frame(subset(train, select=-c(Id, target)))


## loading test data
test <- fread(path_test)

# creating all the features
test$dt <- time_difference(test$minutes_past)
test$mp <- marshall_palmer(test$Ref)

test <- test[, .(
  ref = mean(Ref, na.rm = T),
  maxref = max(Ref, na.rm = T),
  minref = min(Ref, na.rm = T),
  sdref = sd(Ref, na.rm = T),
  ref1 = mean(RefComposite, na.rm = T),
  maxref1 = max(RefComposite, na.rm = T),
  minref1 = min(RefComposite, na.rm = T),
  sdref1 = sd(RefComposite, na.rm = T),
  mp = mean(mp, na.rm = T),
  rd = mean(radardist_km, na.rm = T),
  records = .N
), Id]


# merging the sample dask features
sd <- fread("./Data/test_ref.csv")
sd1 <-  fread("./Data/test_ref1.csv")
sd5 <-  fread("./Data/test_ref5.csv")
sd9 <-  fread("./Data/test_ref9.csv")
sdc <- fread("./Data/test_refcomp.csv")
sdc1 <-  fread("./Data/test_refcomp1.csv")
sdc5 <-  fread("./Data/test_refcomp5.csv")
sdc9 <-  fread("./Data/test_refcomp9.csv")

names(sd)[2] <- "sd"
names(sd1)[2] <- "sd1"
names(sd5)[2] <- "sd5"
names(sd9)[2] <- "sd9"
names(sdc)[2] <- "sdc"
names(sdc1)[2] <- "sdc1"
names(sdc5)[2] <- "sdc5"
names(sdc9)[2] <- "sdc9"

test <- merge(test, sd, by="Id")
test <- merge(test, sd1, by="Id")
test <- merge(test, sd5, by="Id")
test <- merge(test, sd9, by="Id")
test <- merge(test, sdc, by="Id")
test <- merge(test, sdc1, by="Id")
test <- merge(test, sdc5, by="Id")
test <- merge(test, sdc9, by="Id")

# preparing the test dataframe
test_ids <- test$Id
test <- as.data.frame(subset(test, select=-c(Id)))


## xgboost cross validation
source("./Codes/XGBoost_CV.R")
model_xgb <- XGBoost(train,y,test,cv=5,objective="reg:linear",nrounds=5,eta=0.01,max_depth=10,subsample=0.8,min_child_weight=10,metric="mae")

train_xgb <- data.frame("Id" = train_ids, "pred_xgb" = model_xgb$train$pred_xgb)
test_xgb <- data.frame("Id" = test_ids, "pred_xgb" = model_xgb$test$pred_xgb)

write.csv(train_xgb, "./Data/train_xgb.csv", row.names=F)
write.csv(test_xgb, "./Data/test_xgb.csv", row.names=F)

