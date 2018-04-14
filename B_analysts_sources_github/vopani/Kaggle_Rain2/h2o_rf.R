## How Much Does It Rain II competition on Kaggle

## Code: Final Model - h2o RandomForest
## Authors: Pietro Marinelli and Rohan Rao
## Position: 30th

## loading libraries
library(data.table)
library(h2o)


## setting parameters (edit these to your paths)
path_train <- "./train.csv"
path_test <- "./test.csv"

# h2o RF
nthreads <- -1
ntrees <- 1000
max_depth <- 20
min_rows <- 4
sample_rate <- 0.7


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


## loading train data
train <- fread(path_train)

# cleaning data
train$Ref_5x5_10th[train$Ref_5x5_10th < 0] <- NA
train$Ref_5x5_50th[train$Ref_5x5_50th < 0] <- NA
train$Ref_5x5_90th[train$Ref_5x5_90th < 0] <- NA
train$RefComposite[train$RefComposite < 0] <- NA
train$RefComposite_5x5_10th[train$RefComposite_5x5_10th < 0] <- NA
train$RefComposite_5x5_50th[train$RefComposite_5x5_50th < 0] <- NA
train$RefComposite_5x5_90th[train$RefComposite_5x5_90th < 0] <- NA
train$Ref[train$Ref < 0] <- NA

# creating all the features
train$dt <- time_difference(train$minutes_past)

train <- train[,.(
  minutes1 = max(minutes_past, na.rm = T),
  minutes2 = min(minutes_past, na.rm = T),
  dt1 = max(dt*Ref, na.rm = T),
  dt2 = min(dt*Ref, na.rm = T),
  dtm = mean(dt*Ref, na.rm = T),
  dtmv = var(dt*Ref, na.rm = T),
  dist = mean(radardist_km, na.rm = T),
  distmax = max(radardist_km, na.rm = T),
  distmin = min(radardist_km, na.rm = T),
  meanRef = mean(Ref,na.rm=T),
  varRef = var(Ref,na.rm=T),
  minRef = min(Ref,na.rm=T),
  maxRef = max(Ref,na.rm=T),
  medRef = median(Ref,na.rm=T),
  ref1 = mean(Ref_5x5_10th, na.rm = T),
  ref1min = min(Ref_5x5_10th, na.rm = T),
  ref1max = max(Ref_5x5_10th, na.rm = T),
  ref1var = var(Ref_5x5_10th, na.rm = T),
  ref5 = mean(Ref_5x5_50th, na.rm = T),
  ref5min = min(Ref_5x5_50th, na.rm = T),
  ref5max = max(Ref_5x5_50th, na.rm = T),
  ref5var = var(Ref_5x5_50th, na.rm = T),
  ref9 = mean(Ref_5x5_90th, na.rm = T),
  ref9min = min(Ref_5x5_90th, na.rm = T),
  ref9max = max(Ref_5x5_90th, na.rm = T),
  ref9var = var(Ref_5x5_90th, na.rm = T),
  minRefcomp = max(RefComposite,na.rm=T),
  maxRefcomp = min(RefComposite,na.rm=T),
  meanRefcomp = mean(RefComposite,na.rm=T),
  medRefcomp = median(RefComposite,na.rm=T),
  varRefcomp = var(RefComposite,na.rm=T),
  refcomp1 = mean(RefComposite_5x5_10th,na.rm=T),
  refcomp1min = min(RefComposite_5x5_10th,na.rm=T),
  refcomp1max = max(RefComposite_5x5_10th,na.rm=T),
  refcomp1var = var(RefComposite_5x5_10th,na.rm=T),
  refcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
  refcomp5min = min(RefComposite_5x5_50th,na.rm=T),
  refcomp5max = max(RefComposite_5x5_50th,na.rm=T),
  refcomp5var = var(RefComposite_5x5_50th,na.rm=T),
  refcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
  refcomp9min = min(RefComposite_5x5_90th,na.rm=T),
  refcomp9max = max(RefComposite_5x5_90th,na.rm=T),
  refcomp9var = var(RefComposite_5x5_90th,na.rm=T),
  rho = mean(RhoHV,na.rm=T),
  rhomin = min(RhoHV,na.rm=T),
  rhomax = max(RhoHV,na.rm=T),
  rho1 = mean(RhoHV_5x5_10th,na.rm=T),
  rho1min = min(RhoHV_5x5_10th,na.rm=T),
  rho1max = max(RhoHV_5x5_10th,na.rm=T),
  rho5 = mean(RhoHV_5x5_50th,na.rm=T),
  rho5min = min(RhoHV_5x5_50th,na.rm=T),
  rho5max = max(RhoHV_5x5_50th,na.rm=T),
  rho9 = mean(RhoHV_5x5_90th,na.rm=T),
  rho9min = min(RhoHV_5x5_90th,na.rm=T),
  rho9max = max(RhoHV_5x5_90th,na.rm=T),
  zdr = mean(Zdr, na.rm = T),
  zdrmin = min(Zdr, na.rm = T),
  zdrmax = max(Zdr, na.rm = T),
  zdr1 = mean(Zdr_5x5_10th, na.rm = T),
  zdr1min = min(Zdr_5x5_10th, na.rm = T),
  zdr1max = max(Zdr_5x5_10th, na.rm = T),
  zdr5 = mean(Zdr_5x5_50th, na.rm = T),
  zdr5min = min(Zdr_5x5_50th, na.rm = T),
  zdr5max = max(Zdr_5x5_50th, na.rm = T),
  zdr9 = mean(Zdr_5x5_90th, na.rm = T),
  zdr9min = min(Zdr_5x5_90th, na.rm = T),
  zdr9max = max(Zdr_5x5_90th, na.rm = T),
  kdp = mean(Kdp,na.rm=T),
  kdpmin = min(Kdp,na.rm=T),
  kdpmax = max(Kdp,na.rm=T),
  kdp1 = mean(Kdp_5x5_10th,na.rm=T),
  kdp1min = min(Kdp_5x5_10th,na.rm=T),
  kdp1max = max(Kdp_5x5_10th,na.rm=T),
  kdp5 = mean(Kdp_5x5_50th,na.rm=T),
  kdp5min = min(Kdp_5x5_50th,na.rm=T),
  kdp5max = max(Kdp_5x5_50th,na.rm=T),
  kdp9 = mean(Kdp_5x5_90th,na.rm=T),
  kdp9min = min(Kdp_5x5_90th,na.rm=T),
  kdp9max = max(Kdp_5x5_90th,na.rm=T),
  records = .N,
  naCounts = sum(is.na(Ref)),
  target = log1p(mean(Expected))),Id][records>naCounts,]

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

# merging the xgb preds
train_xgb <- fread("./Data/train_xgb.csv")
train <- merge(train, train_xgb, by="Id")


## h2o random forest
h2o.init(nthreads=nthreads)

trainHex<-as.h2o(trainHex, destination_frame="train.hex")

x <- c("minutes1",
       "minutes2",
       "dt1",
       "dt2",
       "dtm",
       "dtmv",
       "dist",
       "distmax",
       "distmin",
       "meanRef",
       "varRef",
       "minRef",
       "maxRef",
       "ref1",
       "ref1min",
       "ref1max",
       "ref1var",
       "ref5",
       "ref5min",
       "ref5max",
       "ref5var",
       "ref9",
       "ref9min",
       "ref9max",
       "ref9var",
       "minRefcomp",
       "maxRefcomp",
       "medRefcomp",
       "meanRefcomp",
       "varRefcomp",
       "refcomp1",
       "refcomp1min",
       "refcomp1max",
       "refcomp1var",
       "refcomp5",
       "refcomp5min",
       "refcomp5max",
       "refcomp5var",
       "refcomp9",
       "refcomp9min",
       "refcomp9max",
       "refcomp9var",
       "rho",
       "rhomin",
       "rhomax",
       "rho1",
       "rho1min",
       "rho1max",
       "rho5",
       "rho5min",
       "rho5max",
       "rho9",
       "rho9min",
       "rho9max",
       "zdr",
       "zdrmin",
       "zdrmax",
       "zdr1",
       "zdr1min",
       "zdr1max",
       "zdr5",
       "zdr5min",
       "zdr5max",
       "zdr9",
       "zdr9min",
       "zdr9max",
       "kdp",
       "kdpmin",
       "kdpmax",
       "kdp1",
       "kdp1min",
       "kdp1max",
       "kdp5",
       "kdp5min",
       "kdp5max",
       "kdp9",
       "kdp9min",
       "kdp9max",
       "sd",
       "sd1",
       "sd5",
       "sd9",
       "sdc",
       "sdc1",
       "sdc5",
       "sdc9",
       "pred_xgb",
       "records",
       "naCounts")

rfHex<-h2o.randomForest(x, y="target", training_frame=trainHex,model_id="rf.hex", ntrees=ntrees, max_depth=max_depth, min_rows=min_rows, sample_rate=sample_rate)


## loading test data
test <- fread(path_test)

# cleaning data
test$Ref_5x5_10th[test$Ref_5x5_10th < 0] <- NA
test$Ref_5x5_50th[test$Ref_5x5_50th < 0] <- NA
test$Ref_5x5_90th[test$Ref_5x5_90th < 0] <- NA
test$RefComposite[test$RefComposite < 0] <- NA
test$RefComposite_5x5_10th[test$RefComposite_5x5_10th < 0] <- NA
test$RefComposite_5x5_50th[test$RefComposite_5x5_50th < 0] <- NA
test$RefComposite_5x5_90th[test$RefComposite_5x5_90th < 0] <- NA
test$Ref[test$Ref < 0] <- NA

# creating all the features
test$dt <- time_difference(test$minutes_past)

test <- test[,.(
  minutes1 = max(minutes_past, na.rm = T),
  minutes2 = min(minutes_past, na.rm = T),
  dt1 = max(dt*Ref, na.rm = T),
  dt2 = min(dt*Ref, na.rm = T),
  dtm = mean(dt*Ref, na.rm = T),
  dtmv = var(dt*Ref, na.rm = T),
  dist = mean(radardist_km, na.rm = T),
  distmax = max(radardist_km, na.rm = T),
  distmin = max(radardist_km, na.rm = T),
  meanRef = mean(Ref,na.rm=T),
  varRef = var(Ref,na.rm=T),
  minRef = min(Ref,na.rm=T),
  maxRef = max(Ref,na.rm=T),
  ref1 = mean(Ref_5x5_10th, na.rm = T),
  ref1min = min(Ref_5x5_10th, na.rm = T),
  ref1max = max(Ref_5x5_10th, na.rm = T),
  ref1var = var(Ref_5x5_10th, na.rm = T),
  ref5 = mean(Ref_5x5_50th, na.rm = T),
  ref5min = min(Ref_5x5_50th, na.rm = T),
  ref5max = max(Ref_5x5_50th, na.rm = T),
  ref5var = var(Ref_5x5_50th, na.rm = T),
  ref9 = mean(Ref_5x5_90th, na.rm = T),
  ref9min = min(Ref_5x5_90th, na.rm = T),
  ref9max = max(Ref_5x5_90th, na.rm = T),
  ref9var = var(Ref_5x5_90th, na.rm = T),
  minRefcomp = max(RefComposite,na.rm=T),
  maxRefcomp = min(RefComposite,na.rm=T),
  meanRefcomp = mean(RefComposite,na.rm=T),
  medRefcomp = median(RefComposite,na.rm=T),
  varRefcomp = var(RefComposite,na.rm=T),
  refcomp1 = mean(RefComposite_5x5_10th,na.rm=T),
  refcomp1min = min(RefComposite_5x5_10th,na.rm=T),
  refcomp1max = max(RefComposite_5x5_10th,na.rm=T),
  refcomp1var = var(RefComposite_5x5_10th,na.rm=T),
  refcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
  refcomp5min = min(RefComposite_5x5_50th,na.rm=T),
  refcomp5max = max(RefComposite_5x5_50th,na.rm=T),
  refcomp5var = var(RefComposite_5x5_50th,na.rm=T),
  refcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
  refcomp9min = min(RefComposite_5x5_90th,na.rm=T),
  refcomp9max = max(RefComposite_5x5_90th,na.rm=T),
  refcomp9var = var(RefComposite_5x5_90th,na.rm=T),
  rho = mean(RhoHV,na.rm=T),
  rhomin = min(RhoHV,na.rm=T),
  rhomax = max(RhoHV,na.rm=T),
  rho1 = mean(RhoHV_5x5_10th,na.rm=T),
  rho1min = min(RhoHV_5x5_10th,na.rm=T),
  rho1max = max(RhoHV_5x5_10th,na.rm=T),
  rho5 = mean(RhoHV_5x5_50th,na.rm=T),
  rho5min = min(RhoHV_5x5_50th,na.rm=T),
  rho5max = max(RhoHV_5x5_50th,na.rm=T),
  rho9 = mean(RhoHV_5x5_90th,na.rm=T),
  rho9min = min(RhoHV_5x5_90th,na.rm=T),
  rho9max = max(RhoHV_5x5_90th,na.rm=T),
  zdr = mean(Zdr, na.rm = T),
  zdrmin = min(Zdr, na.rm = T),
  zdrmax = max(Zdr, na.rm = T),
  zdr1 = mean(Zdr_5x5_10th, na.rm = T),
  zdr1min = min(Zdr_5x5_10th, na.rm = T),
  zdr1max = max(Zdr_5x5_10th, na.rm = T),
  zdr5 = mean(Zdr_5x5_50th, na.rm = T),
  zdr5min = min(Zdr_5x5_50th, na.rm = T),
  zdr5max = max(Zdr_5x5_50th, na.rm = T),
  zdr9 = mean(Zdr_5x5_90th, na.rm = T),
  zdr9min = min(Zdr_5x5_90th, na.rm = T),
  zdr9max = max(Zdr_5x5_90th, na.rm = T),
  kdp = mean(Kdp,na.rm=T),
  kdpmin = min(Kdp,na.rm=T),
  kdpmax = max(Kdp,na.rm=T),
  kdp1 = mean(Kdp_5x5_10th,na.rm=T),
  kdp1min = min(Kdp_5x5_10th,na.rm=T),
  kdp1max = max(Kdp_5x5_10th,na.rm=T),
  kdp5 = mean(Kdp_5x5_50th,na.rm=T),
  kdp5min = min(Kdp_5x5_50th,na.rm=T),
  kdp5max = max(Kdp_5x5_50th,na.rm=T),
  kdp9 = mean(Kdp_5x5_90th,na.rm=T),
  kdp9min = min(Kdp_5x5_90th,na.rm=T),
  kdp9max = max(Kdp_5x5_90th,na.rm=T),
  records = .N,
  naCounts = sum(is.na(Ref))),Id]

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

# merging the xgb preds
test_xgb <- fread("./Data/test_xgb.csv")
test <- merge(test, test_xgb, by="Id")


## scoring the test data
testHex <- as.h2o(testHex, destination_frame="test.hex")
predictions <- as.data.frame(h2o.predict(rfHex,testHex))

# submission
submission <- fread("./Data/sample_solution.csv")
submission$Expected <- expm1(predictions$predict) 
submission$Expected <- round(submission$Expected / 0.254) * 0.254

write.csv(submission, "./Submission/submit.csv")
