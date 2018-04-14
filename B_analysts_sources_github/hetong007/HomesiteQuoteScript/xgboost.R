library(readr)
library(xgboost)

#my favorite seed^^

cat("reading the train and test data\n")
train <- read_csv("../input/train.csv")
test  <- read_csv("../input/test.csv")

y = train[,3]
x = rbind(train[,-c(1,3)],test[,-1])
trind = 1:nrow(train)
teind = setdiff(1:nrow(x), trind)
id = c(train[,1],test[,1])

save(x,y,trind,teind,id,file='../input/ori.data.rda')

load('../input/ori.data.rda')

# There are some NAs in the integer columns so conversion to zero
#train[is.na(train)]   <- 0
#test[is.na(test)]   <- 0
x$PersonalField84[is.na(x$PersonalField84)] = 1
x$PropertyField29[is.na(x$PropertyField29)] = 0

# cat("train data column names and details\n")
# names(train)
# str(train)
# summary(train)
# cat("test data column names and details\n")
# names(test)
# str(test)
# summary(test)


# seperating out the elements of the date column for the data set
x$month <- as.integer(format(x$Original_Quote_Date, "%m"))
x$year <- as.integer(format(x$Original_Quote_Date, "%y"))
x$day <- weekdays(as.Date(x$Original_Quote_Date))

# removing the date column
x <- x[,-1]

p = ncol(x)
cat.list = NULL
cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in 1:p) {
  if (class(x[[f]])=="character") {
    cat.list = c(cat.list, f)
    x[[f]] <- as.integer(as.factor(x[[f]]))
  }
}

save(x,y,trind,teind,id,cat.list, file='../input/xgboost.dat.rda')

set.seed(512)

val.ind <-sample(trind,2000)
nonval.ind <- setdiff(trind, val.ind)

dval<-xgb.DMatrix(data=data.matrix(x[val.ind,]),label=y[val.ind])
dtrain<-xgb.DMatrix(data=data.matrix(x[nonval.ind,]),label=y[nonval.ind])

watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.1, # 0.06, #0.01,
                max_depth           = 8, #changed from default of 8
                subsample           = 0.8, # 0.7
                colsample_bytree    = 0.8 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1000, #300, #280, #125, #250, # changed from 300
                    early.stop.round    = 700,
                    watchlist           = watchlist,
                    maximize            = TRUE
)

pred1 <- predict(clf, data.matrix(x[teind,]), ntreelimit = clf$bestInd)
submission <- data.frame(QuoteNumber=id[teind], QuoteConversion_Flag=pred1)
cat("saving the submission file\n")
write_csv(submission, "xgb1.csv")
