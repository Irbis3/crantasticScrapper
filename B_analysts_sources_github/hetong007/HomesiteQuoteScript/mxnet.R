library(readr)
library(mxnet)

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

save(x,y,trind,teind,cat.list,id, file='../input/xgboost.dat.rda')

load('../input/xgboost.dat.rda')

# one hot encoding

ohe = function(x, name, order = FALSE) {
  res = model.matrix(~.-1,data.frame(factor(x)))
  res = res[,-ncol(res),drop=FALSE]
  if (order && ncol(res)>=2) {
    for (i in 2:ncol(res)) {
      res[,i] = as.numeric(res[,i] | res[,i-1])
    }
  }
  colnames(res) = paste(name, 1:ncol(res), sep='_')
  return(res)
}

tmpx = lapply(cat.list, function(i) ohe(x[,i], colnames(x)[i]))
tmpx = do.call(cbind,tmpx)
x = cbind(x[,-cat.list], tmpx)

# Normalization

sds = apply(x,2,sd)
ind = which(sds == 0)
x = x[,-ind]

normDat = function(x) {
  mu = mean(x)
  sig = sd(x)
  res = (x-mu)/sig
  return(res)
}

for (i in 1:ncol(x)) {
  cat(i,'\r')
  x[,i] = normDat(x[,i])
}

save(x,y,trind,teind,id, file='../input/numeric.dat.rda')

# Train mxnet

load('../input/numeric.dat.rda')

set.seed(0)
val.ind <-sample(trind,5000)
nonval.ind <- setdiff(trind, val.ind)
train = data.matrix(x[nonval.ind,])
val = data.matrix(x[val.ind,])
val.dat = list(data=val,
               label=y[val.ind])

# param tuning

hidden_node.seq = c(200,300,400)
array.batch.size.seq = c(10,30,50,100)
learning.rate.seq = c(0.005, 0.01, 0.03, 0.1)
momentum.seq = c(0.1, 0.5, 0.9)
dropout.seq = c(0, 0.1, 0.5)

L = 20
results = matrix(0, L, 6)
for (l in 1:L) {
  mx.set.seed(l)
  set.seed(l)
  
  hidden_node = sample(hidden_node.seq,1)
  array.batch.size = sample(array.batch.size.seq,1)
  learning.rate = sample(learning.rate.seq,1)
  momentum = sample(momentum.seq,1)
  dropout = sample(dropout.seq,1)
  
  model <- mx.mlp(train, y[nonval.ind], hidden_node=hidden_node, 
                  out_node=2, out_activation="softmax",
                  num.round=10, array.batch.size=array.batch.size, 
                  learning.rate=learning.rate, momentum=momentum, 
                  dropout = dropout, activation = "tanh",
                  eval.data = val.dat, eval.metric=mx.metric.accuracy)
  pred1 <- predict(model, val)
  pred.obj = ROCR::prediction(pred1[2,], y[val.ind])
  auc.obj = ROCR::performance(pred.obj, measure = 'auc')
  auc.val = auc.obj@y.values[[1]]
  
  results[l, ] = c(hidden_node, array.batch.size, learning.rate, momentum, 
                   dropout, auc.val)
  cat(l,results[l,],'\n')
}

results

preds = rep(0, length(teind))
L = 1
for (i in 1:L) {
  mx.set.seed(i)
  model <- mx.mlp(data.matrix(x[nonval.ind,]), y[nonval.ind], hidden_node=400, 
                  out_node=2, out_activation="softmax",
                  num.round=12, array.batch.size=50, 
                  learning.rate=0.01, momentum=0.5, 
                  dropout = 0.5, activation = "tanh",
                  eval.data = val.dat, eval.metric=mx.metric.accuracy)
  pred1 <- predict(model, data.matrix(x[teind,]))
  preds = preds + pred1[2,]
}

submission <- data.frame(QuoteNumber=id[teind], QuoteConversion_Flag=preds/L)
cat("saving the submission file\n")
write_csv(submission, "mxnet1.csv")
