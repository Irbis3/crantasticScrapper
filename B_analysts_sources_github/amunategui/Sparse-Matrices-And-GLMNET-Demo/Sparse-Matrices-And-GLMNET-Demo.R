
some_dataframe <- read.table(text="c1        c2     c3     c4     c5     c6     c7     c8     c9     c10     outcome
2     7     0     0     0     0     0     0     0     0     0
0     0     3     0     0     0     0     0     0     0     0
0     0     0     6     1     0     0     0     0     0     0
0     0     0     2     0     0     0     0     0     0     0
0     0     0     0     0     0     0     0     12     0     1
0     0     0     0     0     25     0     0     0     0     1
1     0     0     0     2     0     0     0     0     0     0
0     0     0     2     0     0     0     0     0     0     0
0     0     0     0     0     0     0     0     14     0     1
0     0     0     0     0     21     0     0     0     0     1
0     0     0     0     0     0     28     0     0     0     1
0     0     0     0     0     0     0     35     0     0     1
0     0     0     0     0     0     0     0     42     0     1
0     0     0     0     0     0     0     0     0     49     1", header=T, sep="") 

library(Matrix)
some_matrix <- data.matrix(some_dataframe[1:10])

# show matrix representation of data set
Matrix(some_matrix, sparse=TRUE)

# split data set into a train and test portion
set.seed(2)
split <- sample(nrow(some_dataframe), floor(0.7*nrow(some_dataframe)))
train <-some_dataframe[split,]
test <- some_dataframe[-split,]

# transform both sets into sparse matrices using the sparse.model.matrix
train_sparse <- sparse.model.matrix(~.,train[1:10])
test_sparse <- sparse.model.matrix(~.,test[1:10])

# model the sparse sets using glmnet
library(glmnet)  
fit <- glmnet(train_sparse,train[,11])

# use cv.glmnet to find best lambda/penalty 
# s is the penalty parameter
cv <- cv.glmnet(train_sparse,train[,11],nfolds=3)
pred <- predict(fit, test_sparse,type="response", s=cv$lambda.min)

#  receiver operating characteristic (ROC curves)
library(pROC)  
auc = roc(test[,11], pred)
print(auc$auc)

# how does sparse deal with categorical data (adding mood feature with two levels)?
cat_dataframe<- read.table(text="c1     c2     c3     c4     c5     c6     c7     c8     c9     c10     mood     outcome
2     7     0     0     0     0     0     0     0     0     happy     0
0     0     3     0     0     0     0     0     0     0     happy     0
0     0     0     6     1     0     0     0     0     0     happy     0
0     0     0     2     0     0     0     0     0     0     happy     0
0     0     0     0     0     0     0     0     12     0     sad     1
0     0     0     0     0     25     0     0     0     0     sad     1
1     0     0     0     2     0     0     0     0     0     happy     0
0     0     0     2     0     0     0     0     0     0     happy     0
0     0     0     0     0     0     0     0     14     0     sad     1
0     0     0     0     0     21     0     0     0     0     sad     1
0     0     0     0     0     0     28     0     0     0     sad     1
0     0     0     0     0     0     0     35     0     0     sad     1
0     0     0     0     0     0     0     0     42     0     sad     1
0     0     0     0     0     0     0     0     0     49     sad     1", header=T, sep="") 
print(sparse.model.matrix(~.,cat_dataframe))

# increasing the number of levels in the mood variable)
cat_dataframe <- read.table(text="c1     c2     c3     c4     c5     c6     c7     c8     c9     c10     mood     outcome
2     7     0     0     0     0     0     0     0     0     angry     0
0     0     3     0     0     0     0     0     0     0     neutral     0
0     0     0     6     1     0     0     0     0     0     happy     0
0     0     0     2     0     0     0     0     0     0     happy     0
0     0     0     0     0     0     0     0     12     0     sad     1
0     0     0     0     0     25     0     0     0     0     sad     1
1     0     0     0     2     0     0     0     0     0     happy     0
0     0     0     2     0     0     0     0     0     0     happy     0
0     0     0     0     0     0     0     0     14     0     sad     1
0     0     0     0     0     21     0     0     0     0     neutral     1
0     0     0     0     0     0     28     0     0     0     sad     1
0     0     0     0     0     0     0     35     0     0     sad     1
0     0     0     0     0     0     0     0     42     0     sad     1
0     0     0     0     0     0     0     0     0     49     sad     1", header=T, sep="") 
print(levels(cat_dataframe$mood))
dim(cat_dataframe)
# sparse added extra columns when in binarized mood
dim(sparse.model.matrix(~.,cat_dataframe))


