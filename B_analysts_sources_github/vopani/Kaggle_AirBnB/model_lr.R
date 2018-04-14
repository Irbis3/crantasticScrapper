## setting working directory
path <- "./Kaggle_AirBnB"
setwd(path)


## loading libraries
library(bit64)
library(data.table)


## loading data
sessions <- fread("./sessions.csv")
X_test <- fread("./test_users.csv")
X_train <- fread("./train_users_2.csv")


## cleaning data

# keeping users from 2014 onwards
X_train <- subset(X_train, as.integer(substr(date_account_created,1,4)) >= 2014)
X_test$country_destination <- "NDF"

# keeping users present in sessions data
X_train <- subset(X_train, id %in% unique(sessions$user_id), select=c("id","country_destination"))
X_test <- subset(X_test, id %in% unique(sessions$user_id), select=c("id","country_destination"))

# features from sessions data
names(sessions)[1] <- "id"
sessions <- subset(sessions, id %in% c(unique(X_train$id), unique(X_test$id)))
sessions$count <- 1

# one-hot encoding action, action_type and action_detail
sessions_action <- dcast(sessions, id ~ action, mean, value.var="count")
sessions_action_type <- dcast(sessions, id ~ action_type, mean, value.var="count")
sessions_action_detail <- dcast(sessions, id ~ action_detail, mean, value.var="count")

# merging with train and test data
X_train <- merge(X_train, sessions_action, all.x=T, by="id")
X_test <- merge(X_test, sessions_action, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_type, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_type, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_detail, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_detail, all.x=T, by="id")

X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0

# removing duplicate columns
X_train <- X_train[, colnames(unique(as.matrix(X_train), MARGIN=2)), with=F]
X_test <- X_test[, names(X_train), with=F]

# extracting ids and target
train_ids <- X_train$id
test_ids <- X_test$id

target <- ifelse(X_train$country_destination == "NDF", 0, 1)

# preparing final datasets
X_train <- X_train[, ":="(id = NULL, country_destination = NULL)]
X_test <- X_test[, ":="(id = NULL, country_destination = NULL)]

# removing variables with less than 4 occurrences
X_train <- as.data.frame(subset(X_train, select=c(names(X_train)[which(colSums(X_train) > 4)])))
X_test <- as.data.frame(X_test[, .SD, .SDcols=names(X_train)])


## logistic regression
source("./LogisticRegression.R")
model_lr <- LogisticRegression(X_train, target, X_test, cv=5, seed=235, metric="logloss")

# CV score: 0.56859

# extracting train and test predictions
train_lr <- model_lr$train
test_lr <- model_lr$test

# saving predictions
train_lr <- data.frame("id"=train_ids, "pred_lr"=train_lr$pred_lr)
test_lr <- data.frame("id"=test_ids, "pred_lr"=test_lr$pred_lr)

write.csv(train_lr, "./train_lr.csv", row.names=F)
write.csv(test_lr, "./test_lr.csv", row.names=F)

