## setting working directory
path <- "./Kaggle_Telstra"
setwd(path)


## loading libraries
library(data.table)
library(plyr)
library(xgboost)


## loading data
X_train <- fread("./train.csv")
X_test <- fread("./test.csv")

event <- fread("./event_type.csv")
log <- fread("./log_feature.csv")
resource <- fread("./resource_type.csv")
severity <- fread("./severity_type.csv")


# event features
event$event_type <- gsub(" ", "_", event$event_type)
X_event <- dcast(event, id ~ event_type, length, value.var="id", fill=0)

X_train <- merge(X_train, X_event, by="id")
X_test <- merge(X_test, X_event, by="id")

# log features
log$log_feature <- gsub(" ", "_", log$log_feature)
X_log <- dcast(log, id ~ log_feature, mean, value.var="volume", fill=0)
X_log$log_count <- rowSums(X_log[,-1,with=F])

X_train <- merge(X_train, X_log, by="id")
X_test <- merge(X_test, X_log, by="id")

# log max
X_max <- log[, .(max_log = max(volume)), .(id)]

X_train <- merge(X_train, X_max, by="id")
X_test <- merge(X_test, X_max, by="id")

# log count
X_log_count <- log[, .(log_unique = .N), .(id)]

X_train <- merge(X_train, X_log_count, by="id")
X_test <- merge(X_test, X_log_count, by="id")

# log order
X_log_order <- as.data.frame(subset(log, select=c("id","log_feature")))
X_log_order <- X_log_order[!duplicated(X_log_order[,"id"]),]
X_log_order$log_order <- seq(1, nrow(X_log_order))
X_log_order$log_feature <- NULL

X_train <- merge(X_train, X_log_order, by="id")
X_test <- merge(X_test, X_log_order, by="id")

# location order
X_panel <- rbind.fill(X_train, X_test)
X_panel <- data.frame(X_panel[order(X_panel$log_order),])

X_panel$location_order <- 1

X_panel$fault_after <- 0
X_panel$fault_before <- 0

X_panel$f54_prev <- -1
X_panel$f82_prev <- -1
X_panel$f170_prev <- -1
X_panel$f203_prev <- -1
X_panel$f232_prev <- -1

X_panel$log_prev <- -1
X_panel$unique_prev <- -1

X_panel$event_type_11_prev <- -999
X_panel$event_type_35_prev <- -999
X_panel$event_type_54_prev <- -999

for (i in 2:nrow(X_panel))
{
  if (X_panel$location[i] == X_panel$location[i-1])
  {
    X_panel$location_order[i] <- X_panel$location_order[i-1] + 1

    X_panel$f54_prev[i] <- X_panel$feature_54[i] / X_panel$feature_54[i-1]
    X_panel$f82_prev[i] <- X_panel$feature_82[i] / X_panel$feature_82[i-1]
    X_panel$f170_prev[i] <- X_panel$feature_170[i] / X_panel$feature_170[i-1]
    X_panel$f203_prev[i] <- X_panel$feature_203[i] / X_panel$feature_203[i-1]
    X_panel$f232_prev[i] <- X_panel$feature_232[i] / X_panel$feature_232[i-1]

    X_panel$log_prev[i] <- X_panel$log_count[i] / X_panel$log_count[i-1]
    X_panel$unique_prev[i] <- X_panel$log_unique[i] / X_panel$log_unique[i-1]

    X_panel$event_type_11_prev[i] <- X_panel$event_type_11[i] - X_panel$event_type_11[i-1]
    X_panel$event_type_35_prev[i] <- X_panel$event_type_35[i] - X_panel$event_type_35[i-1]
    X_panel$event_type_54_prev[i] <- X_panel$event_type_54[i] - X_panel$event_type_54[i-1]

    if (!is.na(X_panel$fault_severity[i-1]) & X_panel$fault_severity[i-1] != 0)
    {
      X_panel$fault_before[i] <- X_panel$fault_severity[i-1]
    }
    if (!is.na(X_panel$fault_severity[i]) & X_panel$fault_severity[i] != 0)
    {
      X_panel$fault_after[i-1] <- X_panel$fault_severity[i]
    }
  }else
  {
    X_panel$location_order[i] <- 1
  }
}

X_train <- data.table(subset(X_panel, !is.na(fault_severity)))
X_test <- data.table(subset(X_panel, is.na(fault_severity)))

# first fault
X_fault <- X_train[fault_severity != 0, .(first_fault = min(location_order)), .(location)]

X_train <- merge(X_train, X_fault, all.x=T, by="location")
X_test <- merge(X_test, X_fault, all.x=T, by="location")

X_train$fault_flag <- ifelse(X_train$location_order > X_train$first_fault, 1, 0)
X_test$fault_flag <- ifelse(X_test$location_order > X_test$first_fault, 1, 0)

X_train$fault_flag[is.na(X_train$fault_flag)] <- 0
X_test$fault_flag[is.na(X_test$fault_flag)] <- 0

# resource features
resource$resource_type <- gsub(" ", "_", resource$resource_type)
X_resource <- dcast(resource, id ~ resource_type, length, value.var="id", fill=0)
X_resource$resource_count <- rowSums(X_resource[,-1,with=F])

X_train <- merge(X_train, X_resource, by="id")
X_test <- merge(X_test, X_resource, by="id")

# severity features
severity$severity_type <- gsub(" ", "_", severity$severity_type)
X_train <- merge(X_train, severity, by="id")
X_test <- merge(X_test, severity, by="id")

# location count
X_panel <- data.table(rbind.fill(X_train, X_test))

location_count <- X_panel[, .(location_count = .N), .(location)]
X_train <- merge(X_train, location_count, by="location")
X_test <- merge(X_test, location_count, by="location")

# cleaning data
target <- as.integer(as.factor(X_train$fault_severity)) - 1

train_ids <- X_train$id
test_ids <- X_test$id

X_train <- X_train[, ":="(location = as.integer(substr(location,10,nchar(as.character(location)))),
                          location_ratio = location_order / location_count,
                          max_prop = max_log / log_count,
                          f203_d_f82 = feature_203 / feature_82,
                          max_log = NULL,
                          first_fault = NULL,
                          id = NULL,
                          fault_severity = NULL)]

X_test <- X_test[, ":="(location = as.integer(substr(location,10,nchar(as.character(location)))),
                        location_ratio = location_order / location_count,
                        max_prop = max_log / log_count,
                        f203_d_f82 = feature_203 / feature_82,
                        max_log = NULL,
                        first_fault = NULL,
                        id = NULL,
                        fault_severity = NULL)]

# onehot encoding categories
source("./categorical.R")
X_data <- encode_categories(X_train, X_test, onehot="severity_type")

X_train <- X_data$train
X_test <- X_data$test

# missing values
X_train[is.na(X_train)] <- -1
X_test[is.na(X_test)] <- -1

# removing sparse columns
X_train <- subset(X_train, select=c(names(X_test)[which(colSums(X_test) != 0)]))
X_train <- subset(X_train, select=c(names(X_train)[which(colSums(X_train) != 0)]))

X_test <- X_test[, names(X_train), with=F]


## xgboost
seed <- 57

# cross-validation
# set.seed(seed)
# model_xgb_cv <- xgb.cv(data=as.matrix(X_train), label=as.matrix(target), num_class=3, nfold=10, objective="multi:softprob", nrounds=1600, eta=0.01, max_depth=8, subsample=0.9, colsample_bytree=0.5, eval_metric="mlogloss", prediction=T)
# CV: 0.40220

set.seed(seed)
model_xgb <- xgboost(as.matrix(X_train), as.matrix(target), num_class=3, objective="multi:softprob", nrounds=1600, eta=0.01, max_depth=8, subsample=0.9, colsample_bytree=0.5, eval_metric="mlogloss")


## predictions
pred <- predict(model_xgb, as.matrix(X_test))
test_xgb <- as.data.frame(matrix(pred, nrow(X_test), 3, byrow=T))

colnames(test_xgb) <- c("predict_0","predict_1","predict_2")
test_xgb$id <- test_ids

write.csv(test_xgb, "./submit.csv", row.names=F)



