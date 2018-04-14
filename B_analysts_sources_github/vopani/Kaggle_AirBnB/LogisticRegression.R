# function for logistic regression
LogisticRegression <- function(X_train,y,X_test=data.frame(),cv=5,seed=123,metric="auc",importance=0)
{
  # defining evaluation metric
  score <- function(a,b,metric)
  {
    switch(metric,
           accuracy = sum(abs(a-b)<=0.5)/length(a),
           auc = auc(a,b),
           logloss = -(sum(log(1-b[a==0])) + sum(log(b[a==1])))/length(a),
           mae = sum(abs(a-b))/length(a),
           precision = length(a[a==b])/length(a),
           rmse = sqrt(sum((a-b)^2)/length(a)),
           rmspe = sqrt(sum(((a-b)/a)^2)/length(a)))           
  }
  
  if (metric == "auc")
  {
    library(pROC)
  }
  
  cat("Preparing Data\n")
  X_train$order <- seq(1, nrow(X_train))
  X_train$result <- as.numeric(y)
  
  set.seed(seed)
  X_train$randomCV <- floor(runif(nrow(X_train), 1, (cv+1)))
  
  # cross validation
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(X_train, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(X_train, randomCV == i) 
    
    # building model
    model_lr <- glm(result ~., data=X_build, family=binomial())
    
    # predicting on validation data
    pred_lr <- predict(model_lr, X_val, type="response")
    X_val <- cbind(X_val, pred_lr)
    
    # predicting on test data
    if (nrow(X_test) > 0)
    {
      pred_lr <- predict(model_lr, X_test, type="response")
    }
    
    cat("CV Fold-", i, " ", metric, ": ", score(X_val$result, X_val$pred_lr, metric), "\n", sep = "")
    
    # initializing outputs
    if (i == 1)
    {
      output <- X_val
      if (nrow(X_test) > 0)
      {
        X_test <- cbind(X_test, pred_lr)
      }      
    }
    
    # appending to outputs
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(X_test) > 0)
      {
        X_test$pred_lr <- (X_test$pred_lr * (i-1) + pred_lr)/i
      }            
    }
    
    gc()
  } 
  
  # final evaluation score
  output <- output[order(output$order),]
  cat("\nLogisticRegression ", cv, "-Fold CV ", metric, ": ", score(output$result, output$pred_lr, metric), "\n", sep = "")
  
  # returning CV predictions and test data with predictions
  return(list("train"=output, "test"=X_test))  
}
