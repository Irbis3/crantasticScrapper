


##Only use Age, Cause of Injury, Sex, place where
##the injury took place and Year of death
##to classify the presumed Accidents, Homicides, etc
y <- c("PRESUNTOtxt")
x <- c("EDADVALOR","CAUSE", "SEXOtxt", "LUGLEStxt", "ANIODEF")

#subset all the deaths that are of unknown injury intent
hom.train <- hom.juarez[!is.na(hom.juarez$PRESUNTOtxt),]
#Get an idea of how many accidents by transportation there are
ddply(subset(hom.train, PRESUNTOtxt == "Accident"), .(CAUSE), nrow)

#Use kNN to impute missing data
hom.train <- cbind(kNN(hom.train[,c(x)])[1:length(x)], PRESUNTOtxt = hom.train$PRESUNTOtxt)


#test that kNN didn't do anything stupid by checking that
#most accidents of unspecified injury mechanism are
#categorized as transportation 
ddply(subset(hom.train, PRESUNTOtxt == "Accident"), .(CAUSE), nrow)


#divide the data set into training and test
inTrain <- createDataPartition(1:nrow(hom.train), p = 3/4, list = FALSE)

train <- hom.train[inTrain,c(x,y)]
test <- hom.train[-inTrain,c(x,y)]



#percentage of deaths by injury intent (accident, etc)
prop.table(table(hom.train$PRESUNTOtxt))


#Base the prediction on injury mechanism(cause), age, sex and place where the body
#was fount
formula <-  PRESUNTOtxt ~ CAUSE + EDADVALOR + SEXOtxt + LUGLEStxt + ANIODEF
bootControl <- trainControl(number = 25,
                            returnData = FALSE)
if ( require("multicore", quietly = TRUE, warn.conflicts = FALSE) ) {
    bootControl$workers <- multicore:::detectCores()
    bootControl$computeFunction <- mclapply
    bootControl$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)
}

#Random Forest
if(file.exists("cache/rf.RData")) {
  message("#############################")
  message("Loading cached data for the random forest classifier")
  message("delete cache directory if you don't want this to happen")
  message("#############################")
  load(file = "cache/rf.RData")
} else {
  message("#############################")
  message("#############################")
  message("Starting Random Forest Classifier:")
  message("#############################")
  message("#############################")
  message("This will take some time")
  message("#############################")
  message("#############################")
  rfFit <- train(formula,
                 data = train,
                 method = "rf",
                 trControl = bootControl,
                 importance = TRUE)
  save(rfFit, file = "cache/rf.RData")
}
fit.pred.rf <- predict(rfFit, test)
print(confusionMatrix(fit.pred.rf, test$PRESUNTOtxt))

#Elastic Net
if(file.exists("cache/glmnet.RData")) {
  message("#############################")
  message("Loading cached data for the penalized regression classifier")
  message("delete cache directory if you don't want this to happen")
  message("#############################")
  load(file = "cache/glmnet.RData")
} else {
  message("#############################")
  message("#############################")
  message("Starting glmnet classifier:")
  message("#############################")
  message("#############################")
  message("This will take some time")
  message("#############################")
  message("#############################")
  glmnetFit <- train(formula,
                     data = train,
                     method = "glmnet",
                     trControl = bootControl)
  save(glmnetFit, file = "cache/glmnet.RData")
}
fit.pred.glmnet <- predict(glmnetFit, test)
print(confusionMatrix(fit.pred.glmnet, test$PRESUNTOtxt))

#Support Vector Machine
if(file.exists("cache/svm.RData")) {
  message("#############################")
  message("Loading cached data for the svm classifier")
  message("delete cache directory if you don't want this to happen")
  message("#############################")
  load(file = "cache/svm.RData")
} else {
  message("#############################")
  message("#############################")
  message("Starting svm classifier:")
  message("#############################")
  message("#############################")
  message("This will take some time")
  message("#############################")
  message("#############################")
  svmFit <- train(formula,
                  data = train,
                  method = "svmRadial",
                  trControl = bootControl)
  save(svmFit, file = "cache/svm.RData")
}
fit.pred.svm <- predict(svmFit, test)
print(confusionMatrix(fit.pred.svm, na.omit(test)$PRESUNTOtxt))

#Determine which was the best model
resamps <- resamples(list('Support Vector Machine' = svmFit,
                          'Random Forest' = rfFit,
                          'Penalized Regression' = glmnetFit))
summary(resamps)
png("graphs/accuracy.png", width = 600, height = 540)
print(bwplot(resamps, metric = "Accuracy"))
dev.off()
dotplot(resamps, metric = "Accuracy")
densityplot(resamps, metric = "Accuracy")


#knn impute the missing data from the unknown intent deaths
hom.unknown <- hom.juarez[is.na(hom.juarez$PRESUNTOtxt),]
hom.unknown[,c(x)] <- kNN(hom.unknown[,c(x)])[1:length(x)]





