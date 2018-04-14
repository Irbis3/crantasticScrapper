##########################################################################
## R Code for "Assessment of compound effects on the steroidogenic 
## pathway as an early detection of male reproductive toxicity" by 
## Goodwin et al (2013)
## 
## For the manuscript, these analyses were run using:
## 
##  > sessionInfo()
##  R version 2.15.2 (2012-10-26)
##  Platform: x86_64-apple-darwin9.8.0/x86_64 (64-bit)
##  
##  locale:
##    [1] C
##  
##  attached base packages:
##    [1] splines   stats     graphics  grDevices utils     datasets  methods  
##    [8] base     
##  
##  other attached packages:
##    [1] nnet_7.3-5         klaR_0.6-7         pamr_1.54          survival_2.36-14  
##    [5] kernlab_0.9-16     pROC_1.5.4         randomForest_4.6-7 RANN_2.2.1        
##    [9] e1071_1.6-1        class_7.3-5        MASS_7.3-22        doMC_1.2.5        
##   [13] multicore_0.1-7    iterators_1.0.6    caret_5.15-87      reshape2_1.2.1    
##   [17] plyr_1.7.1         lattice_0.20-10    foreach_1.4.0      cluster_1.14.3    
##  
##  loaded via a namespace (and not attached):
##    [1] codetools_0.2-8 compiler_2.15.2 grid_2.15.2     stringr_0.6.1  
##    [5] tools_2.15.2   
## 

library(caret)
# library(doMC)
# registerDoMC(14)
library(latticeExtra)

##########################################################################

raw <- read.delim("data.txt", stringsAsFactors = FALSE)
raw$Class <- factor(raw$Class, levels = c("toxic", "nontoxic"))
rownames(raw) <- raw$Compound
raw$Compound <- NULL

save(raw, file = "data.RData")

##########################################################################
## Transform the predictors to be more symmetric

pp <- preProcess(raw[,-1],c("BoxCox", "knnImpute"))
imputed <- predict(pp, raw[,-1])
imputed$Class <- raw$Class

##########################################################################
## Areas under the ROC curve

rocValues <- filterVarImp(imputed[, -ncol(imputed)], imputed$Class)
rocValues <- rocValues[order(rocValues$toxic, decreasing = TRUE),]

paste(toupper(rownames(rocValues)), " (", round(rocValues$toxic, 2), ")", 
      sep = "", collapse = ", ")

##########################################################################
## Create an object for resampling the area under the ROC curve

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

##########################################################################
## There objects will be used for recursive feature selection

rfFuncs2 <- rfFuncs
rfFuncs2$summary <- twoClassSummary
nbFuncs2 <-nbFuncs
nbFuncs2$summary <- twoClassSummary
caretFuncs2 <- caretFuncs
caretFuncs2$summary <- twoClassSummary

ctrl2 <- rfeControl(functions = rfFuncs2, 
                    method = "repeatedcv", 
                    saveDetails = TRUE,
                    repeats = 5,
                    allowParallel = FALSE)

##########################################################################
## Random Forests

set.seed(823)
rfFit <- train(Class ~ ., data = imputed,
               method = "rf",
               metric = "ROC",
               importance = TRUE,
               ntree = 1000,
               tuneLength = 10,
               trControl = ctrl)

set.seed(823)
rfRFE <- rfe(Class ~ ., data = imputed, rfeControl = ctrl2, 
             metric = "ROC",             
             sizes = 1:(ncol(imputed)-1))

rfImp <- varImp(rfFit, scale = FALSE)$importance
rfImp <- rfImp[order(rfImp$toxic, decreasing = TRUE),]
paste(toupper(rownames(rocValues)), collapse = ", ")

rfHoldouts <- merge(rfFit$pred, rfFit$bestTune)
histogram(~toxic|obs, data = rfHoldouts, layout = c(1, 2))

rfSubset <- subset(rfHoldouts, toxic > .6 | toxic < .4)


##########################################################################
## Support vector machines

set.seed(823)
svmRFit <- train(Class ~ ., data = imputed,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 tuneLength = 12,
                 trControl = ctrl)

ctrl2$functions <- caretFuncs2
set.seed(823)
svmRFE <- rfe(Class ~ ., data = imputed, rfeControl = ctrl2, 
              method = "svmRadial",
              tuneLength = 12,
              metric = "ROC",
              prob.model = TRUE,
              sizes = 1:(ncol(imputed)-1))

##########################################################################
## Nearest Shrunken Centroids

set.seed(823)
nscFit <- train(Class ~ ., data = imputed,
                method = "pam",
                metric = "ROC",
                tuneLength = 30,
                trControl = ctrl)

##########################################################################
## Naive Bayes

set.seed(823)
nbFit <- train(Class ~ ., data = imputed,
                method = "nb",
                metric = "ROC",
                trControl = ctrl)

ctrl2$functions <- nbFuncs2
set.seed(823)
nbRFE <- rfe(Class ~ ., data = imputed, rfeControl = ctrl2, 
             metric = "ROC",
             sizes = 1:(ncol(imputed)-1))

nbHoldouts <- subset(nbRFE$pred, Variables == nbRFE$optsize)


##########################################################################
## Regularized logistic regression

set.seed(823)
lrFit <- train(Class ~ ., data = imputed,
                method = "multinom",
                preProc = c("center", "scale"),
                metric = "ROC",
                tuneGrid = data.frame(.decay = c(0, .001, .01, .1)),
               trace = FALSE,
                trControl = ctrl)

ctrl2$functions <- caretFuncs2
set.seed(823)
lrRFE <- rfe(Class ~ ., data = imputed, rfeControl = ctrl2, 
             method = "multinom",
             tuneGrid = data.frame(.decay = .01),
             trace = FALSE,
             metric = "ROC",
             sizes = 1:(ncol(imputed)-1))

##########################################################################
## Collect the resampling results

rs <- resamples(list(NaiveBayes = nbFit,
                     NaiveBayes.RFE = nbRFE,
                     RandomForest = rfFit,
                     RandomForest.RFE = rfRFE,
                     SVM = svmRFit,
                     SVM.RFE = svmRFE,
                     LogisticReg = lrFit,
                     LogisticReg.RFE = lrRFE,
                     NSC = nscFit))
##########################################################################
## RFE plot

nbRFE$results$Model <- "Naive Bayes"
rfRFE$results$Model <- "Random Forest"
svmRFE$results$Model <- "Support Vector Machine"
lrRFE$results$Model <- "Logistic Regression"

rfeProfiles <- rbind( nbRFE$results[, c("Variables", "ROC", "ROCSD", "Model")],
                      rfRFE$results[, c("Variables", "ROC", "ROCSD", "Model")],
                      lrRFE$results[, c("Variables", "ROC", "ROCSD", "Model")],
                     svmRFE$results[, c("Variables", "ROC", "ROCSD", "Model")])
png("rfe.png", width = 7*96, height = 5*96)
trellis.par.set(caretTheme())
print(
xyplot(ROC ~ Variables, data = rfeProfiles, groups = Model,
       type = "o", auto.key = list(columns = 2))
)
dev.off()

library(ggplot2)

p <- ggplot(rfeProfiles, aes(y=ROC, x=Variables))

limits <- aes(ymax = ROC + ROCSD/sqrt(length(rfFit$control$index)), 
              ymin = ROC - ROCSD/sqrt(length(rfFit$control$index)))

p + geom_point() + 
  geom_line() + 
  geom_errorbar(limits, width = .5) + 
  facet_grid( ~ Model) + 
  ylim(c(.5, 1)) + 
  ylab("Area Under the ROC Curve") +
  theme_bw()


##########################################################################
## Results table

meanValues <- colMeans(rs$values[,-1])
meanDF <- data.frame(Mean = meanValues,
                     Model = unlist(lapply(strsplit(names(meanValues), "~"), function(x) x[1])),
                     Metric = unlist(lapply(strsplit(names(meanValues), "~"), function(x) x[2])))
meanDF <-reshape(meanDF, idvar = "Model", direction = "wide", timevar = "Metric")


semValues <- apply(rs$values[,-1], 2, function(x) sd(x)/sqrt(length(x)))
semDF <- data.frame(SEM = semValues,
                    Model = unlist(lapply(strsplit(names(semValues), "~"), function(x) x[1])),
                    Metric = unlist(lapply(strsplit(names(semValues), "~"), function(x) x[2])))
semDF <-reshape(semDF, idvar = "Model", direction = "wide", timevar = "Metric")

results <- merge(meanDF, semDF)
results$Model <- gsub(".RFE", " (RFE)", as.character(results$Model), fixed = TRUE)
results$preds <- ncol(imputed) - 1
results$preds[results$Model == "LogisticReg (RFE)"] <- lrRFE$bestSubset
results$preds[results$Model == "NaiveBayes (RFE)"] <- nbRFE$bestSubset
results$preds[results$Model == "RandomForest (RFE)"] <- rfRFE$bestSubset
results$preds[results$Model == "SVM (RFE)"] <- svmRFE$bestSubset
results$preds[results$Model == "NSC"] <- length(predictors(nscFit))

results <- results[, c(1, 8, 2, 5, 3, 6, 4, 7)]
results <- results[order(results$Model),]

write.csv(results, file = "table.csv")

##########################################################################
## post-hoc ROC analysis

rf0 <- rfHoldouts[, c("obs", "toxic")]
rf0$Model <- "Random Forest"
nb0 <- nbHoldouts[, c("obs", "toxic")]
nb0$Model <- "Naive Bayes (RFE)"

holdouts <- rbind(rf0, nb0)
holdouts$Class <- paste("Truth:", as.character(holdouts$obs))

png("hist.png", width = 7*96, height = 5*96)
trellis.par.set(caretTheme())
print(
useOuterStrips(
  histogram(~toxic|Model*Class, data = holdouts,
          panel = function(...)
            {
            panel.abline(v = c(.4, .6), col = "grey", lty = 2)
            panel.histogram(...)
          },
            xlab = "Probability of Toxicity"))
)
dev.off()

rfSubset <- subset(rfHoldouts, toxic > .6 | toxic < .4)
rfSubsetROC <- roc(rfSubset$obs, rfSubset$toxic, levels = rev(levels(rfSubset$obs)))
ci.auc(rfSubsetROC)
1-(nrow(rfSubset)/nrow(rfHoldouts))
