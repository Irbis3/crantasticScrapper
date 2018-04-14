# Dean Attali
# Mar 25, 2014
# UBC STAT540

# lab 10 - Supervised learning, classification, cross validation, variable selection

library(MASS)
library(reshape)
library(car)
library(limma)
library(e1071)
library(glmnet)
library(ROCR)
library(CMA)
library(GEOquery)
library(lattice)

##### data preparation

# download datasets from GEO
if (file.exists("class_LNstatus.Rdata")) {
  # if previously downloaded
  load("class_LNstatus.Rdata")
} else {
  # if downloading for the first time takes a several mins!; returns a list
  datgeo <- getGEO("GSE23177", GSEMatrix = TRUE)
  dat <- datgeo[[1]]  #Note that dat is an ExpressionSets
  
  str(pData(dat), max.level = 0)
  
  # extract only those variables of interest
  pData(dat) <- subset(pData(dat), select = c("characteristics_ch1.2", "characteristics_ch1.3", 
                                              "characteristics_ch1"))
  names(pData(dat)) <- c("LnStatus", "LnRatio", "Set")
  
  # Note: LNRatio will not be used in this Seminar. However, you can use it to
  # try some of the regularization techniques learned in class
  
  # split the ExpressionSet into training and test sets.
  train.es <- dat[, dat$Set == "patient type: training set"]
  test.es <- dat[, dat$Set != "patient type: training set"]
  
  # Re-label factor
  pData(train.es)$LnStatus <- recode(pData(train.es)$LnStatus, "levels(pData(train.es)$LnStatus)[1]='neg'; else='pos'", 
                                     levels = c("neg", "pos"))
  
  pData(test.es)$LnStatus <- recode(pData(test.es)$LnStatus, "levels(pData(test.es)$LnStatus)[1]='neg'; else='pos'", 
                                    levels = c("neg", "pos"))
  
  # create data matrices with expression values (probesets in rows). Some of
  # the functions we will use do not take ExpressionSets as objects
  trainDat <- exprs(train.es)
  testDat <- exprs(test.es)
  
  # Redefine the quantitative variable LnRatio to make it a numeric variable.
  ntrain <- dim(pData(train.es))[1]
  ntest <- dim(pData(test.es))[1]
  
  pData(train.es)$LnRatio <- as.numeric(unlist(strsplit(as.vector(unlist(pData(train.es)$LnRatio)), 
                                                        ":", fixed = TRUE))[(1:ntrain) * 2])
  pData(test.es)$LnRatio <- as.numeric(unlist(strsplit(as.vector(unlist(pData(test.es)$LnRatio)), 
                                                       ":", fixed = TRUE))[(1:ntest) * 2])
  
  # save the data to avoid future re-downloading
  save(dat, trainDat, testDat, train.es, test.es, file = "class_LNstatus.Rdata")
}

# undestand your data for classification
table(pData(train.es)$LnStatus)
table(pData(test.es)$LnStatus)

# understand the continuous response
tapply(pData(train.es)$LnRatio, pData(train.es)$LnStatus, summary)
tapply(pData(test.es)$LnRatio, pData(test.es)$LnStatus, summary)

# look at the expression of 3 randomly picked genes in both training and test sets
set.seed(1234)
(getMe <- sample(1:nrow(train.es), size = 3))  ## [1]   2756 15082 14766

# training data
trDat <- trainDat[getMe, ]
str(trDat)

trDat <- data.frame(LnStatus = pData(train.es)$LnStatus, Set = rep("train", 
                                                                   nrow(pData(train.es))), t(trDat))
str(trDat)

plotDat.train <- melt(trDat, id = c("LnStatus", "Set"), variable_name = "gene")
colnames(plotDat.train)[colnames(plotDat.train) == "value"] = "gExp"

# test data
tDat <- testDat[getMe, ]
str(tDat)

tDat <- data.frame(LnStatus = pData(test.es)$LnStatus, Set = rep("test", nrow(pData(test.es))), 
                   t(tDat))
str(tDat)

plotDat.test <- melt(tDat, id = c("LnStatus", "Set"), variable_name = "gene")
colnames(plotDat.test)[colnames(plotDat.test) == "value"] = "gExp"

plotDat <- rbind(plotDat.train, plotDat.test)

# plot 3 randomly picked genes in both training and test sets
stripplot(gExp ~ LnStatus | gene + Set, plotDat, grid = TRUE, group = LnStatus, 
          auto.key = TRUE, jitter.data = TRUE)


##### classification

## cross validation splits
nfold <- 6
tabTrain <- table(train.es$LnStatus)
indlist <- sapply(names(tabTrain), function(z) which(train.es$LnStatus == z), 
                  simplify = FALSE)

set.seed(1234)

# Each row contains 8 pos and 8 negative samples.
fold.pos <- matrix(sample(indlist[["pos"]]), nrow = nfold)
fold.neg <- matrix(sample(indlist[["neg"]]), nrow = nfold)

splits <- GenerateLearningsets(y = train.es$LnStatus, method = "CV", fold = 6, 
                               strat = TRUE)

## feature selection

# Define here the constants that you will not evaluate. For example, I will
# use the top-50 limma genes

ngenes <- 50
nmethod <- 7  #number of methods you plan to compare. 

# Define here an output objects to store results
pr.err <- matrix(-1, nfold, nmethod, dimnames = list(paste0("Fold", 1:nfold), 
                                                     c("1NN", "5NN", "10NN", "15NN", "LDA", "Logit", "SVM")))

for (i in 1:nfold) {
  
  # Test Fold for the i-th step
  testdat.fold <- trainDat[, c(fold.pos[i, ], fold.neg[i, ])]
  # I will create a factor of classes for the test set of the i_th fold
  testclass.fold <- train.es$LnStatus[c(fold.pos[i, ], fold.neg[i, ])]
  
  
  # The rest of the samples are the training set for the i-th step
  traindat.fold <- trainDat[, -c(fold.pos[i, ], fold.neg[i, ])]
  trainclass.fold <- train.es$LnStatus[-c(fold.pos[i, ], fold.neg[i, ])]
  
  # Step 1: feature selection (do you remember limma?).
  
  # Note that a different set of genes will be selected for each fold! you can
  # then compare how consistent these sets were.
  
  limma.dat <- as.data.frame(traindat.fold)
  desMat <- model.matrix(~trainclass.fold, limma.dat)  #design matrix
  trainFit <- lmFit(limma.dat, desMat)
  eBtrainFit <- eBayes(trainFit)
  
  # top-50 limma genes
  top.fold <- topTable(eBtrainFit, coef = which(colnames(coef(trainFit)) != 
                                                  "(Intercept)"), n = ngenes, sort.by = "P")
  
  # Retain the top-50 limma genes from the train and test sets
  traindat.fold <- traindat.fold[rownames(top.fold), ]
  testdat.fold <- testdat.fold[rownames(top.fold), ]
  
  
  # STEP 2: select a classifier Set a counter for the method tested
  l <- 0
  
  # kNN classifiers
  for (kk in c(1, 5, 10, 15)) {
    # every time you get inside this loop, the l counter gets redefined (i.e.,
    # 1, 2, etc for method 1, method 2, etc)
    l <- l + 1
    
    # knn needs samples in rows
    yhat.knn <- knn3(train = t(traindat.fold), test = t(testdat.fold), cl = trainclass.fold, 
                    k = kk)
    # Store the prediction error for each kk within this fold
    pr.err[i, l] <- mean(testclass.fold != yhat.knn)
  }  #end of kNN loop
  
  # LDA method. Note that you can change the prior parameter to reflect a
  # different proportion of case and control samples. The default is to use
  # the class proportions from the training set.
  
  m.lda <- lda(x = t(traindat.fold), group = trainclass.fold, prior = c(0.5, 
                                                                        0.5))
  yhat.lda <- predict(m.lda, newdata = t(testdat.fold))$class
  pr.err[i, "LDA"] <- mean(testclass.fold != yhat.lda)
  
  # Logit
  glm.dat <- data.frame(t(traindat.fold), group = trainclass.fold)
  m.log <- glm(group ~ ., data = glm.dat, family = binomial)
  
  pr.log <- predict(m.log, newdata = data.frame(t(testdat.fold)), type = "response")
  pr.cl <- rep(0, length(testclass.fold))
  pr.cl[pr.log > 1/2] <- "pos"
  pr.cl[pr.log <= 1/2] <- "neg"
  
  pr.cl <- factor(pr.cl)
  pr.err[i, "Logit"] <- mean(pr.cl != testclass.fold)
  
  # SVM
  m.svm <- svm(x = t(traindat.fold), y = trainclass.fold, cost = 1, type = "C-classification", 
               kernel = "linear")
  pr.svm <- predict(m.svm, newdata = t(testdat.fold))
  
  pr.err[i, "SVM"] <- mean(pr.svm != testclass.fold)
}  #end of CV loop


## look at error rate for each model
cv.err <- colMeans(pr.err)

# mean - 1 sd (sd of the 6 error rates)
ls <- cv.err - apply(pr.err, 2, sd)

# mean + 1 sd (sd of the 6 error rates)
us <- cv.err + apply(pr.err, 2, sd)

# plot the results
plot(1:nmethod, cv.err, ylim = c(0, 1), xlim = c(1, (nmethod + 0.5)), type = "n", 
     axes = FALSE, xlab = "Classifier", ylab = "Error rate", main = "6-fold CV Error")

for (j in 1:ncol(pr.err)) points(jitter(rep(j, 6), factor = 2), jitter(pr.err[, 
                                                                              j]), cex = 0.8, pch = "X", col = "gray")

for (i in 1:nmethod) lines(c(i, i), c(ls[i], us[i]), lwd = 2, col = "gray")
points(1:nmethod, ls, pch = 19, col = "red")
points(1:nmethod, us, pch = 19, col = "green")
points(1:nmethod, cv.err, pch = 19, cex = 1.5, col = "black")
axis(2, ylab = "Error rate")
axis(1, 1:nmethod, colnames(pr.err))

box()


### test the selected model (10NN appears to have least error)
yhat.knn <- knn(train = t(trainDat), test = t(testDat), cl = train.es$LnStatus, 
                k = 10)
# Store the prediction error for each kk within this fold
pr.errTest <- mean(test.es$LnStatus != yhat.knn)
pr.errTest



### Instead of all the CV above: CMA to reduce the number of steps
featureScores <- GeneSelection(X = t(trainDat), y = train.es$LnStatus, learningsets = splits, 
                               method = "limma")

# Compare list of selected genes using:
toplist(featureScores)

# We can aggregate the results across the 6 splits.
seliter <- numeric()
for (i in 1:nfold) seliter <- c(seliter, toplist(featureScores, iter = i, top = 10, 
                                                 show = FALSE)$index)
sort(table(seliter), dec = T)  # summarize

# Choose the 20 probes which are chosen most commonly in the 6 splits
bestprobes <- as.numeric(names(sort(table(seliter), dec = T)))[1:20]

# examine the annotations. I just selected a few columns from the fData of
# the eSet.
fData(dat)[bestprobes, c("Gene Symbol", "Gene Title", "ENTREZ_GENE_ID", "Representative Public ID")]