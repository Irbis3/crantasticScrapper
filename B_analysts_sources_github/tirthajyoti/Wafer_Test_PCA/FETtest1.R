library(caret)
ndie <- 500
accuracy.ratio <- c()
Coupling.factor <- c()
a1<- c();a2<- c() 
yield <-c(); IGS.fail<-c();IDS.fail<-c();UIL.fail <- c();Rds.fail<-c()

# Play with range of i and coupling factor multiplier
for (i in 1:150){
Coupling.factor[i] <-(i-1)*0.07
# Influence of coupling strength limited up to a certain point to limit yield loss
if (i<=100){
    UIL.IDS.Coupling <- 1e6*Coupling.factor[i]
    Rds.IGS.Coupling <- 1.2e6*Coupling.factor[i]
}
# Enumerating various yield loss
x1<- rnorm(ndie,75e-12,17e-12)
IGS.fail[i]<- sum(x1>1e-10)/ndie
x2<- rnorm(ndie,0.75e-6,0.17e-6)
IDS.fail[i]<- sum(x2>1e-6)/ndie
x3<- rnorm(ndie,mean=20,sd=1)-UIL.IDS.Coupling*x2
UIL.fail[i]<- sum(x3<15)/ndie
x4<- rnorm(ndie,mean=0.8e-3,sd=0.1e-3)+Rds.IGS.Coupling*x1
Rds.fail[i]<- sum(x4>1.5e-3)/ndie

wfr.sort<- data.frame(IGS = x1, IDS = x2, 
                      UIL.coupled = x3, Rds.coupled = x4)

wfr.sort$result.coupled <-0
wfr.sort$result.coupled <- (wfr.sort$IGS>1e-10)|(wfr.sort$IDS>1e-6)|
    (wfr.sort$UIL.coupled<15)|(wfr.sort$Rds.coupled>1.5e-3)
wfr.sort$result.coupled <- !wfr.sort$result.coupled
yield[i] <- 100*sum(as.numeric(wfr.sort$result.coupled))/ndie
wfr.sort$result.coupled<- as.factor(wfr.sort$result.coupled)

# Test/Train split
p<- 0.6; # Split-ratio
inTrain<- createDataPartition(y=wfr.sort$result.coupled,p=p,list=F)
train <- wfr.sort[inTrain,]; test<- wfr.sort[-inTrain,]

# Centering and scaling pre-process
L.df <- length(wfr.sort)
prepro <- preProcess(train[,-c(L.df)],method = c("center", "scale"))
trainScale<- predict(prepro, train[,-c(L.df)])
#Cbind to include 'type' data to trainScale
trainScale<- cbind(trainScale, result.coupled=train$result.coupled) 
testScale<- predict(prepro,test[,-c(L.df)])
testScale<- cbind(testScale, result.coupled=test$result.coupled)

# PCA (with all 4 test data)
prC.coupled<- preProcess((trainScale[c('IGS','IDS','UIL.coupled','Rds.coupled')]), 
                         method = 'pca', pcaComp = 2)
trPC.coupled <- predict(prC.coupled,
                        train[c('IGS','IDS','UIL.coupled','Rds.coupled')])
trPC.coupled<- cbind(trPC.coupled,result.coupled=train$result.coupled)
testPC.coupled <- predict(prC.coupled, 
                          test[c('IGS','IDS','UIL.coupled','Rds.coupled')])
testPC.coupled<- cbind(testPC.coupled,result.coupled=test$result.coupled)

# PCA-2 (with only leakage test data)
prC.coupled2<- preProcess((trainScale[c('IGS','IDS')]), 
                         method = 'pca', pcaComp = 2)
trPC.coupled2 <- predict(prC.coupled2,
                        train[c('IGS','IDS')])
trPC.coupled2<- cbind(trPC.coupled2,result.coupled=train$result.coupled)
testPC.coupled2 <- predict(prC.coupled2, 
                          test[c('IGS','IDS')])
testPC.coupled2<- cbind(testPC.coupled2,result.coupled=test$result.coupled)

# Model fit and prediction
fit.coupled <- train(result.coupled~., data=trPC.coupled, method = 'glm')
Pred.coupled<- data.frame(Prediction=predict(fit.coupled,testPC.coupled))
m1<- confusionMatrix(test$result.coupled,Pred.coupled$Prediction)

fit.coupled2 <- train(result.coupled~., data=trPC.coupled2, method = 'glm')
Pred.coupled2<- data.frame(Prediction=predict(fit.coupled2,testPC.coupled2))
m2<- confusionMatrix(test$result.coupled,Pred.coupled2$Prediction)

# Accuracy calculation
a1[i]<-m1$overall[1]
a2[i]<-m2$overall[1]
accuracy.ratio[i] <- 100*m2$overall[1]/m1$overall[1]
}

# Plotting
par(mfrow=c(2,1))
plot(x=Coupling.factor,y=a1,pch=19, cex=1.2,
     xlab="Coupling factor", 
     ylab="Prediction accuracy (%) of using PCA of all 4 test results")
plot(x=Coupling.factor,y=a2,pch=19, cex=1.2,
     xlab="Coupling factor", 
     ylab="Prediction accuracy (%) of using PCA of only leakage results")
# plot(Coupling.factor,accuracy.ratio, type='l',lwd=3,
#      ylab="Accuracy ratio of limited PCA and full PCA methods", 
#      xlab="Coupling.Factor")
# plot(x=Coupling.factor, y=yield, pch=19, 
#      ylab="Wafer yield (%)", xlab="Coupling.factor")
par(mfrow=c(2,2))
hist(wfr.sort$IGS,breaks=40, col='green')
hist(wfr.sort$IDS,breaks=40, col='green')
hist(wfr.sort$UIL.coupled,breaks=40, col='green')
hist(wfr.sort$Rds.coupled,breaks=40, col='green')

par(mfrow=c(2,2),pch=19,cex=1.2)
plot(x=Coupling.factor, y=100*IGS.fail,
     ylab="IGS leakage fail (%)", xlab="Coupling.factor")
plot(x=Coupling.factor, y=100*IDS.fail,
     ylab="IDS leakage fail (%)", xlab="Coupling.factor")
plot(x=Coupling.factor, y=100*UIL.fail,
     ylab="Low UIL fail (%)", xlab="Coupling.factor")
plot(x=Coupling.factor, y=100*Rds.fail,
     ylab="High Rds fail (%)", xlab="Coupling.factor")
