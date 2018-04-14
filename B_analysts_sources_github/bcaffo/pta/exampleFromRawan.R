##load data, exclude vars not used, and define factor vars
dat<-read.csv("PTAshort_04.03.2012_TIME_ALL_PTA.csv", header=T)
dat$patient<-NULL
dat$ind_30days<-NULL
dat$combo<-NULL
dat$time_weeks<-NULL
dat$status<-NULL
dat$TTA<-NULL
dat$race<-as.factor(dat$race)
dat$gender<-as.factor(dat$gender)
dat$codedinj<-as.factor(dat$codedinj)
dat$dichsurgint<-as.factor(dat$dichsurgint)
dat$dichskullfx<-as.factor(dat$dichskullfx)
dat$dichopen<-as.factor(dat$dichopen)
dat$trichsidelesion<-as.factor(dat$trichsidelesion)
dat$codedlesloc<-as.factor(dat$codedlesloc)
dat$simplifieddepth<-as.factor(dat$simplifieddepth)
dat$dichhema<-as.factor(dat$dichhema)
dat$dichpreinjurydx<-as.factor(dat$dichpreinjurydx)

library(randomForest)

#impute data, build forest, make prediction
#dataset used for prediction labeled preddat in the following function
#all vars in dataset are used for prediction - change if necessary
predictions <- vector("list", 1000)
for(i in 1:1000) {
	dat.sample<-dat[sample(nrow(dat), replace=TRUE),]
	dat.imp<-rfImpute(time ~ ., data=dat.sample)
	fit<-randomForest(time ~ ., data = dat.imp, ntree=500)
	predictions[[i]] <- predict(fit, preddat[1,])
	}
avg.pred<-(sum(unlist(predictions)))/(1000)
se.pred<-sd(unlist(predictions))
quantile(unlist(predictions), c(0.025, 0.975))


#general use code as a function
dat<-
ndat<-
num.bootsamples<-
bootstrap.predci<-function(data=dat, newdata=ndat, num.bootsamples) {
	predictions <- rep(NA, num.bootsamples)
	for(i in 1:num.bootsamples) {
		dat.sample<-dat[sample(nrow(dat), replace=TRUE),]
		dat.imp<-rfImpute(time ~ ., data=dat.sample)
		fit<-randomForest(time ~ ., data = dat.imp, ntree=500)
		predictions[i] <- predict(fit, ndat[1,])
		}
	avg.pred<-mean(predictions)
	se.pred<-sd(predictions)
	quantile(predictions, c(0.025, 0.975))
}
bootstrap.predci(dat, ndat, num.bootsamples)
