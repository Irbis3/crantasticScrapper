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

dat.imp<-rfImpute(time ~ ., data=dat)
bestFit <- randomForest(time ~ ., data = dat.imp, ntree = 500)
rm(dat.imp)

bsFit <- sapply(1 : 1000,
    function(i){    
        dat.sample<-dat[sample(nrow(dat), replace=TRUE),]
	    dat.imp<-rfImpute(time ~ ., data=dat.sample)
        return(randomForest(time ~ ., data = dat.imp, ntree=500))        
    }
)
save(bestFit, file = "fittedModels.rda")

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

#testing something
                newdata <- data.frame(
                            race = as.factor(1),
                            gender = as.factor(input$gender),
                            GCS = input$GCS,
                            TFC = input$TFC,
                             codedinj = as.factor(input$codedinj),
                             dichsurgint = as.factor(input$dichsurgint),
                             dichskullfx = as.factor(input$dichskullfx),
                             dichopen = as.factor(input$dichopen),
                             trichsidelesion = as.factor(input$trichsidelesion),
                             codedlesloc = as.factor(input$codedlesloc),
                             simplifieddepth = as.factor(input$simplifieddepth),
                             dichhema = as.factor(input$dichhema),
                             dichpreinjurydx = as.factor(input$dichpreinjurydx),
                             age = input$age)



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
