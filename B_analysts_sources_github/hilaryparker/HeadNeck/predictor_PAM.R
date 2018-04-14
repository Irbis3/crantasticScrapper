predictor_PAM<-function(train.dat, train.grp, test.dat, test.grp){

	fit <- pamr.train(list(x=train.dat,y=train.grp))
	pred <- pamr.predict(fit,test.dat,threshold=2)
	
	return(mean(pred == test.grp))

}