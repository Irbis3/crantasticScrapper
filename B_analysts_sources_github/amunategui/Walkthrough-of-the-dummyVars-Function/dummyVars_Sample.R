survey <- data.frame(service=c('very unhappy','unhappy','neutral','happy','very happy'))
print(survey)

survey <- data.frame(service=c('very unhappy','unhappy','neutral','happy','very happy'), rank=c(1,2,3,4,5))
print(survey)

library(caret) 

?dummyVars # many options

customers <- data.frame(id=c(10,20,30,40,50), gender=c('male','female','female','male','female'), mood=c('happy','sad','happy','sad','happy'), outcome=c(1,1,0,0,0))

dmy <- dummyVars(" ~ .", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)
print(str(trsf))

# works only on factors
customers$outcome <- as.factor(customers$outcome)

# tranform just gender
dmy <- dummyVars(" ~ gender", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)

# use fullRank to avoid the 'dummy trap'
dmy <- dummyVars(" ~ .", data = customers, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)
