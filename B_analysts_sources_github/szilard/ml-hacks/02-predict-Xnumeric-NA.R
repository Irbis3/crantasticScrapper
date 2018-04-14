
set.seed(123)

N <- 10000

x1 <- runif(N, min = -1, max = 1)
x2 <- runif(N, min = -1, max = 1)

z <- 1 * x1 + 10 * x2
p <- 1/(1+exp(-z))
y <- rbinom(N,1,p)

d <- data.frame(x1=x1, x2=x2, y=y)

md <- glm(y ~ ., data = d, family = binomial())
md

#Coefficients:
#(Intercept)           x1           x2  
#   -0.01689      1.19116     10.07862  


predict(md, newdata = data.frame(x1=0.2, x2=0.3), type = "response")
predict(md, newdata = data.frame(x1=NA_real_, x2=0.3), type = "response")

#> predict(md, newdata = data.frame(x1=0.2, x2=0.3), type = "response")
#        1 
#0.9624904 
#> predict(md, newdata = data.frame(x1=NA_real_, x2=0.3), type = "response")
# 1 
#NA 



library(h2o)

h2oServer <- h2o.init()

dx <- as.h2o(h2oServer, d)

md <- h2o.glm(x = 1:2, y = 3, data = dx, family = "binomial", alpha = 1, lambda = 0)
md

#Coefficients:
#       x1        x2 Intercept 
#  1.19097  10.07690  -0.01690 


h2o.predict(md, as.h2o(h2oServer, data.frame(x1=c(0.2,0.2), x2=c(0.3,0.3))))[,3]
h2o.predict(md, as.h2o(h2oServer, data.frame(x1=c(0.2,NA), x2=c(0.3,0.3))))[,3]

#> h2o.predict(md, as.h2o(h2oServer, data.frame(x1=c(0.2,0.2), x2=c(0.3,0.3))))[,3]
#         X1
#1 0.9624702
#2 0.9624702
#> h2o.predict(md, as.h2o(h2oServer, data.frame(x1=c(0.2,NA), x2=c(0.3,0.3))))[,3]
#         X1
#1 0.9624702
#2        NA


h2o.predict(md, as.h2o(h2oServer, data.frame(x1=NA_real_, x2=0.3)))[,3]

#> h2o.predict(md, as.h2o(h2oServer, data.frame(x1=NA_real_, x2=0.3)))[,3]
#Error in .h2o.__remoteSend(object@data@h2o, .h2o.__PAGE_GLMPREDICT2, model = object@key,  : 
#  http://127.0.0.1:54321/2/GLMPredict.json  returned the following error:
#   Incompatible column: 'x1', expected (trained on) numeric, was passed a categorical

h2o.predict(md, as.h2o(h2oServer, data.frame(x1=0.2, x2=0.3)))[,3]

#> h2o.predict(md, as.h2o(h2oServer, data.frame(x1=0.2, x2=0.3)))[,3]
#Error in function (type, msg, asError = TRUE)  : Empty reply from server

