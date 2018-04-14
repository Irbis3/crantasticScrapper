
set.seed(123)

N <- 10000

x1 <- sample(c("a","b","c"), N, replace = TRUE)
x2 <- runif(N, min = -1, max = 1)

z <- 1 * ifelse(x1=="b",1,0) + 10 * x2
p <- 1/(1+exp(-z))
y <- rbinom(N,1,p)

d <- data.frame(x1=x1, x2=x2, y=y)

md <- glm(y ~ ., data = d, family = binomial())
md

#Coefficients:
#(Intercept)          x1b          x1c           x2  
#    0.02925      1.01760     -0.03212     10.28583  


predict(md, newdata = data.frame(x1="a", x2=0.3), type = "response")
predict(md, newdata = data.frame(x1="d", x2=0.3), type = "response")

#> predict(md, newdata = data.frame(x1="a", x2=0.3), type = "response")
#        1 
#0.9575072 
#> predict(md, newdata = data.frame(x1="d", x2=0.3), type = "response")
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#  factor x1 has new level d



library(h2o)

h2oServer <- h2o.init()

dx <- as.h2o(h2oServer, d)

md <- h2o.glm(x = 1:2, y = 3, data = dx, family = "binomial", alpha = 1, lambda = 0)
md

#Coefficients:
#     x1.b      x1.c        x2 Intercept 
#  1.01742  -0.03210  10.28373   0.02923 


h2o.predict(md, as.h2o(h2oServer, data.frame(x1="a", x2=0.3)))[,3]
h2o.predict(md, as.h2o(h2oServer, data.frame(x1="d", x2=0.3)))[,3]

#> h2o.predict(md, as.h2o(h2oServer, data.frame(x1="a", x2=0.3)))[,3]
#         X1
#1 0.9574805
#> h2o.predict(md, as.h2o(h2oServer, data.frame(x1="d", x2=0.3)))[,3]
#         X1
#1 0.9841991



