# Kernel Estimation Comparsion
# Part I
require(parallelMap)
require(ggplot2)

# Monte Carlo simulation
set.seed(123)
# Problem 1, Question 1
samples <- c("norm","exp","unif","beta","gamma")
sample.settings <- list(norm=list(mean=0,sd=1),
                        exp=list(rate=1),
                        unif=list(min=0,max=1),
                        beta=list(shape1=2,shape2=5),
                        gamma=list(shape=9,scale=0.5))
sizes <- c(200,800)
kernels <- c("gaussian","epanechnikov","rectangular",
             "triangular","cosine","optcosine","biweight")
bws <- c("nrd0","nrd","sj","ucv","bcv")
test.table <- expand.grid(sample=samples,size=sizes,
                          kernel=kernels,bw=bws,
                          stringsAsFactors=FALSE)
kest <- function(i,args,sample.settings) {
  settings <- sample.settings[[args$sample]]
  sample <- do.call(sprintf("r%s",args$sample),
                    c(list(n=args$size),settings))
  kden <- density(sample,bw=args$bw,kernel=args$kernel)
  den <- do.call(sprintf("d%s",args$sample),
                  c(list(x=kden$x),settings))
  transform(args,made=mean(abs(den-kden$y)))
}

parallelStartSocket(cpus=4,show.info=FALSE)
test.result <- do.call(rbind,
                       lapply(1:nrow(test.table),
                              function(i,fun,...) {
  print(sprintf("Running for row %d",i))
  do.call(rbind,parallelLapply(1:1000,fun,args=test.table[i,],
                                 sample.settings=sample.settings))
  },fun=kest,test.table=test.table,sample.settings=sample.settings))
parallelStop()

ggplot(data=test.result,aes(x=kernel,y=made,fill=kernel))+
  geom_boxplot()+
  facet_grid(sample~size+bw,scales="free_y")+
  ggtitle("Kernel estimation errors under different settings")+
  scale_x_discrete(breaks=NULL)+
  theme(legend.position="top",legend.direction="horizontal")

qplot(made,..density..,data=test.result,geom="density",color=sample)+
  facet_grid(kernel~size+bw)+
  ggtitle("Kernel estimation errors under different settings")+
  theme(legend.position="top",legend.direction="horizontal")

# Part II
# Project 2 Local density method
require(locfit)
require(ggplot2)
x.norm <- rnorm(2000)
x.norm.density <- density(x.norm)
x.norm.local <- density.lf(x.norm)
x.norm.df <- rbind(data.frame(label="pdf",
                              x=x.norm.density$x,
                              y=dnorm(x.norm.density$x)),
                   data.frame(label="kernel",
                              x=x.norm.density$x,
                              y=x.norm.density$y),
                   data.frame(label="local",
                              x=x.norm.local$x,
                              y=x.norm.local$y))
qplot(x,y,data=x.norm.df,color=label,fill=label,geom="line")+
  ggtitle("Normal distribution")


x.unif <- runif(2000)
x.unif.density <- density(x.unif)
x.unif.local <- density.lf(x.unif)
x.unif.df <- rbind(data.frame(label="pdf",
                              x=x.unif.density$x,
                              y=dunif(x.unif.density$x)),
                   data.frame(label="kernel",
                              x=x.unif.density$x,
                              y=x.unif.density$y),
                   data.frame(label="local",
                              x=x.unif.local$x,
                              y=x.unif.local$y))
qplot(x,y,data=x.unif.df,color=label,geom="line")


ade <- function(x,y,range=c(0,1),bounded=FALSE) {
  if(bounded) {index <- x>=range[1] & x<=range[2]}
    else {index <- 1:length(x)}
  xs <- x[index]
  ys <- y[index]
  yp <- dunif(xs,min=range[1],max=range[2])
  list(mean=mean(abs(yp-ys)),sum=sum(abs(yp-ys)))
}

simulate <- function(i,ade,range=c(0,1),size=1000) {
  require(locfit)
  sample <- runif(size,min=range[1],max=range[2])
  sample.density <- density(sample)
  sample.local <- density.lf(sample)
  sample.density.ade <- with(sample.density,ade(x,y,range,FALSE))
  sample.local.ade <- with(sample.local,ade(x,y,range,FALSE))
  sample.density.bounded.ade <- with(sample.density,ade(x,y,range,TRUE))
  sample.local.bounded.ade <- with(sample.local,ade(x,y,range,TRUE))
  rbind(data.frame(method="kernel",
                   min=range[1],max=range[2],sample.size=size,
                   made=sample.density.ade$mean,
                   bounded.made=sample.density.bounded.ade$mean,
                   boundary.effect=(sample.density.ade$sum-
                                      sample.density.bounded.ade$sum)/
                                  sample.density.ade$sum),
        data.frame(method="local",
                   min=range[1],max=range[2],sample.size=size,
                   made=sample.local.ade$mean,
                   bounded.made=sample.local.bounded.ade$mean,
                   boundary.effect=(sample.local.ade$sum-
                                      sample.local.bounded.ade$sum)/
                     sample.local.ade$sum))
}

simulate.bounded <- function(i,ade,range=c(0,1),size=1000) {
  require(locfit)
  sample <- runif(size,min=range[1],max=range[2])
  sample.density <- density(sample,from=range[1],to=range[2])
  sample.local <- density.lf(sample,from=range[1],to=range[2])
  sample.density.made <- with(sample.density,ade(x,y,range,FALSE))$mean
  sample.local.made <- with(sample.local,ade(x,y,range,FALSE))$mean
  rbind(data.frame(method="kernel",
                   min=range[1],max=range[2],sample.size=size,
                   made=sample.density.made),
        data.frame(method="local",
                   min=range[1],max=range[2],sample.size=size,
                   made=sample.local.made))
}

require(parallelMap)
parallelStartSocket(4)
boundary.effect.df <- do.call("rbind",parallelLapply(1:1000,simulate,ade=ade))
require(grid)
vp <- function(x,y) viewport(layout.pos.row=x,layout.pos.col=y)

plot1 <- qplot(made,data=boundary.effect.df,
      group=method,color=method,fill=method,geom="density",alpha=0.5)+
  ggtitle("Mean absolute deviation errors")
plot2 <- qplot(bounded.made,data=boundary.effect.df,
      group=method,color=method,fill=method,geom="density",alpha=0.5)+
  ggtitle("Mean absolute deviation errors within support")
plot3 <- qplot(boundary.effect,data=boundary.effect.df,
      group=method,color=method,fill=method,geom="density",alpha=0.5)+
  ggtitle("Boundary Effect")

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(plot1,vp=vp(1,1))
print(plot2,vp=vp(1,2))
print(plot3,vp=vp(1,3))

dlf.df <- do.call("rbind",parallelLapply(1:1000,simulate.bounded,ade=ade))
qplot(made,data=dlf.df,
      group=method,color=method,fill=method,geom="density",alpha=0.5)+
  ggtitle("Restricted estimation within support")


# Part III: Empirical project
DTB3 <- read.delim("data/DTB3.txt",na.strings=".")
TB3MS <- read.delim("data/TB3MS.txt",na.strings=".")
WTB3MS <- read.delim("data/WTB3MS.txt",na.strings=".")

library(xts)
DTB3.ts <- xts(DTB3$DTB3,order.by=as.Date(DTB3$DATE))
WTB3MS.ts <- xts(WTB3MS$WTB3MS,order.by=as.Date(WTB3MS$DATE))
TB3MS.ts <- xts(TB3MS$TB3MS,order.by=as.Date(TB3MS$DATE))
par(mfrow=c(3,1))
plot(DTB3.ts,type="l")
plot(WTB3MS.ts,type="l")
plot(TB3MS.ts,type="l")

Box.test(DTB3.ts)
Box.test(WTB3MS.ts)
Box.test(TB3MS.ts)

par(mfrow=c(3,2))
acf(DTB3.ts,na.action=na.pass)
pacf(DTB3.ts,na.action=na.pass)
acf(WTB3MS.ts,na.action=na.pass)
pacf(WTB3MS.ts,na.action=na.pass)
acf(TB3MS.ts,na.action=na.pass)
pacf(TB3MS.ts,na.action=na.pass)

par(mfrow=c(3,2))
acf(diff(DTB3.ts),na.action=na.pass)
pacf(diff(DTB3.ts),na.action=na.pass)
acf(diff(WTB3MS.ts),na.action=na.pass)
pacf(diff(WTB3MS.ts),na.action=na.pass)
acf(diff(TB3MS.ts),na.action=na.pass)
pacf(diff(TB3MS.ts),na.action=na.pass)

require(ggplot2)
DTB3.density <- density(DTB3$DTB3,kernel="gaussian",na.rm=T)
ggplot(DTB3,aes(x=DTB3))+
  geom_histogram(aes(y=..density..),alpha=0.4,
                 binwidth=0.5,color="black",fill="white")+
  geom_density(alpha=0.4,fill="grey",color="darkblue")

DTB3.df <- transform(DTB3,LABEL="DTB3",VALUE=DTB3)[,c("LABEL","DATE","VALUE")]
WTB3MS.df <- transform(WTB3MS,LABEL="WTB3MS",VALUE=WTB3MS)[,c("LABEL","DATE","VALUE")]
TB3MS.df <- transform(TB3MS,LABEL="TB3MS",VALUE=TB3MS)[,c("LABEL","DATE","VALUE")]
TB3 <- rbind(DTB3.df,WTB3MS.df,TB3MS.df)
ggplot(TB3,aes(x=VALUE,fill=LABEL))+
  geom_histogram(aes(y=..density..),alpha=0.6,binwidth=0.25)+
  geom_density(alpha=0.4,color=0,fill="black")+
  facet_grid(LABEL~.)+
  theme(legend.position="top",legend.direction="horizontal")