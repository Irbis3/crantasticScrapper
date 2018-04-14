# Functions for analzing MY dispersal data.

agg <- function(data)
{
dat=ddply(data, .(dist), transform, count=sum(total))
dat=dat[,c(2,7,8,9,11)]
dat2=unique(dat)
return (dat2)
}