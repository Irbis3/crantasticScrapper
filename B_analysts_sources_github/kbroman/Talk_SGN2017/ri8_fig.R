
##############################
# RI lines
##############################

source("colors.R")
color <- c(rgb(102,203,254,maxColorValue=255),
#           rgb(254,102,254,maxColorValue=255),
           rgb(254,  0,128,maxColorValue=255),
           rgb(102,254,102,maxColorValue=255),
           rgb(128,  0,128,maxColorValue=255),
           rgb(203,102,254,maxColorValue=255),
           rgb(254,203,102,maxColorValue=255),
           rgb(  0,128,128,maxColorValue=255),
           rgb(102,102,102,maxColorValue=255))

#bitmap(file="../Figs/ri8.bmp", width=9, height=5, res=288,
#       pointsize=14)
pdf("../Figs/ri8.pdf", width=9.75, height=6.5, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4),las=1,fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor,bty="n")
plot(0,0,xlim=c(0,864),ylim=c(25,480),xaxt="n",yaxt="n",xlab="",ylab="",type="n")

u <- par("usr")
text(u[1],460,expression(G[0]),adj=c(0,0.5))

xloc <- 10
for(i in 1:8) {
  rect(xloc,480,xloc+10,440,col=color[i],border=color[i], lend=1, ljoin=1)
  rect(xloc+28,480,xloc+38,440,col=color[i],border=color[i], lend=1, ljoin=1)
  text(xloc+19,430,LETTERS[i],adj=c(0.5,1))

  if((i %% 2)==0) {
    points(xloc-27.5,460,pch=4,cex=1.3)
    arrows(xloc-27.5,445,xloc-27.5,410,len=0.1)
  }

  xloc <- xloc+38+55
  if((i %% 2)==0) xloc <- xloc+50
}

xloc <- 10+38+55-27.5
xxloc <- NULL
for(i in 1:4) {
  xxloc <- c(xxloc,xloc)
  rect(xloc-19,400,xloc- 9,360,col=color[2*i-1],border=color[2*i-1], lend=1, ljoin=1)
  rect(xloc+ 9,400,xloc+19,360,col=color[2*i],border=color[2*i], lend=1, ljoin=1)
#  text(xloc,350,paste(LETTERS[2*i-1],LETTERS[2*i],sep=""),adj=c(0.5,1))
  text(xloc-14,350,LETTERS[2*i-1],adj=c(0.5,1))
  text(xloc+14,350,LETTERS[2*i],adj=c(0.5,1))
  
  xloc <- xloc+(38+55)*2+50
}
xxloc <- c(mean(xxloc[1:2]),mean(xxloc[3:4]))
points(xxloc[1],380,pch=4,cex=1.3)
arrows(xxloc[1],365,xxloc[1],330,len=0.1)
points(xxloc[2],380,pch=4,cex=1.3)
arrows(xxloc[2],365,xxloc[2],330,len=0.1)
text(u[1],380,expression(G[1]),adj=c(0,0.5))

#f1 <- vector("list",4)
#for(i in 1:4) f1[[i]] <- create.par(100,c(2*i-1,2*i))
#set.seed(112099)
#f2a <- cross(f1[[1]],f1[[2]],m=10,obl=TRUE)
#f2b <- cross(f1[[3]],f1[[4]],m=10,obl=TRUE)
#f3a <- cross(f2a,f2b,m=10,obl=TRUE)
#f3b <- cross(f2a,f2b,m=10,obl=TRUE)
#f4a <- cross(f3a,f3b,m=10,obl=TRUE)
#f4b <- cross(f3a,f3b,m=10,obl=TRUE)
#temp <- list(f4a,f4b)
#for(i in 1:30) {
#  fa <- cross(temp[[1]],temp[[2]],m=10,obl=TRUE)
#  fb <- cross(temp[[1]],temp[[2]],m=10,obl=TRUE)
#  temp <- list(fa,fb)
#}
#save(f1,f2a,f2b,f3a,f3b,f4a,f4b,fa,file="for_ri8_fig.RData")
load("for_ri8_fig.RData.gz")

rect(xxloc[1]-19,320,xxloc[1]- 9,280,col=color[1],border=color[1], lend=1, ljoin=1)
rect(xxloc[1]+ 9,320,xxloc[1]+19,280,col=color[3],border=color[3], lend=1, ljoin=1)
rect(xxloc[2]-19,320,xxloc[2]- 9,280,col=color[5],border=color[5], lend=1, ljoin=1)
rect(xxloc[2]+ 9,320,xxloc[2]+19,280,col=color[7],border=color[7], lend=1, ljoin=1)

mult <- 40/f2a$mat[1,ncol(f2a$mat)]
temp <- f2a$mat
for(j in 2:ncol(temp)) {
  if(temp[2,j]==2)
    rect(xxloc[1]-19,280+temp[1,j]*mult,xxloc[1]-9,280+temp[1,j-1]*mult,
         col=color[2],border=color[2], lend=1, ljoin=1)
}
temp <- f2a$pat
for(j in 2:ncol(temp)) {
  if(temp[2,j]==4)
    rect(xxloc[1]+ 9,280+temp[1,j]*mult,xxloc[1]+19,280+temp[1,j-1]*mult,
         col=color[4],border=color[4], lend=1, ljoin=1)
}
temp <- f2b$mat
for(j in 2:ncol(temp)) {
  if(temp[2,j]==6)
    rect(xxloc[2]-19,280+temp[1,j]*mult,xxloc[2]-9,280+temp[1,j-1]*mult,
         col=color[6],border=color[6], lend=1, ljoin=1)
}
temp <- f2b$pat
for(j in 2:ncol(temp)) {
  if(temp[2,j]==8)
    rect(xxloc[2]+9,280+temp[1,j]*mult,xxloc[2]+19,280+temp[1,j-1]*mult,
         col=color[8],border=color[8], lend=1, ljoin=1)
}

text(u[1],300,expression(G[2]),adj=c(0,0.5))
#text(xxloc,270,c("ABCD","EFGH"),adj=c(0.5,1))
text(xxloc[1]-14,270,"AB",adj=c(0.5,1))
text(xxloc[1]+14,270,"CD",adj=c(0.5,1))
text(xxloc[2]-14,270,"EF",adj=c(0.5,1))
text(xxloc[2]+14,270,"GH",adj=c(0.5,1))
xxloc <- mean(xxloc)
points(xxloc,300,pch=4,cex=1.3)
segments(xxloc,285,xxloc,265)
segments(xxloc-39,265,xxloc+39,265)
arrows(xxloc+39*c(-1,1),265,xxloc+39*c(-1,1),245,len=0.1)


rect(xxloc-58,235,xxloc-48,195,col=color[1],border=color[1], lend=1, ljoin=1)
rect(xxloc-30,235,xxloc-20,195,col=color[5],border=color[5], lend=1, ljoin=1)
rect(xxloc+20,235,xxloc+30,195,col=color[1],border=color[1], lend=1, ljoin=1)
rect(xxloc+48,235,xxloc+58,195,col=color[5],border=color[5], lend=1, ljoin=1)

temp <- f3a$mat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=1)
    rect(xxloc-58,195+temp[1,j]*mult,xxloc-48,195+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
temp <- f3a$pat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=5)
    rect(xxloc-30,195+temp[1,j]*mult,xxloc-20,195+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
temp <- f3b$mat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=1)
    rect(xxloc+20,195+temp[1,j]*mult,xxloc+30,195+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
temp <- f3b$pat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=5)
    rect(xxloc+48,195+temp[1,j]*mult,xxloc+58,195+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
text(u[1],215,expression(G[3]),adj=c(0,0.5))
points(xxloc,215,pch=4,cex=1.3)
segments(xxloc,200,xxloc,180)
segments(xxloc-39,180,xxloc+39,180)
arrows(xxloc+39*c(-1,1),180,xxloc+39*c(-1,1),160,len=0.1)


rect(xxloc-58,150,xxloc-48,110,col=color[1],border=color[1], lend=1, ljoin=1)
rect(xxloc-30,150,xxloc-20,110,col=color[1],border=color[1], lend=1, ljoin=1)
rect(xxloc+20,150,xxloc+30,110,col=color[1],border=color[1], lend=1, ljoin=1)
rect(xxloc+48,150,xxloc+58,110,col=color[1],border=color[1], lend=1, ljoin=1)

temp <- f4a$mat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=1)
    rect(xxloc-58,110+temp[1,j]*mult,xxloc-48,110+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
temp <- f4a$pat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=1)
    rect(xxloc-30,110+temp[1,j]*mult,xxloc-20,110+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
temp <- f4b$mat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=1)
    rect(xxloc+20,110+temp[1,j]*mult,xxloc+30,110+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
temp <- f4b$pat
for(j in 2:ncol(temp)) {
  if(temp[2,j]!=1)
    rect(xxloc+48,110+temp[1,j]*mult,xxloc+58,110+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
text(u[1],130,expression(G[4]),adj=c(0,0.5))
points(xxloc,130,pch=4,cex=1.3)
arrows(xxloc,80,xxloc,75,len=0.1)
arrows(xxloc,115,xxloc,75,len=0.1,lty=3)

points(rep(u[1]+10,3),c(-8,0,8)+mean(c(130,65/2)),pch=16,cex=0.4)
a <- 65-40
text(u[1],(65+a)/2,expression(G[infinity]),adj=c(0,0.5))
mult <- (65-a)/fa$mat[1,ncol(fa$mat)]
rect(xxloc+19*c(-1,1),rep(65,2),xxloc+9*c(-1,1),rep(a,2),col=color[1],border=color[1], lend=1, ljoin=1)
temp <- fa$mat
for(j in 2:ncol(fa$mat)) {
  if(temp[2,j]!=1)
    rect(xxloc+19*c(-1,1),rep(a,2)+temp[1,j]*mult,xxloc+9*c(-1,1),rep(a,2)+temp[1,j-1]*mult,
         col=color[temp[2,j]],border=color[temp[2,j]], lend=1, ljoin=1)
}
dev.off()
