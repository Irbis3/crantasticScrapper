source("colors.R")
##############################
# RI lines
##############################

color <- broman::brocolors("crayons")[c("Cornflower", "Blush")]

#bitmap(file="../Figs/rilines.bmp", width=9, height=5, res=288,
#       pointsize=14)
pdf("../Figs/rilines.pdf", width=9.75, height=6.5, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4),las=1,fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor,bty="n")
plot(0,0,xlim=c(0,864),ylim=c(25,480),xaxt="n",yaxt="n",xlab="",ylab="",type="n")

rect(c(306,326),c(480,480),c(312,332),c(440,440),col=color[1],border=color[1],
     lend=1,ljoin=1)
rect(c(532,552),c(480,480),c(538,558),c(440,440),col=color[2],border=color[2],
     lend=1,ljoin=1)
points(432,465,pch=4,cex=1.3)
segments(432,450,432,430)
segments(319,430,545,430)
arrows(c(319,545),c(430,430),c(319,545),c(410,410),len=0.1)
text(300-25,460,expression(P[1]),adj=c(1,0.5),cex=1.3)
text(564+25,460,expression(P[2]),adj=c(0,0.5),cex=1.3)

rect(306,400,312,360,col=color[1],border=color[1],
     lend=1,ljoin=1)
rect(532,400,538,360,col=color[1],border=color[1],
     lend=1,ljoin=1)
rect(326,400,332,360,col=color[2],border=color[2],
     lend=1,ljoin=1)
rect(552,400,558,360,col=color[2],border=color[2],
     lend=1,ljoin=1)
text(300-25,380,expression(F[1]),adj=c(1,0.5),cex=1.3)
text(564+25,380,expression(F[1]),adj=c(0,0.5),cex=1.3)
points(432,385,pch=4,cex=1.3)
segments(432,370,432,350)


#f1 <- create.par(100,c(1,2))
#set.seed(994)
#f2 <- vector("list",10)
#for(i in 1:10) f2[[i]] <- cross(f1,f1,m=10,obl=TRUE)
#f3 <- vector("list",10)
#for(i in 1:5) {
#  f3[[2*i-1]] <- cross(f2[[2*i-1]],f2[[2*i]],m=10,obl=TRUE)
#  f3[[2*i]] <- cross(f2[[2*i-1]],f2[[2*i]],m=10,obl=TRUE)
#}
#f4 <- vector("list",10)
#for(i in 1:5) {
# f4[[2*i-1]] <- cross(f3[[2*i-1]],f3[[2*i]],m=10,obl=TRUE)
#  f4[[2*i]] <- cross(f3[[2*i-1]],f3[[2*i]],m=10,obl=TRUE)
#}
#temp <- f4
#my.ri8 <- vector("list",10)
#for(j in 1:40) {
#  for(i in 1:5) {
#    my.ri8[[2*i-1]] <- cross(temp[[2*i-1]],temp[[2*i]],m=10,obl=TRUE)
#    my.ri8[[2*i]] <- cross(temp[[2*i-1]],temp[[2*i]],m=10,obl=TRUE)
#  }
#  temp <- my.ri8
#}
#save(f1,f2,f3,f4,my.ri8,file="for_rilines_fig.RData")
load("for_rilines_fig.RData.gz")

xloc <- 38
mult <- 40/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
xxloc <- NULL
for(i in 1:4) {
  rect(xloc,320,xloc+6,280,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+20,320,xloc+26,280,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+82,320,xloc+88,280,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+102,320,xloc+108,280,col=color[1],border=color[1],
     lend=1,ljoin=1)

  f2m <- f2[[2*i-1]]$mat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc,280+f2m[1,j]*mult,xloc+6,280+f2m[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }
  f2p <- f2[[2*i-1]]$pat
  for(j in 2:ncol(f2p)) {
    if(f2p[2,j]==2)
      rect(xloc+20,280+f2p[1,j]*mult,xloc+26,280+f2p[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }

  f2m <- f2[[2*i]]$mat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc+82,280+f2m[1,j]*mult,xloc+88,280+f2m[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }
  f2p <- f2[[2*i]]$pat
  for(j in 2:ncol(f2p)) {
    if(f2p[2,j]==2)
      rect(xloc+102,280+f2p[1,j]*mult,xloc+108,280+f2p[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }

  xxloc <- c(xxloc,xloc+(6+20)/2,xloc+(88+102)/2)

  points(xloc+54,300,pch=4,cex=1.3)
  segments(xloc+54,285,xloc+54,265)
  segments(xxloc[2*i-1],265,xxloc[2*i],265)

  xloc <- xloc+78+120+30
}

segments(min(xxloc),350,max(xxloc),350)
arrows(xxloc,c(350,350),xxloc,c(330,330),len=0.1)
text(38-25,300,expression(F[2]),adj=c(1,0.5),cex=1.3)
arrows(xxloc,265,xxloc,245,len=0.1)

xloc <- 38
for(i in 1:4) {
  rect(xloc,235,xloc+6,195,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+20,235,xloc+26,195,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+82,235,xloc+88,195,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+102,235,xloc+108,195,col=color[1],border=color[1],
     lend=1,ljoin=1)

  f3m <- f3[[2*i-1]]$mat
  for(j in 2:ncol(f3m)) {
    if(f3m[2,j]==2)
      rect(xloc,195+f3m[1,j]*mult,xloc+6,195+f3m[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }
  f3p <- f3[[2*i-1]]$pat
  for(j in 2:ncol(f3p)) {
    if(f3p[2,j]==2)
      rect(xloc+20,195+f3p[1,j]*mult,xloc+26,195+f3p[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }

  f3m <- f3[[2*i]]$mat
  for(j in 2:ncol(f3m)) {
    if(f3m[2,j]==2)
      rect(xloc+82,195+f3m[1,j]*mult,xloc+88,195+f3m[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }
  f3p <- f3[[2*i]]$pat
  for(j in 2:ncol(f3p)) {
    if(f3p[2,j]==2)
      rect(xloc+102,195+f3p[1,j]*mult,xloc+108,195+f3p[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }

  points(xloc+54,215,pch=4,cex=1.3)
  segments(xloc+54,200,xloc+54,180)
  segments(xxloc[2*i-1],180,xxloc[2*i],180)

  xloc <- xloc+78+120+30
}

text(38-25,215,expression(F[3]),adj=c(1,0.5),cex=1.3)
arrows(xxloc,180,xxloc,160,len=0.1)

xloc <- 38
for(i in 1:4) {
  rect(xloc,150,xloc+6,110,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+20,150,xloc+26,110,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+82,150,xloc+88,110,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+102,150,xloc+108,110,col=color[1],border=color[1],
     lend=1,ljoin=1)

  f4m <- f4[[2*i-1]]$mat
  for(j in 2:ncol(f4m)) {
    if(f4m[2,j]==2)
      rect(xloc,110+f4m[1,j]*mult,xloc+6,110+f4m[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }
  f4p <- f4[[2*i-1]]$pat
  for(j in 2:ncol(f4p)) {
    if(f4p[2,j]==2)
      rect(xloc+20,110+f4p[1,j]*mult,xloc+26,110+f4p[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }

  f4m <- f4[[2*i]]$mat
  for(j in 2:ncol(f4m)) {
    if(f4m[2,j]==2)
      rect(xloc+82,110+f4m[1,j]*mult,xloc+88,110+f4m[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }
  f4p <- f4[[2*i]]$pat
  for(j in 2:ncol(f4p)) {
    if(f4p[2,j]==2)
      rect(xloc+102,110+f4p[1,j]*mult,xloc+108,110+f4p[1,j-1]*mult,col=color[2],border=color[2],
     lend=1,ljoin=1)
  }

  points(xloc+54,130,pch=4,cex=1.3)
  arrows(xloc+54,80,xloc+54,75,len=0.1)
  arrows(xloc+54,115,xloc+54,75,len=0.1,lty=3)
#  segments(xloc+54,115,xloc+54,95)
#  segments(xxloc[2*i-1],95,xxloc[2*i],95)

  xloc <- xloc+78+120+30
}

text(38-25,130,expression(F[4]),adj=c(1,0.5),cex=1.3)


xloc <- 38
a <- 65-40
mult <- (65-a)/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
for(i in 1:4) {
  rect(xloc+54-13,65,xloc+54-7,a,col=color[1],border=color[1],
     lend=1,ljoin=1)
  rect(xloc+54+7,65,xloc+54+13,a,col=color[1],border=color[1],
       lend=1,ljoin=1)

  my.ri8m <- my.ri8[[2*i-1]]$mat
  for(j in 2:ncol(my.ri8m)) {
    if(my.ri8m[2,j]==2)
      rect(xloc+54-13,a+my.ri8m[1,j]*mult,xloc+54-7,a+my.ri8m[1,j-1]*mult,
           col=color[2],border=color[2],
           lend=1,ljoin=1)
  }
  my.ri8p <- my.ri8[[2*i-1]]$pat
  for(j in 2:ncol(my.ri8p)) {
    if(my.ri8p[2,j]==2)
      rect(xloc+54+7,a+my.ri8p[1,j]*mult,xloc+54+13,a+my.ri8p[1,j-1]*mult,
           col=color[2],border=color[2],
           lend=1,ljoin=1)
  }

  xloc <- xloc+78+120+30
}


points(rep(-3,3),c(-8,0,8)+mean(c(130,(65+a)/2)),pch=16,cex=0.4)
text(38-25,(65+a)/2,expression(F[infinity]),adj=c(1,0.5),cex=1.3)

dev.off()
