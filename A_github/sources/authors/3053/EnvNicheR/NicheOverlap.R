NicheOverlap<-function(data, Level1, Taxon1, Level2=Level1, Taxon2,
colA=hsv(h=0,s=1,v=1, alpha=0.4),colB=hsv(h=0.7,s=1,v=1, alpha=0.4),
xlab="Polar coordinate X in pixel", ylab="Polar coordinate Y in pixels",
cex=1.57, cex.lab=1.5,font.lab=1, main="",cex.main = 2, font.main=2,
family="serif", digits =2, xlegend="topleft",ylegend=NULL, pch=15,
bty="n",text.font=3, cex.legend=1.2, ncol=1, x.intersp = 1, y.intersp = 1,
legend=TRUE){

datos<-na.exclude(data)


A<-subset(datos,datos[,Level1] %in% Taxon1)
B<-subset(datos,datos[,Level2] %in% Taxon2)


###################
#Overlap

AA<-cbind(A$Pixel.X,A$Pixel.Y)
AA<-unique(AA)
DA<-dim(AA)

BB<-cbind(B$Pixel.X,B$Pixel.Y)
BB<-unique(BB)
DB<-dim(BB)

CC<-rbind(AA,BB)
DUP<-duplicated(CC)
DC<-length(DUP[DUP==TRUE])


OA<-DC*100/DA[1]
OB<-DC*100/DB[1]

OA<-round(OA,digits=digits)
OB<-round(OB,digits=digits)

####################

par(family=family)

#Plot
par(mfrow = c(1,1))
plot(A$Pixel.X,A$Pixel.Y, pch=pch, cex=cex, col=colA, xlim=c(0,30), ylim=c(0,30),
xlab=xlab, ylab=ylab, cex.lab=cex.lab, font.lab=font.lab, main=main, cex.main=cex.main,
font.main=font.main)
points(B$Pixel.X,B$Pixel.Y, pch=pch, cex=cex, col=colB)


if(legend==TRUE){
legend(x=xlegend,y=ylegend, legend=c(paste(Taxon1,OA,"%"),paste(Taxon2,OB,"%")), col=c(colA,colB), pch=pch, bty=bty,
text.font=text.font, cex=cex.legend, x.intersp=x.intersp, y.intersp=y.intersp, ncol=ncol)
}
else{
www<-1
}

}
