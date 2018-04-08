PreExt<-function(data, var, envar, Interval, Interval.Value, Pre, Extent,
Level=NULL, Taxon=NULL, ResetPAR=TRUE, PAR=NULL,
PLOT=NULL, XLAB=NULL, YLAB="Frequency", MAIN=NULL,
COLOR=c("#00FF0032","#FF000032"), XLIM=NULL, YLIM=NULL, TYPE="l",
LTY=c(1,2), PCH=NULL, LEGEND=NULL, AXIS=NULL, MTEXT= NULL, TEXT=NULL){

#Selection of taxa

if(!is.null(Level)){data<-subset(data,data[,Level] %in% Taxon)}

#Selection of the environmental variable

data<-subset(data,data[,var] %in% envar)

if(ResetPAR==TRUE){
#Resetear par() a las opciones por defecto
resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar()) }
else{
}


if(!is.null(PAR)){
parexe<-paste("par(new,",toString(x=PAR), ")")
eval(parse(text=parexe))
}
else{
par(font.lab=2, mar=c(5,5,3,2),cex.lab=1.5)
}

#Means of intervals for Extent and Presence

meanP<-tapply(X=data[,Pre], INDEX=data[, Interval], FUN=mean , na.rm=TRUE)

meanA<-tapply(X=data[,Extent], INDEX=data[, Interval], FUN=mean , na.rm=TRUE)


#SDs of intervals for Extent and Presence

sdP<-tapply(X=data[,Pre], INDEX=data[, Interval], FUN=sd , na.rm=TRUE)

sdA<-tapply(X=data[,Extent], INDEX=data[, Interval], FUN=sd , na.rm=TRUE)

#Means of intervals for the variable

meanX<-tapply(X=data[,Interval.Value], INDEX=data[, Interval], FUN=mean , na.rm=TRUE)



if(anyNA(sdP)==TRUE){
UP<-meanP
DP<-meanP
}
else{
UP<-meanP+sdP
DP<-meanP-sdP
}


if(anyNA(sdA)==TRUE){
UA<-meanA
DA<-meanA
}
else{
UA<-meanA+sdA
DA<-meanA-sdA
}

if(is.null(XLIM)){
XLIM<-c(min(meanX),max(meanX))
}
else{
XLIM=XLIM
}

if(is.null(YLIM)){
YLIM=c(min(c(DP,DA) , na.rm = TRUE),max(c(UP, UA), na.rm=TRUE))
}
else{
YLIM=YLIM
}

if(is.null(XLAB)){
XLAB<-envar
}
else{
XLAB<-XLAB

}

####Plot

t1<-substr(COLOR[1], 1, nchar(COLOR[1])-2)
t2<-substr(COLOR[2], 1, nchar(COLOR[2])-2)
col1<-paste(t1,"FF" , sep = "")
col2<-paste(t2,"FF" , sep = "")

if(!is.null(PLOT)){
plotexe<-paste("plot(",toString(x=PLOT), ")")
eval(parse(text=plotexe))
}
else{
plotexe<-paste("plot(","x=meanX,", "y=meanP,",
"xlab=XLAB,", "ylab=YLAB,","type=TYPE,", "pch=PCH[1],", "lty=LTY[1],",
"col=col1,", "main=MAIN,","xlim=XLIM,", "ylim=YLIM", ")")
eval(parse(text=plotexe))
}

points(x=meanX, y=meanA, pch=PCH[2], type=TYPE, lty=LTY[2], col=col2,
xlim=XLIM, ylim=YLIM)

polygon(x=c(meanX,rev(meanX)), y=c(UP,rev(DP)), border=NA, col=COLOR[1])

polygon(x=c(meanX,rev(meanX)), y=c(UA,rev(DA)), border=NA, col=COLOR[2])

if(!is.null(LEGEND)){
legendexe<-paste("legend(",toString(x=LEGEND), ")")
eval(parse(text=legendexe))
}
else{
legendexe<-paste("legend(","x='topleft',","legend=c('Presence','Extent'),","bty='n',",
"lty=LTY,", "pch=PCH,", "col=c(col1,col2)", ")")
eval(parse(text=legendexe))
}


if(!is.null(AXIS)){
axisexe<-paste("axis(",toString(x=AXIS), ")")
eval(parse(text=axisexe))
}
else{
}

if(!is.null(MTEXT)){
mtextexe<-paste("mtext(",toString(x=MTEXT), ")")
eval(parse(text=mtextexe))
}
else{

}

if(!is.null(TEXT)){
textexe<-paste("text(",toString(x=TEXT), ")")
eval(parse(text=textexe))
}
else{
}


}