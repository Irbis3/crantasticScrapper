ContrBB<-function(data, vars, Level=NULL, Taxon=NULL, graph="beanplot", order="decreasing", side="no", beanlines="median",
what=c(1,1,1,1), ll=NULL, border="black", OrderCat=NULL, LabelCat=NULL, XLAB="Variables",
YLAB="Percentage of contribution to instability index", COLOR=NULL, LEGEND=NULL, MTEXT= NULL, TEXT=NULL,
ResetPAR=TRUE, PAR=NULL, BEANPLOT=NULL, BOXPLOT=NULL){

####Selection of variables

datos<-data

if(!is.null(Level)){datos<-subset(datos,datos[,Level] %in% Taxon)}

datos<-data.frame(subset(datos, select=vars))


####Organizing the data

dimd<-dim(datos)

for(n in 1:dimd[2]){

if(n==1){
dat<-data.frame(vars[n],datos[,n])
colnames(dat)<-c("Variable","Contribution")
}
else{
dat1<-data.frame(vars[n],datos[,n])
colnames(dat1)<-c("Variable","Contribution")
dat<-rbind(dat,dat1)
}#End if
}#End for



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
par(font.lab=2, mar=c(5,5,3,2),cex.lab=1.4)
}

remove(datos)
remove(data)

####Assign the colors
if(!is.null(COLOR)){
color<-COLOR
color1<-as.list(COLOR)
}
else{
color<-terrain.colors(length(unique(dat[,1])))
color1<-as.list(color)
}

####Length of the beanline per point found

if(is.null(ll)) ll<-0.075-0.000005*dimd[1] else ll<-ll

####Organizing the labels of X axis
dat[,1]<-factor(dat[,1], levels = unique(dat[,1]), labels = unique(dat[,1]))

if(beanlines=="quantiles") valor<-"median" else valor<-beanlines
median<- tapply(dat[, 2], dat[, 1], valor)


if(!is.null(OrderCat)){
dat[,1]<-factor(dat[,1], levels = OrderCat, labels = LabelCat)
}
else{
if(!is.null(order)){
if(order=="decreasing") names<-names(sort(median, decreasing=TRUE)) else names<-names(sort(median, decreasing=FALSE))

if(order=="alphaAZ") names<-sort(names, decreasing=FALSE) else names<-names

if(order=="alphaZA") names<-sort(names, decreasing=TRUE) else names<-names

if(!is.null(LabelCat))  LabelCat<-LabelCat else LabelCat<-names
dat[,1]<-factor(dat[,1], levels = names, labels = LabelCat)
}
else{
if(!is.null(LabelCat)) dat[,1]<-factor(dat[,1], levels = unique(dat[,1]), labels = LabelCat) else dat<-dat
}
}


if(graph=="beanplot"){
####Beanplot
if(!is.null(BEANPLOT)){
beanplotexe<-paste("beanplot::beanplot(","dat[,2]~dat[,1],",toString(x=BEANPLOT), ")")
eval(parse(text=beanplotexe))
}
else{
beanplotexe<-paste("beanplot::beanplot(","dat[,2]~dat[,1],",
"xlab=XLAB,", "ylab=YLAB,","side=side,", "beanlines=beanlines,",
"what=what,", "border=border,","col=color1,", "ll=ll", ")")
eval(parse(text=beanplotexe))
}
}
else{
####Boxplot
if(!is.null(BOXPLOT)){
boxplotexe<-paste("boxplot(","dat[,2]~dat[,1],",toString(x=BOXPLOT), ")")
eval(parse(text=boxplotexe))
}
else{
boxplotexe<-paste("boxplot(","dat[,2]~dat[,1],",
"xlab=XLAB,", "ylab=YLAB,","col=color",")")
eval(parse(text=boxplotexe))
}
}


####It is possible to add a legend
if(!is.null(LEGEND)){
legendexe<-paste("legend(",toString(x=LEGEND), ")")
eval(parse(text=legendexe))
}
else{
}

####It is possible to add text in the axes 
if(!is.null(MTEXT)){
mtextexe<-paste("mtext(",toString(x=MTEXT), ")")
eval(parse(text=mtextexe))
}
else{
}

####It is possible to add text inside the plot
if(!is.null(TEXT)){
textexe<-paste("text(",toString(x=TEXT), ")")
eval(parse(text=textexe))
}
else{
}


}

