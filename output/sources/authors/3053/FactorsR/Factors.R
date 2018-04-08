Factors<-function(data, varY,  varX, Area="World", outliers=FALSE,
quant1=0.05, quant2 = 0.95, stepwise=FALSE, direction="both", deplog=FALSE, indlog=FALSE,
zero=TRUE, vifmethod="hier.part", threshold=10, PAIRS=TRUE, CoVa=FALSE, SVM=TRUE,
Map=TRUE, TXT=TRUE, jpg=TRUE, jpg1="Correlation matrix.jpg",  jpg2="Contribution in the regression model.jpg",
jpg3="Residuals.jpg", jpg4="Actual richness.jpg", jpg5="Predicted richness.jpg",
jpg6="Contribution by Hierarchical Partitioning-All variables.jpg", 
jpg7="Contribution by Hierarchical Partitioning-Selected variables.jpg", pairs=NULL,
colhist="#00FFFFFF", ksvm=NULL, typeV="lmg", rela=TRUE, b=1000, names.abbrev=15, ylimV=NULL,
mainV=NULL, cex.title=1.5, minLon, maxLon, minLat, maxLat, colbg="#FFFFFF", colcon="#C8C8C8",
colf="black", pro=TRUE, inc=0.005, exclude=NULL, colexc=NULL, colfexc="black",
colscale1=c("#FFFFFFFF", "#FF0080FF","#FF00FFFF","#00FFFFFF", "#64FF64FF","#C8FF00FF",
 "#FFFF00FF","#FFC800FF","#FF6400FF","#FF0000FF"), colscale2=c("#FFFFFFFF", "#C8FFFFFF",
"#64FFFFFF","#00FFFFFF",   "#64FF64FF","#C8FF00FF","#FFFF00FF","#FFC800FF","#FF6400FF",
"#FF0000FF"),legend.pos="y", breaks=10, xl=0, xr=0, yb=0, yt=0, asp, lab=NULL, xlab="Longitude",
ylab="Latitude", main1="Residuals of SVM model", main2="Actual richness",
main3="Richness predicted by SVM model",  cex.main=1.6, cex.lab=1.4, cex.axis=1.2, family="sans",
font.main=2, font.lab=1, font.axis=1, lwdP=0.6, lwdC=0.1, trans=c(1,1), log=c(0,0),
ndigits=0, ini, end=NULL, file1="Output.txt", CSV1="Residuals of SVM model.csv",
CSV2="Actual richness.csv", CSV3="Richness predicted by SVM model.csv", 
CSV4="Residuals of the multiple regression model.csv", na="NA",
dec=",", row.names=FALSE){


#####Checking data required
if(exists("adworld")==FALSE){
adworld<-1
stop("It is necessary to load data(adworld)")
}

if(Area!="World" & exists("adworld1")==FALSE){
stop("It is necessary to use RWizard and replace data(adworld) by @_Build_AdWorld_, for using administative areas")
}

if(Area!="World" & exists("adworld2")==FALSE){
stop("It is necessary to use RWizard and replace data(adworld) by @_Build_AdWorld_, for using administative areas")
}

if(exists("adworld1")==FALSE){
adworld1<-1
}

if(exists("adworld2")==FALSE){
adworld2<-1
}

## Obtained form the package SciViews
panel.reg <- function (x, y, col = par("col"), bg = par("bg"), pch = par("pch"),
cex = par("cex"), lwd = par("lwd"), line.reg = lm, line.col = "red",
line.lwd = lwd, untf = TRUE, ...) 
{
    points(x, y, col = col, bg = bg, pch = pch, cex = cex)
    if (is.function(line.reg)) 
        abline(reg = line.reg(y ~ x), col = line.col, lwd = line.lwd,
			untf = untf, ...)
}

## Obtained form the package SciViews
panel.hist <- function(x, ...)
{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5));h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col=colhist, ...)
}

f<-(data[1,1]-data[2,1])

dimdatos<-dim(data)
maxt<-data[,1:2]

maxt[,1]<-floor((90-maxt[,1])/(90-maxt[1,1]))
maxt[,2]<-floor((180+maxt[,2])/(180+maxt[1,2]))

colnames(maxt)<-c("R","C")

datos<-data.frame(maxt,data)
data<-datos
if(zero==TRUE) datos<-datos else datos<-datos[which(datos[,varY]>0),]

datosT<-data.frame(datos[,1:2],subset(datos, select=varY), subset(datos, select=varX))

datos<-na.exclude(datosT)
row.names(datos) <- NULL



panel.hist <- function(x, ...)
{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5));h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col=colhist, ...)
}


ZZ<-matrix(c("","","","","","","",""), nrow=4)

#Outliers
if(outliers==TRUE){
begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[2,1]<-"Removing outliers...."
ZZ[3,1]<-begin.times
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(quant1, quant2), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


datos2<-data.frame(datos[,varY],datos[,varX])
colnames(datos2)<-c(varY,varX)
zz<-dim(datos2)
fo<-paste(names(datos2)[1], "~", names(datos2)[2])
for(i in 3:zz[2]){
fo<-paste(fo, "+", names(datos2)[i])
}
reg<-lm(fo,data=datos2)
res<-residuals(reg)


resOut<- remove_outliers(res)

datosOut<-data.frame(datos,resOut)

datosOut<-na.exclude(datosOut)
dif<-dim(datosOut)
vex<-apply(datosOut[,-dif[2]],2,sd)
if(any(vex == 0)) datos<-na.exclude(datos) else datos<-na.exclude(datosOut)

}

datosT<-datos

datos<-datos[,c(-1,-2)]

datos2<-data.frame(datos[,varY],datos[,varX])
colnames(datos2)<-c(varY,varX)

if(deplog==TRUE){
YLN<-log(datos2[,varY]-(min(datos2[,varY])-1))
datos2<-data.frame(YLN, datos2[,varX])
colnames(datos2)<-c(paste("LN", varY, sep=""),varX)
}

if(indlog==TRUE){
datosLN<-datos2[, varX]
dimT<-dim(datos2[, varX])
for(i in 1:dimT[2]){
datosLN[,i]<-log(datos2[,varX[i]]-(min(datos2[,varX[i]])-1))
}
colLN<-colnames(datosLN)
lenLN<-length(colnames(datosLN))
for(i in 1:lenLN){
colLN[i]<-paste("LN", colLN[i], sep="")
}
colnames(datosLN)<-colLN
datos2<-data.frame(datos2,datosLN)
}


datos<-datos2


#Colinearity
begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[2,1]<-"Estimating VIF and removing auto-correlated variables (be patient)...."
ZZ[3,1]<-begin.times
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

VIF1<-usdm::vif(datos2)

if(deplog==TRUE) VIF1<-VIF1[VIF1$Variables!=paste("LN", varY, sep=""),] else VIF1<-VIF1[VIF1$Variables!=varY,]

VIF2<-"No estimated"

CT<-"No estimated"

hjkl<-0

if(!is.null(vifmethod)){
if(vifmethod=="vif"){
write.csv2(datos2,"VIF_variables.csv",row.names=FALSE)
write.table(threshold,"threshold.txt", row.names=FALSE,col.names=FALSE)
write.table(threshold,"VIF.txt", row.names=FALSE,col.names=FALSE)
while(file.exists("VIF_Created.txt") ==FALSE){
}
file.remove("VIF_Created.txt")
if(file.exists("VIF.txt") ) {file.remove("VIF.txt")}
VIF2<-read.csv2(file = "VIF_Result.csv", header=TRUE, encoding="latin1")
}


if(vifmethod=="vifcor"){
VIF2<-usdm::vifcor(datos2)
VIF2<-attributes(VIF2)
VIF2<-VIF2$results
}

if(vifmethod=="vifstep"){
VIF2<-usdm::vifstep(datos2, th=threshold)
VIF2<-attributes(VIF2)
VIF2<-VIF2$results
}



if(vifmethod=="hier.part"){
dit<-dim(datos2)
if(dit[2]>13){
VIF2<-usdm::vifstep(datos2, th=threshold)
VIF2<-attributes(VIF2)
VIF2<-VIF2$results

}
else{
hjkl<-1

hie1<-hier.part::hier.part(datos2[,1], datos2[,-1], barplot = FALSE)
VIF2<-usdm::vif(datos2[,-1])
CT<-hie1$I.perc
if(max(VIF2[,2])>threshold){
while(max(VIF2[,2])>threshold){ 
hie<-hier.part::hier.part(datos2[,1], datos2[,-1], barplot = FALSE)
VIFmatriz<-cbind(hie$I.perc,VIF2[,2])
VIFmatriz1<-VIFmatriz[which(VIFmatriz[,2]>threshold),]
eliminar<-rownames(which(VIFmatriz1 == min(VIFmatriz1[,1]), arr.ind = TRUE))
datos2<-datos2[ , -which(names(datos2) %in% eliminar)]
VIF2<-usdm::vif(datos2[,-1])
}
}

VIF2<-usdm::vifstep(datos2, th=threshold)
VIF2<-attributes(VIF2)
VIF2<-VIF2$results

CTT<-hie1$I.perc
CTT<-data.frame(Variables=rownames(CTT), CTT)
CTT<-CTT[order(CTT$I, decreasing = TRUE), ]
if(jpg==TRUE){
jpeg(filename = jpg6, width = 12000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}
barplot(CTT$I, names.arg=CTT$Variables, ylab="% Independent effects (%I)", main="All variables", font.lab=2, font.axis=2, cex.lab=1.2)

if(jpg==TRUE){
dev.off()
}


}

}


VIF2<-data.frame(VIF2, VIF2)



dimT<-dim(VIF2)

VIF2[,3]<-gsub("LN","",VIF2[,3])

for(i in 1:dimT[1]){
fo<-paste(names(datos2)[1], "~", VIF2[i,1])
reg<-lm(fo, data=datos2)
mod<-summary(reg)
VIF2[i,4]<-mod$r.squared
}

VIF2<-VIF2[order(VIF2[,4], decreasing = TRUE), ]

VIF2<-VIF2[!duplicated(VIF2[,3]),]

if(deplog==TRUE) VIF2<-VIF2[VIF2$Variables!=paste("LN", varY, sep=""),] else VIF2<-VIF2[VIF2$Variables!=varY,]


VIF2<-VIF2[,1:2]

datos2<-subset(datos2, select=as.character(unique(VIF2[,1])))

if(deplog==TRUE) datos3<-data.frame(datos[,paste("LN", varY, sep="")],datos2) else datos3<-data.frame(datos[,varY],datos2)

if(deplog==TRUE) colnames(datos3)<-c(paste("LN", varY, sep=""), colnames(datos2)) else colnames(datos3)<-c(varY, colnames(datos2))
datos2<-datos3
remove(datos3)


}



if(hjkl==1){
if(jpg==TRUE){
jpeg(filename = jpg7, width = 12000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}

dit2<-dim(datos2)
if(dit[2]==dit2[2]){
barplot(CTT$I, names.arg=CTT$Variables, ylab="% Independent effects (%I)",main="Selected variables", font.lab=2, font.axis=2, cex.lab=1.2)
}
else{
hie<-hier.part::hier.part(datos2[,1], datos2[,-1], barplot = FALSE)
CTT<-hie$I.perc
CTT<-data.frame(Variables=rownames(CTT), CTT)
CTT<-CTT[order(CTT$I, decreasing = TRUE), ]
barplot(CTT$I, names.arg=CTT$Variables, ylab="% Independent effects (%I)",main="Selected variables", font.lab=2, font.axis=2, cex.lab=1.2)
}

if(jpg==TRUE){
dev.off()
}
hjkl<-0
}



#Multivariate regression
begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[2,1]<-"Multivariate regression analysis...."
ZZ[3,1]<-begin.times
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)


zz<-dim(datos2)
fo<-paste(names(datos2)[1], "~", names(datos2)[2])
for(i in 3:zz[2]){
fo<-paste(fo, "+", names(datos2)[i])
}
reg<-lm(fo,data=datos2)

if(stepwise==TRUE){
reg<-step(reg)
}

Re1<-summary(reg)
res<-residuals(reg)

if(deplog==TRUE){
residuos<-data.frame(datos2[,paste("LN", varY, sep="")],res)
colnames(residuos)<-c(paste("LN", varY, sep=""),"Residuals")
}
else{
residuos<-data.frame(datos2[,varY],res)
colnames(residuos)<-c(varY,"Residuals")
}

#Normality
Lillie1<-nortest::lillie.test(res)

#Autocorrelation
DW1<-lmtest::dwtest(reg)

#Homocedasticity
BreuschPagan1<-lmtest::bptest(reg)


#Colinearity of model
VIFModel<-car::vif(reg)


if(PAIRS==TRUE){



if(jpg==TRUE) jpeg(filename = jpg1, width = 4000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600) else dev.new()

lent<-length(reg$coefficients)
selec<-names(reg$coefficients)[2:lent]
selec<-unlist(strsplit(selec, split=","))
selec<-subset(datos2, select=selec)


if(deplog==TRUE){
logth<-paste("LN", varY, sep="")
selec1<-data.frame(datos2[,logth],selec)
colnames(selec1)<-c(logth,colnames(selec))
}
else{
selec1<-data.frame(datos2[,varY],selec)
colnames(selec1)<-c(varY,colnames(selec))
}

if(!is.null(pairs)){
exe<-paste("pairs(","x=selec1,", toString(x=pairs), ")")
eval(parse(text=exe))
}
else{
exe<-paste("pairs(","x=selec1,",  "main=NULL,", "cex.main=2,", "lower.panel=panel.smooth,",
"upper.panel=panel.reg,", "diag.panel=panel.hist,", "cex.labels=1,", "font.labels=2,", 
"cex =1.2,","pch=24,", "bg='#B2DFEEFF',", "col.smooth='red',", "line.col='red'", ")")
eval(parse(text=exe))
}

if(jpg==TRUE) dev.off() else hhjk<-1

}

#Contribution of each variable
dimd<-dim(datos2)
Contr<-"No estimated"
if(CoVa==TRUE){
if(length(reg$coefficients)>3){

begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[1,1]<-"Relative contribution of each variable (be patient)...."
ZZ[2,1]<-begin.times
ZZ[3,1]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

imprel<-relaimpo::calc.relimp(reg,type=typeV,rela=rela)
boot <- relaimpo::boot.relimp(reg, b = b, type = typeV, rela = rela)
Contr<-relaimpo::booteval.relimp(boot)
if(jpg==TRUE) jpeg(filename = jpg2, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600) else dev.new()

plot(relaimpo::booteval.relimp(boot,sort=T), names.abbrev=names.abbrev, ylim=ylimV, main=mainV, cex.title=cex.title)

if(jpg==TRUE) dev.off() else hhjk<-1
}
}



#Support Vector Machine
R2<-""
varYY<-""
fo<-""
if(SVM==TRUE){

begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[1,1]<-"Support Vector Machine model (be very patient)...."
ZZ[2,1]<-begin.times
ZZ[3,1]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)



datos2<-datos
colnames(datos2)<-c(varY,varX)

if(deplog==TRUE){
YLN<-log(datos2[,varY]-(min(datos2[,varY])-1))
datos2<-data.frame(YLN, datos2[,varX])
colnames(datos2)<-c(paste("LN", varY, sep=""),varX)
}

if(indlog==TRUE){
datosLN<-datos2[, varX]
dimT<-dim(datos2[, varX])
for(i in 1:dimT[2]){
datosLN[,i]<-log(datos2[,varX[i]]-(min(datos2[,varX[i]])-1))
}
colLN<-colnames(datosLN)
lenLN<-length(colnames(datosLN))
for(i in 1:lenLN){
colLN[i]<-paste("LN", colLN[i], sep="")
}
colnames(datosLN)<-colLN
datos2<-data.frame(datos2,datosLN)
}

#datos<-na.exclude(datos2)

if(deplog==TRUE){
varte<-paste("LN", varY, sep="")
var<-datos[,varte]
varYY<-paste("LN",varY)
}
else{
var<-datos[,varY]
varYY<-varY
}

zz<-length(reg$coefficients)

fo<-names(reg$coefficients)[2]
for(i in 3:zz){
fo<-append(fo, names(reg$coefficients)[i])
}



if(!is.null(ksvm)){
exe<-paste("kernlab::ksvm(","var ~ .,", "data= datos[,fo],", toString(x=ksvm), ")")
modelo<-eval(parse(text=exe))
}
else{
exe<-paste("kernlab::ksvm(","var ~ .,", "data= datos[,fo],","prob.model=FALSE", ")")
modelo<-eval(parse(text=exe))
}


datos$svm <- kernlab::predict(modelo, datos[,-1]) 

datos$svm<- as.numeric(datos$svm)



if(deplog==TRUE){
pred<-cbind(datosT[,1],datosT[,2],datosT[,3]-(exp(datos$svm-(min(datos$svm)-1))))
pred1<-cbind(data[,1],data[,2],data[,5])
pred2<-cbind(datosT[,1],datosT[,2],(exp(datos$svm-(min(datos$svm)-1))))
}
else{
pred<-cbind(datosT[,1],datosT[,2],(datosT[,3]-datos$svm))
pred1<-cbind(data[,1],data[,2],data[,5])
pred2<-cbind(datosT[,1],datosT[,2],datos$svm)
}



row.names(pred) <- NULL
row.names(pred1) <- NULL
row.names(pred2) <- NULL



R2<-1-var(datos$svm-datosT[,3])/var(datosT[,3])

if(deplog==TRUE){
R2<-1-var(datos$svm-datos[,varte])/var(datos[,varte])
}

}


if(SVM==TRUE){
if(Map==TRUE){

#Residuals of the SVM model

begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[1,1]<-"Residuals of the SVM model...."
ZZ[2,1]<-begin.times
ZZ[3,1]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)




ff<-180/f
cc<-ff*2


matriz<-matrix(-9999, nrow=ff, ncol=cc)

col<-c(0,seq(from=-180+f, to=180, by=f))
row<-c(seq(from=90-f, to=-90, by=-f))
matriz<-as.data.frame(matriz)



names(matriz)<-NULL
matriz<-matriz
matriz1<-matriz
matriz2<-matriz

matriz[as.matrix(pred[,1:2])] <- pred[,3]
matriz1[as.matrix(pred1[,1:2])] <- pred1[,3]
matriz2[as.matrix(pred2[,1:2])] <- pred2[,3]

matriz<-cbind(row,matriz)
matriz<-matriz[-1,]
matriz<-rbind(col,matriz)
colnames(matriz)<-NULL

matriz1<-cbind(row,matriz1)
matriz1<-matriz1[-1,]
matriz1<-rbind(col,matriz1)
colnames(matriz1)<-NULL

matriz2<-cbind(row,matriz2)
matriz2<-matriz2[-1,]
matriz2<-rbind(col,matriz2)
colnames(matriz2)<-NULL


if(dec=="."){
write.csv(matriz,file = CSV1, row.names=FALSE)
write.csv(matriz1,file = CSV2, row.names=FALSE)
write.csv(matriz2,file = CSV3, row.names=FALSE)
matriz<-read.csv(CSV1,header=FALSE)
matriz1<-read.csv(CSV2,header=FALSE)
matriz2<-read.csv(CSV3,header=FALSE)
}
else{
write.csv2(matriz,file = CSV1, row.names=FALSE)
write.csv2(matriz1,file = CSV2, row.names=FALSE)
write.csv2(matriz2,file = CSV3, row.names=FALSE)
matriz<-read.csv2(CSV1,header=FALSE)
matriz1<-read.csv2(CSV2,header=FALSE)
matriz2<-read.csv2(CSV3,header=FALSE)
}



d<-length(Area)
AA<-Area[1]
if (AA=="World"){
datos1<-adworld[2:5,]
}
else{
datos1<-rbind(adworld1,adworld2)
}

datos1<-na.exclude(datos1)



for (qw in 1:3){

if(qw==1){
varscale<-matriz
color<-c("#FFFFFFFF", "#FF0080FF","#FF00FFFF","#00FFFFFF", "#64FF64FF","#C8FF00FF", "#FFFF00FF","#FFC800FF","#FF6400FF","#FF0000FF")
if (missing(colscale1)) colscale<-color else colscale<-colscale1
}

if(qw==2){
varscale<-matriz1

if(!is.null(end)){
datos1<-replace(matriz1, matriz1>=end, end)
datos1[1,]<-varscale[1,]
datos1[,1]<-varscale[,1]
varscale<-datos1
rm(datos1)
codlegend<-paste(">",end)
}

if (ini==-10000) ini<-0.6 else ini<-ini
color<-c("#FFFFFFFF", "#C8FFFFFF","#64FFFFFF","#00FFFFFF", "#64FF64FF","#C8FF00FF","#FFFF00FF","#FFC800FF","#FF6400FF","#FF0000FF")
if (missing(colscale2)) colscale<-color else colscale<-colscale2
}

if(qw==3){
varscale<-matriz2

if(!is.null(end)){
datos1<-replace(matriz2, matriz2>=end, end)
datos1[1,]<-varscale[1,]
datos1[,1]<-varscale[,1]
varscale<-datos1
rm(datos1)
codlegend<-paste(">",end)
}
if (ini==-10000) ini<-0.6 else ini<-ini
color<-c("#FFFFFFFF", "#C8FFFFFF","#64FFFFFF","#00FFFFFF", "#64FF64FF","#C8FF00FF","#FFFF00FF","#FFC800FF","#FF6400FF","#FF0000FF")
if (missing(colscale2)) colscale<-color else colscale<-colscale2
}



ZZ[1,1]<-begin.times
ZZ[2,1]<-"Printing plot"
if(qw==1) ZZ[2,2]<-main1 else hjjuy<-1
if(qw==2) ZZ[2,2]<-main2 else hjjuy<-1
if(qw==3) ZZ[2,2]<-main3 else hjjuy<-1
ZZ[3,1]<-""

write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)



if (missing(inc)) inc=0.005 else inc=inc
if (AA=="World"){
if (missing(minLat)) minLat<--90 else minLat<-minLat
if (missing(maxLat)) maxLat<-90 else maxLat<-maxLat
if (missing(minLon)) minLon<--180 else minLon<-minLon
if (missing(maxLon)) maxLon<-180 else maxLon<-maxLon
}
else{
if (missing(maxLon)){
if(max(datos1$Lon)<0) maxLon<-(max(datos1$Lon)-max(datos1$Lon)*inc) else maxLon<-(max(datos1$Lon)+max(datos1$Lon)*inc)
}
else {
maxLon<-maxLon
}
if (missing(minLon)){
if(min(datos1$Lon)<0) minLon<-(min(datos1$Lon)+min(datos1$Lon)*inc) else minLon<-(min(datos1$Lon)-min(datos1$Lon)*inc)
}
else {
minLon<-minLon
}

if (missing(maxLat)){
if(max(datos1$Lat)<0) maxLat<-(max(datos1$Lat)-max(datos1$Lat)*inc) else maxLat<-(max(datos1$Lat)+max(datos1$Lat)*inc)
}
else {
maxLat<-maxLat
}
if (missing(minLat)){
if(min(datos1$Lat)<0) minLat<-(min(datos1$Lat)+min(datos1$Lat)*inc) else minLat<-(min(datos1$Lat)-min(datos1$Lat)*inc)
}
else {
minLat<-minLat
}
}


if (missing(trans)) trans=c(1,1) else trans<-trans
if (missing(log)) log=c(0,0) else log<-log

Lon<-as.numeric(varscale[1,-1])
varLon<-as.numeric(varscale[1,-1])
a<-length(Lon)
for (i in 1:a){
if(i==1) varLon[i]<-((-180+Lon[i])/2) else varLon[i]<-((Lon[i-1]+Lon[i])/2)
}

Lat<-as.numeric(varscale[-1,1])
varLat<-as.numeric(varscale[-1,1])
a<-length(Lat)
for (i in 1:a){
if(i==1) varLat[i]<-((90+Lat[i])/2) else varLat[i]<-((Lat[i-1]+Lat[i])/2)
}
varLat<-(-varLat)


firstrow<-varscale[1,]

ajuste<-varscale[varscale[,1]<=maxLat&varscale[,1]>=minLat,]


if(firstrow==ajuste[1,]){
}
else{
ajuste<-rbind(firstrow,ajuste)
}

ajuste<-ajuste[,ajuste[1,]<=maxLon&ajuste[1,]>=minLon]



ajuste<-ajuste[-1,-1]




ajuste<-as.matrix(ajuste)

colnames(ajuste)<-NULL


if(trans[1]==0){
ajuste<-replace(ajuste, ajuste==-9999,NA)
ajuste<-ajuste/trans[2]
ajuste<-replace(ajuste, is.na(ajuste),-9999)
}
else{
ajuste<-replace(ajuste, ajuste==-9999,NA)
ajuste<-ajuste*trans[2]
ajuste<-replace(ajuste, is.na(ajuste),-9999)
}

if(log[1]==0){
ajuste<-ajuste
}
else{
ajuste<-replace(ajuste, ajuste==-9999,NA)
ajuste<-log(ajuste+log[2])
ajuste<-replace(ajuste, is.na(ajuste),-9999)
}


ajuste<- ajuste[nrow(ajuste):1,]



varscale<-varscale[-1,-1]

varscale<-as.matrix(varscale)



if(trans[1]==0){
varscale<-replace(varscale, varscale==-9999,NA)
varscale<-varscale/trans[2]
varscale<-replace(varscale, is.na(varscale),-9999)
}
else{
varscale<-replace(varscale, varscale==-9999,NA)
varscale<-varscale*trans[2]
varscale<-replace(varscale, is.na(varscale),-9999)
}

if(log[1]==0){
varscale<-varscale
}
else{
varscale<-replace(varscale, varscale==-9999,NA)
varscale<-log(varscale+log[2])
varscale<-replace(varscale, is.na(varscale),-9999)
}

varscale<- varscale[nrow(varscale):1,]
varscale<-t(varscale)



if (maxLon>=180) maxLon<-180 else maxLon<-maxLon
if (minLon<=-180) minLon<--180 else minLon<-minLon
if (maxLat>=90) maxLat<-90 else maxLat<-maxLat
if (minLat<=-90) minLat<--90 else minLat<-minLat

if (missing(Area)) Area="World" else Area=Area
if (missing(colbg)) colbg="transparent" else colbg=colbg
if (missing(colcon)) colcon="transparent" else colcon=colcon
if (missing(colf)) colf="black" else colf=colf
if (missing(colfexc)) colfexc="black" else colfexc=colfexc
if (missing(pro)) pro=TRUE else pro=pro
if (missing(ylab)) ylab="Latitude" else ylab=ylab
if (missing(xlab)) xlab="Longitude" else xlab=xlab
if (missing(main1)) main1="Residuals of the SVM model" else main1=main1
if (missing(main2)) main2="Actual richness" else main2=main2
if (missing(main3)) main3="Richness predicted by SVM model" else main3=main3

if (missing(cex.lab)) cex.lab=1.4 else cex.lab=cex.lab
if (missing(cex.axis)) cex.axis=1.2 else cex.axis=cex.axis
if (missing(lwdP)) lwdP=0.6 else lwdP=lwdP
if (missing(lwdC)) lwdC=0.1 else lwdC=lwdC
if (missing(family)) family="sans" else family=family
if (missing(font.main)) font.main=2 else font.main=font.main
if (missing(font.lab)) font.lab=1 else font.lab=font.lab
if (missing(font.axis)) font.axis=1 else font.axis=font.axis
if (missing(lab)) lab=NULL else lab=lab
if (missing(exclude)) exclude=NULL else exclude=exclude
if (missing(colexc)) colexc="white" else colexc=colexc
if (missing(varscale)) varscale=NULL else varscale=varscale

if (missing(breaks)) breaks=10 else breaks=breaks
if (missing(xl)) xl=0 else xl=xl
if (missing(xr)) xr=0 else xr=xr
if (missing(yb)) yb=0 else yb=yb
if (missing(yt)) yt=0 else yt=yt
if (missing(ndigits)) ndigits=0 else ndigits=ndigits



legend.max=max(ajuste)

if(legend.max<=10){
legend.min=(if(min(ajuste[!ajuste==-9999])==0) min(ajuste[!ajuste==-9999])+(max(ajuste)/(length(colscale)-1)) else min(ajuste[!ajuste==-9999]))
}
else{
legend.min=min(ajuste[!ajuste==-9999])
}

if(legend.min<0) legend.min<-legend.min+legend.min*0.1/100 else legend.min<-legend.min-legend.min*0.1/100
if(legend.max<0) legend.max<-legend.max-legend.max*0.1/100 else legend.max<-legend.max+legend.max*0.1/100


Lati<-(maxLat+minLat)/2
if (pro==TRUE) aspe=(1/cos(Lati*pi/180)) else aspe=1
if (missing(asp)) asp=aspe else asp=asp

x<-0
y<-0
rm(datos1)


if(qw==1) file=jpg3 else hjjuy<-1
if(qw==2) file=jpg4 else hjjuy<-1
if(qw==3) file=jpg5 else hjjuy<-1

if(jpg==TRUE) jpeg(filename = file, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600) else hhjhk<-1


########### function written by Greg Snow
squishplot <- function(xlim,ylim,asp=1){
   if(length(xlim) < 2) stop('xlim must be a vector of length 2')
   if(length(ylim) < 2) stop('ylim must be a vector of length 2')

  tmp <- par(c('plt','pin','xaxs','yaxs'))

  if( tmp$xaxs == 'i' ){ # not extended axis range

        xlim <- range(xlim)
  } else { # extended range

	tmp.r <- diff(range(xlim))
	xlim <- range(xlim) + c(-1,1)*0.04*tmp.r

  }

  if( tmp$yaxs == 'i' ){ # not extended axis range

        ylim <- range(ylim)
  } else { # extended range

	tmp.r <- diff(range(ylim))
	ylim <- range(ylim) + c(-1,1)*0.04*tmp.r

  }

  tmp2 <- (ylim[2]-ylim[1])/(xlim[2]-xlim[1])

  tmp.y <- tmp$pin[1] * tmp2 * asp

  if(tmp.y < tmp$pin[2]){ # squish vertically
	par(pin=c(tmp$pin[1], tmp.y))
	par(plt=c(tmp$plt[1:2], par('plt')[3:4]))
  } else { # squish horizontally
	tmp.x <- tmp$pin[2]/tmp2/asp
	par(pin=c(tmp.x, tmp$pin[2]))
	par(plt=c(par('plt')[1:2], tmp$plt[3:4]))

  }

  return(invisible(tmp['plt']))
} # end of function
###################

if (missing(ini)){
if(min(varscale[!varscale==-9999])==0) ini<-0 else ini<-legend.min
}
else{
ini<-ini
}

if (ini==-10000){
if(min(varscale[!varscale==-9999])==0) ini<-0 else ini<-legend.min
}
else{
ini<-ini
}


if (maxLon==180 & minLon==-180 & minLat==-90 & maxLat==90){
xl<-185
xr<-195
}


if(qw==1) main=main1 else hjjuy<-1
if(qw==2) main=main2 else hjjuy<-1
if(qw==3) main=main3 else hjjuy<-1



par(lwd=lwdP,fg="black",family=family)

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)

legend.freq1=abs((legend.max-ini)/(length(colscale)-1))

legend.freq=abs((legend.max-ini)/(breaks-1))

if (missing(legend.pos)){
if((maxLon-minLon)>260 & (maxLon-minLon)/(maxLat-minLat)>2.265) legend.pos="x" else legend.pos="y"
}

if(legend.pos=="x"){
if (missing(cex.main)) cex.main=1.3 else cex.main=cex.main
}

if(legend.pos=="y"){
if (missing(cex.main)) cex.main=1.6 else cex.main=cex.main
}


if (legend.pos=="y") par(oma=c(0,0,0,1)) else  par(oma=c(0,0,2,0))
image(varLon, varLat,varscale,xlim=c(minLon,maxLon),ylim=c(minLat,maxLat), axes=F, xaxs="i", yaxs="i", xlab="",ylab="", col=colscale, breaks=c(ini,seq(ini,legend.max,by=legend.freq1)))



par(new=T,lwd=lwdP)

plot(x,y,xlim=c(minLon,maxLon),ylim=c(minLat,maxLat),xlab=xlab, main="", axes=TRUE,
ylab = ylab, cex.lab=cex.lab, cex.axis= cex.axis,type="n",bty="l",
font.lab=font.lab, font.axis=font.axis,lab=lab,yaxs="i",xaxs="i",yaxt="n",xaxt="n")
mtext(text=main,side=3, line=0.3, cex=cex.main, font=font.main)

axis(side=1,xlim=c(minLon,maxLon),lwd=lwdP)
axis(side=2,ylim=c(minLat,maxLat),lwd=lwdP)

if (colbg=="#FFFFFF") rect(0, 0, 0, 0, col = colbg) else rect(minLon, minLat, maxLon, maxLat, col = colbg)

if (legend.pos=="y"){
if (xl==0){
x1<-(maxLon-minLon)*(-0.00106495)+0.747382095+maxLon
x2<-(maxLon-minLon)*(-0.003194851)+2.060146284+maxLon
}
else{
x1<-xl
x2<-xr
}

if(legend.max<=10){
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)

}
else{
if(ini==0){
legend.freq=abs((legend.max-ini)/(breaks-1))
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)

}
else{
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)

}

}

if(qw>1){ 
if(!is.null(end)){
lensequ<-length(sequ)
sequ[lensequ]<-codlegend
}
}

plotrix::color.legend(xl=x1, yb=minLat, xr= x2,
yt=maxLat, sequ, gradient="y", align="rb", cex=1.2, rect.col=colscale[-1])
}
else{
if (yb==0){
if(!is.null(main)){
y1<-maxLat+(maxLat-minLat)*(0.101851852)-1.333333333
y2<-maxLat+(maxLat-minLat)*(0.157407407)-1.333333333
}
else{
y1<-maxLat+(maxLat-minLat)*(0.027777778)
y2<-maxLat+(maxLat-minLat)*(0.083333333)
}
}
else{
y1<-yb
y2<-yt
}

if(legend.max<=10){
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}

if(qw>1){ 
if(!is.null(end)){
lensequ<-length(sequ)
sequ[lensequ]<-codlegend
}
}

plotrix::color.legend(xl=minLon, yb=y1, xr=maxLon, yt=y2, sequ,
gradient="x", align="lt", cex=1.2, rect.col=colscale[-1])
}

if (AA=="World") {
polygon(adworld$Lon,adworld$Lat,col=colcon, border=colf)
if(!is.null(exclude)){
polygon(adworld2$Lon,adworld2$Lat,col=colexc, border=colfexc)
}
}
else {
polygon(adworld1$Lon,adworld1$Lat,col=colcon, border=colf)
polygon(adworld2$Lon,adworld2$Lat,col=colexc, border=colfexc)
}

par(tmp)
ini<-(-10000)
if(jpg==TRUE) dev.off() else hhjk<-1


}

}
}


Resultados<-list("VIF FOR ALL VARIABLES", VIF1, 
"CONTRIBUTION OF VARIABLES ESTIMATED",
"BY USING HIERARCHICHAL PARTITIONING (I)",CT,
"VIF FOR SELECTED VARIABLES", VIF2,
"VIF OF MULTIPLE REGRESSION", VIFModel, "NORMALITY", Lillie1, "AUTOCORRELATION", DW1, "HOMOCEDASTICITY",
BreuschPagan1, "CONTRIBUTION OF THE VARIABLES IN MULTIPLE REGRESSION", Contr,"MULTIPLE REGRESSION", Re1,
"VARIABLES INCLUDED IN THE SUPPORT VECTOR MACHINE MODEL", "Dependent variable",varYY,
"Independent variables", fo, "PERCENTAGE OF VARIANCE EXPLAINED BY SUPPORT VECTOR MACHINE MODEL",R2)

print(Resultados)

#Output text file

if(TXT==TRUE){

begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[1,1]<-"Saving all results...."
ZZ[2,1]<-begin.times
ZZ[3,1]<-""
ZZ[4,1]<-"END"
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

if(!is.null(file1)){
sink(file1)
print(Resultados)
sink()
}

}

else{
ZZ[1,1]<-"END"
ZZ[2,1]<-""
ZZ[3,1]<-""
ZZ[4,1]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
}




if(dec=="."){
write.csv(x=residuos,file = CSV4, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = residuos,file = CSV4, fileEncoding = "", row.names=row.names,na=na)
}


}
