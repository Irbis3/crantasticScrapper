KnowB<-function(data, format="A", cell=60,  curve= "Rational", estimator=1,
cutoff=1, cutoffCompleteness= 0, cutoffSlope= 1, largematrix=FALSE,
 Area="World", extent=TRUE, minLon, maxLon, minLat, maxLat,
colbg="transparent", colcon="transparent", colf="black", pro = TRUE, inc = 0.005, exclude = NULL,
colexc = NULL, colfexc="black", colscale=rev(heat.colors(100)), legend.pos="y",
breaks=10, xl=0, xr=0, yb=0, yt=0, asp, lab = NULL, xlab = "Longitude", ylab = "Latitude",
main1="Observed richness", main2="Records",
main3="Completeness", main4="Slope", cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.2, cex.legend=1.2, family = "sans", font.main = 2, font.lab = 1, font.axis = 1, lwdP=0.6, lwdC=0.1, trans=c(1,1),
log=c(0,0), ndigits=0,   save="CSV", file1 = "Observed richness",
 file2 = "List of species", file3 = "Species per site", file4 = "Estimators", file5 = "Species per record",
file6 = "Records", file7 = "Completeness", file8 = "Slope", file9 = "Standard error of the estimators",
na = "NA", dec = ",", row.names = FALSE, jpg=TRUE, jpg1="Observed richness.jpg",
jpg2="Records.jpg",  jpg3="Completeness.jpg", jpg4="Slope.jpg",cex=1.5, pch=15,
cex.labels=1.5, pchcol="red", ask=FALSE){




method<-"accumulation"

SpR<-FALSE



options(warn=-1)

if(jpg==FALSE) par(ask=ask) else yuret<-1


#####Checking data required
if(exists("adworld")==FALSE){
adworld<-1
stop("It is necessary to load data(adworld)")
}

if(Area!="World" & exists("adworld1")==FALSE){
stop("It is necessary to use RWizard and replace data(adworld) by @_Build_AdWorld_, for using administative areas")
}

if(!is.null(exclude)){
stop("It is necessary to use RWizard and replace data(adworld) by @_Build_AdWorld_, for using administative areas")
}

if(exists("adworld1")==FALSE){
adworld1<-1
}

if(exists("adworld2")==FALSE){
adworld2<-1
}


####End Checking

if(format=="B"){
data[is.na(data)]<-0
}



if(format=="A"){
data[is.na(data)]<-0
}
x<-na.exclude(data)

if(format=="B"){
format<-"A"
xLo<-x[,1]
xLa<-x[,2]
dimg<-dim(x[,c(-1,-2)])
replicas<-rep(dimg[1], dimg[2])
x2<-matrix(as.matrix(x[,c(-1,-2)]), ncol=1)
headers<-names(x[,c(-1,-2)])
sps<-rep(x=headers,times=replicas)
xLo<-rep(x=xLo,times=dimg[2])
xLa<-rep(x=xLa,times=dimg[2])
x<-data.frame(sps,xLo,xLa,x2)
names(x)<-c("Species","Longitude","Latitude", "Counts")
x<-x[x$Counts>0,]
x<-x[(x$Longitude!=0 & x$Latitude!=0),]
}


if(format=="A"){
if(method=="accumulation"){
b<-x[,4]
x<-x[rep(1:nrow(x[,1:3]), b), ] 
x[,4]<-1
}
}


values1<-data.frame(1,2,3,4,5,6,7,8,9,10)
values2<-data.frame(1,2,3,4,5,6,7)

if(estimator==0){
values3<-data.frame(1,2,3,4,5,6,7,8,9,10,11)
sevalues3<-data.frame(1,2,3,4,5,6,7,8)
}
else{
values3<-data.frame(1,2,3,4,5,6,7,8)
sevalues3<-data.frame(1,2,3,4,5,6)
}

sevalues1<-data.frame(1,2,3,4,5,6,7)
sevalues2<-data.frame(1,2,3,4,5,6)

salA<-data.frame(c(2,5,6,1,0,6,5,8,7,4,9,8), nrow=4)
sal<-data.frame(c(2,5,6,1,0,6,5,8,7,4,9,8), nrow=4)
cu2<-NA;cu3<-NA;cu4<-NA;cu5<-NA
sp2<-NA;sp3<-NA;sp4<-NA;sp5<-NA
serandom<-NA;seexact<-NA;secoleman<-NA;serarefaction<-NA;R2exact<-NA;R2random<-NA


if(format=="A"){
re<-dim(x)

Records<-seq(1,re[1],1)

temp<-cbind(x,Records)
div<-x[,2]*cos(180*3.1416/180)+x[,3]*sin(90*3.1416/180)+(x[,2]+x[,3])*x[,2]+(x[,2]-x[,3])*x[,3]+
(x[,2]+x[,3])*x[,3]+(x[,2]-x[,3])*x[,2]+(x[,2]-x[,3])*x[,2]*x[,3]+(x[,2]+x[,3])*x[,2]*x[,3]
dt1<-cbind(x,div)
dt1<-dt1[order(dt1[,5]), ]




pp1<-subset(temp, !duplicated(temp[,1]))
dimtemp<-dim(temp)
dimpp1<-dim(pp1)

elements<-dimtemp[1]*dimpp1[1]
rm(pp1)

elements[is.na(elements)]<-0

if(elements>2000000000) elements<-0 else elements<-elements

if(SpR==FALSE) elements<-0 else elements<-elements

if(elements==0){

datosac<-x
datosf<-x


if(largematrix==FALSE){
}
else{
pp1<-subset(temp[,c(1,2,3)], !duplicated(temp[,1]))

pp1<-pp1[order(pp1[,1]), ]

pp2<-t(pp1)

coluSp<-dim(pp2)



datos3<-aggregate(x[,4],by=list(x[,1]),mean)
datos3<-datos3[order(datos3[,1]), ]
d3<-dim(datos3)

for (t in 1:d3[1]){
sele<-subset(temp,temp[,1] %in% datos3[t,1])
dimsele<-dim(sele)
matr1<-matrix(0, nrow=dimsele[1], ncol=coluSp[2]+2)
matr1[,1]<-sele[,2]
matr1[,2]<-sele[,3]
matr1[,t+2]<-1
if(t==1){
colnames(matr1)<-c("Longitude","Latitude",pp2[1,])
write.table(matr1,"Species per record.txt", row.names=FALSE)
}
else{
write.table(matr1,"Species per record.txt", row.names=FALSE, col.names=FALSE, append=TRUE)
}
}
rm(datos3)
}




}#Big Matrix


else{
datosf<-with(dt1, table(dt1[,5],dt1[,1]))
datosac<-with(temp, table(temp[,5],temp[,1]))
}

if(elements==0){
}
else{



if(save=="RData"){

file3<-paste(file3,".RData", sep="")
file5<-paste(file5,".RData", sep="")

save(datosf, file=file3)
save(datosac, file=file5)

load(file3)
load(file5)
}

else{

file3<-paste(file3,".CSV", sep="")
file5<-paste(file5,".CSV", sep="")

if(dec=="."){
write.csv(x=datosf, file = file3, row.names=row.names,na=na)
write.csv(x=datosac, file = file3, row.names=row.names,na=na)
}
else{
write.csv2(x=datosf, file = file3, row.names=row.names,na=na)
write.csv2(x=datosac, file = file5, row.names=row.names,na=na)
}

if(dec=="."){
datosf<-read.csv(file3, header=T, check.names=FALSE)
datosac<-read.csv(file5, header=T, check.names=FALSE)
}
else{
datosf<-read.csv2(file3, header=T, check.names=FALSE)
datosac<-read.csv2(file5, header=T, check.names=FALSE)
}

}




}


if(elements==0){
datosf<-x
datosac<-x
}
else{
datosac<-cbind(temp[,2:3],datosac)
datos2<-dt1[,2:3]
datos2<-subset(datos2, !duplicated(datos2)) 
datosf<-cbind(datos2,datosf)
datosf<-datosf[order(datosf[,1], datosf[,2]), ]
}





datosf[is.na(datosf)]<-0



species<-aggregate(x[,4],by=list(x[,1]),FUN=sum,na.rm=TRUE)
species1<-aggregate(x[,4],by=list(x[,1]),FUN=mean,na.rm=TRUE)
species<-cbind(species,species1[,2])
colnames(species)<-c("Species","Sum", "Mean")



}
else{
datosf<-x
datosac<-replace(x[,-c(1,2)], x[,-c(1,2)]>1,1)
datosac<-cbind(x[,c(1,2)],datosac)

Sum<-colSums(x[,-c(1,2)])
Mean<-colMeans(x[,-c(1,2)])
species<-rbind(Sum,Mean)
colnames(species)<-colnames(x[,-c(1,2)])
species<-cbind(c("Sum","Mean"),species)

}

if(format=="B") elements<-1 else elements<-elements

if(elements==0){
}
else{

if(save=="RData"){

file3<-paste(file3,".RData", sep="")
file5<-paste(file5,".RData", sep="")

save(datosf, file=file3)
save(datosac, file=file5)
}


else{

file3<-paste(file3,".CSV", sep="")
file5<-paste(file5,".CSV", sep="")

if(dec=="."){
write.csv(x=datosf, file = file3, row.names=row.names,na=na)
write.csv(x=datosac, file = file3, row.names=row.names,na=na)
}
else{
write.csv2(x=datosf, file = file3, row.names=row.names,na=na)
write.csv2(x=datosac, file = file5, row.names=row.names,na=na)
}

}


}

rm(datos2)



f<-cell/60
f<-round(f,digits=18)
ff<-180/f
cc<-ff*2
matriz<-matrix(-9999, nrow=ff, ncol=cc+1)
col<-c(0,seq(from=-180+f, to=180, by=f))
row<-c(seq(from=90-f, to=-90, by=-f))
matriz<-as.data.frame(matriz)
matriz[,1]<-row
matriz<-rbind(col,matriz)
names(matriz)<-NULL
a<-dim(matriz)


options(warn=-1)
matriz1<-matriz
matriz4<-matriz
matriz5<-matriz
matriz6<-matriz

if(format=="A"){
maxLat1<-ceiling(max(x[,3]))
minLat1<-floor(min(x[,3]))
maxLon1<-ceiling(max(x[,2]))
minLon1<-floor(min(x[,2]))
}
else
{
maxLat1<-ceiling(max(x[,2]))
minLat1<-floor(min(x[,2]))
maxLon1<-ceiling(max(x[,1]))
minLon1<-floor(min(x[,1]))
}

rm(x)

r1<-which(abs(row-maxLat1)==min(abs(row-maxLat1)))
r2<-which(abs(row-minLat1)==min(abs(row-minLat1)))
c1<-which(abs(col-minLon1)==min(abs(col-minLon1)))
c2<-which(abs(col-maxLon1)==min(abs(col-maxLon1)))
if(length(c1)==2) c1<-c1[2] else c1<-c1
if(length(c2)==2) c2<-c2[2] else c2<-c2
if(length(r1)==2) c1<-r1[2] else r1<-r1
if(length(r2)==2) r2<-r2[2] else r2<-r2


if(format=="A"){
if(method=="accumulation"){
datosf<-datosac
}
}
else{
if(method=="accumulation"){
datosa<-replace(datosf[,-c(1,2)], datosf[,-c(1,2)]>1,1)
datosf<-cbind(datosf[,c(1,2)],datosa)
}

}

rm(datosac)


ZZ<-matrix(c("","","",""), nrow=2)

begin.time<-Sys.time() 
leng1<-length(seq(r1-f, r2+f, by = f))
uio<-1
for (z in seq(r1-f, r2+f, by = f)){

if(uio<=1){
uio<-uio+1
}
else{
if(uio>=2){
end.time<-Sys.time() 
end.times <- format(end.time, "%b %d, %Y at %X")
run.time<-difftime(end.time,begin.time,units="secs")
run<-as.numeric(run.time)
run<-run/length(seq(r1-f,z, by=f))
run1<-run*(leng1-length(seq(r1-f,z, by=f)))
if(run1>=3600){
ZZ[2,2]<-"remaining hours...."
}
else{
if(run1<=60) ZZ[2,2]<-"remaining seconds...." else ZZ[2,2]<-"remaining minutes...."
}
if(run1>=3600){
minutes<-run1/3600
}
else{
if(run1<=60) minutes<-run1 else minutes<-run1/60 
}
minutes<-round(minutes, digits=1)
ZZ[1,1]<-end.times
ZZ[2,1]<-minutes
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
}
else{
}
}


for (h in seq(c1-f, c2+f, by = f)){

if(format=="A"){
if(elements==0){

datosx<-temp[(temp[,2]>=col[h-f])&(temp[,2]<col[h])&(temp[,3]>=row[z+f])&(temp[,3]<row[z]),]

dimx<-dim(datosx)

datosL<-datosx[,2:3]


if(dimx[1]==0) datosx<-datosx else datosx<-with(datosx, table(datosx[,5],datosx[,1]))


if(dimx[1]==0) datosx<-datosx else datosx<-datosx[, apply(datosx, 2, sum)!=0]#delete all columns with all values equal to zero 



}
else{
datosx<-datosf[(datosf[,1]>=col[h-f])&(datosf[,1]<col[h])&(datosf[,2]>=row[z+f])&(datosf[,2]<row[z]),]
datosx<-datosx[, apply(datosx, 2, sum)!=0]#delete all columns with all values equal to zero 
datosL<-datosx
}
}

else{
datosx<-datosf[(datosf[,1]>=col[h-f])&(datosf[,1]<col[h])&(datosf[,2]>=row[z+f])&(datosf[,2]<row[z]),]
datosx<-datosx[, apply(datosx, 2, sum)!=0]#delete all columns with all values equal to zero 
datosL<-datosx
}


if(elements==0) datosx<-datosx else datosx<-datosx[,-c(1,2)]


dimy<-dim(datosx)


dimy[is.null(dimy)] <- 0

if(dimy[1]==1){
cut<-sum(datosx, na.rm=TRUE)/dimy[2]
}
else{
cut<-dimy[1]/dimy[2]
}



if(dimy[1]==0){
hgh<-1
}
else{
if(dimy[2]==0) {
matriz1[z+2,h]<-dimy[2]
matriz4[z+2,h]<-dimy[1]
Lo1<-subset(datosL[,1], !duplicated(datosL[,1]))
La1<-subset(datosL[,2], !duplicated(datosL[,2]))
Longitude<-mean(Lo1)
Latitude<-mean(La1)
}
else{
if(dimy[2]==1){
matriz1[z+2,h]<-dimy[2]
matriz4[z+2,h]<-dimy[1]
Lo1<-subset(datosL[,1], !duplicated(datosL[,1]))
La1<-subset(datosL[,2], !duplicated(datosL[,2]))
Longitude<-mean(Lo1)
Latitude<-mean(La1)
}
else{

if(dimy[1]==0){
hgh<-1
}
else{
if(dimy[1]==1){



matriz1[z+2,h]<-dimy[2]
matriz4[z+2,h]<-dimy[1]
Lo1<-subset(datosL[,1], !duplicated(datosL[,1]))
La1<-subset(datosL[,2], !duplicated(datosL[,2]))
Longitude<-mean(Lo1)
Latitude<-mean(La1)
}
else{



if(method=="accumulation"){
if(cut<cutoff){
if(estimator==0){
Methods<-c(NA,NA)
Methodssp<-c(NA,NA)
sp2<-NA;sp3<-NA
seMethods<-c(NA,NA)
serandom<-NA;seexact<-NA
}
else{
Methods<-c(NA)
Methodssp<-c(NA)
sp2<-NA;sp3<-NA
seMethods<-c(NA)
serandom<-NA;seexact<-NA
}

}
else{


if(estimator==0 | estimator==2){
cu<- vegan::specaccum(datosx, method="random", permutations = 200)
datosc<-data.frame(cu$richness, cu$sites)

ymax<-max(datosc[,1],na.rm=T)
ymin<-min(datosc[,1],na.rm=T)
xmax<-max(datosc[,2],na.rm=T)
xmin<-min(datosc[,2],na.rm=T)

if(curve=="Clench"){
modelo<-try(nls(cu.richness ~ A*cu.sites/(1+B*cu.sites), data=datosc, start=list(A=1, B=0.01)), silent=T)
}

if(curve=="Exponential"){
modelo<-try(nls(cu.richness ~ (A)*(1-exp((-B*cu.sites))), data=datosc, start=list(A=ymax, B=0.01)), silent=T)
}

if(curve=="Saturation"){
modelo<-try(nls(cu.richness~A*(1-exp(-B*(cu.sites-C))), data=datosc, trace=T, start=list(A=ymax, B=0.01, C=0)), silent=TRUE)
}

if(curve=="Rational"){
modelo<-try(nls(cu.richness~(A+B*cu.sites)/(1+C*cu.sites), data=datosc, trace=T, start=list(A=1, B=1, C=0)), silent=TRUE)
}

res<-summary(modelo)
if(res[1]=="1"){
cu2<-NA
serandom<-NA
}
else{
cu2<-res$parameters[1,1]/res$parameters[2,1]
if(curve=="Saturation" | curve=="Exponential"){
cu2<-res$parameters[1,1]
}
if(curve=="Rational"){
cu2<-res$parameters[2,1]/res$parameters[3,1]
}

if(cu2<0){
cu2<-NA
sp2<-NA
serandom<-NA
}
else{
cu2<-cu2
leng<-length(cu$sites)
sp2<-cu$richness[leng]-cu$richness[leng-1]

if(curve=="Clench"){res1<-datosc[,1]-(res$coefficients[1,1]*datosc[,2])/(1+res$coefficients[2,1]*datosc[,2])}
if(curve=="Exponential"){res1<-datosc[,1]-(res$coefficients[1,1])*(1-exp((-res$coefficients[2,1]*datosc[,2])))}
if(curve=="Saturation"){res1<-datosc[,1]-(res$coefficients[1,1]*(1-exp(-res$coefficients[2,1]*(datosc[,2]-res$coefficients[3,1]))))}
if(curve=="Rational"){res1<-datosc[,1]-(res$coefficients[1,1]+res$coefficients[2,1]*datosc[,2])/(1+res$coefficients[3,1]*datosc[,2])}

serandom<-sqrt(sum((res1)^2)/length(res1))

R2random<-1-var(res1, na.rm=T)/var(datosc[,1], na.rm=T)
}
}
}

if(estimator==0 | estimator==1){
cu<- vegan::specaccum(datosx, method="exact")
datosc<-data.frame(cu$richness, cu$sites)

ymax<-max(datosc[,1],na.rm=T)
ymin<-min(datosc[,1],na.rm=T)
xmax<-max(datosc[,2],na.rm=T)
xmin<-min(datosc[,2],na.rm=T)

if(curve=="Clench"){
modelo<-try(nls(cu.richness ~ A*cu.sites/(1+B*cu.sites), data=datosc, start=list(A=1, B=0.01)), silent=T)
}

if(curve=="Exponential"){
modelo<-try(nls(cu.richness ~ (A)*(1-exp((-B*cu.sites))), data=datosc, start=list(A=ymax, B=0.01)), silent=T)
}

if(curve=="Saturation"){
modelo<-try(nls(cu.richness~A*(1-exp(-B*(cu.sites-C))), data=datosc, trace=T, start=list(A=ymax, B=0.01, C=0)), silent=TRUE)
}

if(curve=="Rational"){
modelo<-try(nls(cu.richness~(A+B*cu.sites)/(1+C*cu.sites), data=datosc, trace=T, start=list(A=1, B=1, C=0)), silent=TRUE)
}


res<-summary(modelo)
if(res[1]=="1"){
cu3<-NA
seexact<-NA
}
else{
cu3<-res$parameters[1,1]/res$parameters[2,1]
if(curve=="Saturation" | curve=="Exponential"){
cu3<-res$parameters[1,1]
}
if(curve=="Rational"){
cu3<-res$parameters[2,1]/res$parameters[3,1]
}
if(cu3<0){
cu3<-NA
sp3<-NA
seexact<-NA
}
else{
cu3<-cu3
leng<-length(cu$sites)
sp3<-cu$richness[leng]-cu$richness[leng-1]


if(curve=="Clench"){res1<-datosc[,1]-(res$coefficients[1,1]*datosc[,2])/(1+res$coefficients[2,1]*datosc[,2])}
if(curve=="Exponential"){res1<-datosc[,1]-(res$coefficients[1,1])*(1-exp((-res$coefficients[2,1]*datosc[,2])))}
if(curve=="Saturation"){res1<-datosc[,1]-(res$coefficients[1,1]*(1-exp(-res$coefficients[2,1]*(datosc[,2]-res$coefficients[3,1]))))}
if(curve=="Rational"){res1<-datosc[,1]-(res$coefficients[1,1]+res$coefficients[2,1]*datosc[,2])/(1+res$coefficients[3,1]*datosc[,2])}

seexact<-sqrt(sum((res1)^2)/length(res1))

R2exact<-1-var(res1, na.rm=T)/var(datosc[,1], na.rm=T)
}
}
}

if(estimator==0){
Methods<-c(cu3,cu2)
if(cut<cutoff) seMethods<-c(NA,NA) else seMethods<-c(seexact, serandom)
Methods[Methods < 0] <- NA
seMethods[seMethods < 0] <- NA
if(cut<cutoff) Methodssp<-c(NA,NA) else Methodssp<-c(sp3, sp2)
Methodssp[Methodssp < 0] <- NA
}


if(estimator==1){
Methods<-c(cu3)
if(cut<cutoff) seMethods<-c(NA) else seMethods<-c(seexact)
Methods[Methods < 0] <- NA
seMethods[seMethods < 0] <- NA
if(cut<cutoff) Methodssp<-c(NA) else Methodssp<-c(sp3)
Methodssp[Methodssp < 0] <- NA
}

if(estimator==2){
Methods<-c(cu2)
if(cut<cutoff) seMethods<-c(NA) else seMethods<-c(serandom)
Methods[Methods < 0] <- NA
seMethods[seMethods < 0] <- NA
if(cut<cutoff) Methodssp<-c(NA) else Methodssp<-c(sp2)
Methodssp[Methodssp < 0] <- NA
}

}
}



Methods[Methods < 0] <- NA



if(estimator==0){
pred<-mean(Methods, na.rm=T)
}
else{
pred<-mean(Methods[estimator], na.rm=T)
}


if(method=="accumulation"){

if (estimator==0){
slope<-mean(Methodssp, na.rm=T)
pred<-mean(Methods, na.rm=T)
}
else{
slope<-Methodssp
pred<-Methods
}

}

com<-(dimy[2]*100/pred)

Lo1<-subset(datosL[,1], !duplicated(datosL[,1]))
La1<-subset(datosL[,2], !duplicated(datosL[,2]))
Longitude<-mean(Lo1)
Latitude<-mean(La1)



if(method=="accumulation"){

if(estimator==0){


if(!is.na(slope) & slope>cutoffSlope){
com<-NA
}
if(!is.na(com) & com<cutoffCompleteness){
com<-NA
}
ratio<-dimy[1]/dimy[2]
temp4<-c(Longitude, Latitude, dimy[1],dimy[2],cu3,cu2,sp3,sp2,slope,com, ratio)
cu2<-NA;cu3<-NA
sp2<-NA;sp3<-NA
values3<-rbind(values3,temp4)

setemp4<-c(Longitude, Latitude, dimy[1],dimy[2],seexact,serandom,R2exact,R2random)
serandom<-NA;seexact<-NA; R2exact<-NA; R2random<-NA
sevalues3<-rbind(sevalues3,setemp4)
}

if(estimator==1){
if(!is.na(com) & com<cutoffCompleteness){
com<-NA
}
if(!is.na(slope) & slope>cutoffSlope){
com<-NA
}
ratio<-dimy[1]/dimy[2]
temp4<-c(Longitude, Latitude, dimy[1],dimy[2],cu3,sp3,com, ratio)
cu2<-NA;cu3<-NA
sp2<-NA;sp3<-NA
values3<-rbind(values3,temp4)

setemp4<-c(Longitude, Latitude, dimy[1],dimy[2],seexact,R2exact)
serandom<-NA;seexact<-NA;R2exact<-NA
sevalues3<-rbind(sevalues3,setemp4)
}

if(estimator==2){
if(!is.na(slope) & slope>cutoffSlope){
com<-NA
}
if(!is.na(com) & com<cutoffCompleteness){
com<-NA
}
ratio<-dimy[1]/dimy[2]
temp4<-c(Longitude, Latitude, dimy[1],dimy[2],cu2,sp2,com, ratio)
cu2<-NA;cu3<-NA
sp2<-NA;sp3<-NA
values3<-rbind(values3,temp4)

setemp4<-c(Longitude, Latitude, dimy[1],dimy[2],serandom,R2random)
serandom<-NA;seexact<-NA;R2random<-NA
sevalues3<-rbind(sevalues3,setemp4)
}





}


matriz1[z+2,h]<-dimy[2]
matriz4[z+2,h]<-dimy[1]


if(is.na(pred)) matriz5[z+2,h]<-(-9999) else matriz5[z+2,h]<-com

if(method=="accumulation"){
if(is.na(slope)) matriz6[z+2,h]<-(-9999) else matriz6[z+2,h]<-slope

if(is.na(pred)){
}
else{

if(slope>cutoffSlope){
matriz6[z+2,h]<-(-9999)
matriz5[z+2,h]<-(-9999)
}
}
}
else{
matriz6[z+2,h]<-(-9999)
}

}
}
}
}
}


}
}




if(method=="accumulation"){
values<-values3[-1,]
sevalues<-sevalues3[-1,]

if(estimator==0){
colnames(values)<-c("Longitude", "Latitude", "Records","Observed.richness", "Richness.exact", "Richness.random", "Slope.exact", "Slope.random", "Mean.slope", "Completeness", "Ratio")
colnames(sevalues)<-c("Longitude", "Latitude", "Records","Observed.richenss", "SE.exact", "SE.random","R2.exact","R2.random")
}

if(estimator==1){
colnames(values)<-c("Longitude", "Latitude", "Records","Observed.richness","Richness", "Slope","Completeness","Ratio")
colnames(sevalues)<-c("Longitude", "Latitude", "Records","Observed.richness","SE","R2")
}

if(estimator==2){
colnames(values)<-c("Longitude", "Latitude", "Records","Observed.richness","Richness", "Slope", "Completeness","Ratio")
colnames(sevalues)<-c("Longitude", "Latitude", "Records","Observed.richness","SE","R2")
}


}
else{
}


ZZ[1,1]<-end.times
ZZ[2,1]<-"Saving files...."
ZZ[2,2]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)


if(save=="RData"){

file1<-paste(file1,".RData", sep="")
file6<-paste(file6,".RData", sep="")
file7<-paste(file7,".RData", sep="")
file8<-paste(file8,".RData", sep="")
file2<-paste(file2,".RData", sep="")
file4<-paste(file4,".RData", sep="")
file9<-paste(file9,".RData", sep="")

save(matriz1, file=file1)
save(matriz4, file=file6)
save(matriz5, file=file7)
if(method=="accumulation"){
save(matriz6,file = file8)
}
save(species, file = file2)
save(values, file = file4)
save(sevalues, file = file9)

}
else{

file1<-paste(file1,".CSV", sep="")
file6<-paste(file6,".CSV", sep="")
file7<-paste(file7,".CSV", sep="")
file8<-paste(file8,".CSV", sep="")
file2<-paste(file2,".CSV", sep="")
file4<-paste(file4,".CSV", sep="")
file9<-paste(file9,".CSV", sep="")

if(dec=="."){
write.csv(x=matriz1, file = file1, row.names=row.names,na=na)
write.csv(x=species, file = file2, row.names=row.names,na=na)
write.csv(x=values, file = file4,  row.names=row.names,na=na)
write.csv(x=matriz4, file = file6,  row.names=row.names,na=na)
write.csv(x=matriz5, file = file7,  row.names=row.names,na=na)
write.csv(x=sevalues, file = file9,  row.names=row.names,na=na)
if(method=="accumulation"){
write.csv(x=matriz6, file = file8,  row.names=row.names,na=na)
}


}
else{
write.csv2(x=matriz1, file = file1, row.names=row.names,na=na)
write.csv2(x=species, file = file2,  row.names=row.names,na=na)
write.csv2(x=values, file = file4,  row.names=row.names,na=na)
write.csv2(x=matriz4, file = file6,  row.names=row.names,na=na)
write.csv2(x=matriz5, file = file7,  row.names=row.names,na=na)
write.csv2(x=sevalues, file = file9,  row.names=row.names,na=na)
if(method=="accumulation"){
write.csv2(x=matriz6, file = file8,  row.names=row.names,na=na)
}

}

}




if(method=="accumulation") rty<-4 else rty<-3

d<-length(Area)
AA<-Area[1]
if (AA=="World"){
datos1<-adworld[2:5,]
}
else{
datos1<-rbind(adworld1,adworld2)
}

datos1<-na.exclude(datos1)



for (qw in 1:rty){

if(qw==1){

if(save=="RData"){
load(file6)
varscale<-as.matrix(matriz4)
}
else{
if(dec=="."){
varscale<-read.csv(file6, header=F)
}
else{
varscale<-read.csv2(file6, header=F)
}
}


}

if(qw==2){

if(save=="RData"){
load(file1)
varscale<-as.matrix(matriz1)
}
else{
if(dec=="."){
varscale<-read.csv(file1, header=F)
}
else{
varscale<-read.csv2(file1, header=F)
}
}


matrizz1<-varscale

}



if(qw==3){
if(save=="RData"){
load(file7)
varscale<-as.matrix(matriz5)
}
else{
if(dec=="."){
varscale<-read.csv(file7, header=F)
}
else{
varscale<-read.csv2(file7, header=F)
}
}



}

if(qw==4){
if(save=="RData"){
load(file8)
varscale<-as.matrix(matriz6)
}
else{
if(dec=="."){
varscale<-read.csv(file8, header=F)
}
else{
varscale<-read.csv2(file8, header=F)
}
}


}

ZZ[1,1]<-end.times
ZZ[2,1]<-"Printing plot"
if(qw==1) ZZ[2,2]<-main2 else hjjuy<-1
if(qw==2) ZZ[2,2]<-main1 else hjjuy<-1
if(qw==3) ZZ[2,2]<-main3 else hjjuy<-1
if(qw==4) ZZ[2,2]<-main4 else hjjuy<-1

write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)



if (AA=="World"){
if (missing(minLat)) minLat<--90 else minLat<-minLat
if (missing(maxLat)) maxLat<-90 else maxLat<-maxLat
if (missing(minLon)) minLon<--180 else minLon<-minLon
if (missing(maxLon)) maxLon<-180 else maxLon<-maxLon
if(missing(extent)) extent<-TRUE else extent<-extent
if(extent==TRUE) minLat<-minLat1-f else minLat<-minLat
if(extent==TRUE) maxLat<-maxLat1+f else maxLat<-maxLat
if(extent==TRUE) minLon<-minLon1-f else minLon<-minLon
if(extent==TRUE) maxLon<-maxLon1+f else maxLon<-maxLon
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


ifelse(firstrow==ajuste[1,], jj<-1, ajuste<-rbind(firstrow,ajuste))


ajuste<-ajuste[,ajuste[1,]<=maxLon&ajuste[1,]>=minLon]



ajuste<-ajuste[-1,-1]

ajuste<-as.matrix(ajuste)



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

if (missing(varscale)) varscale=NULL else varscale=varscale

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


if(qw==1) file=jpg2 else hjjuy<-1
if(qw==2) file=jpg1 else hjjuy<-1
if(qw==3) file=jpg3 else hjjuy<-1
if(qw==4) file=jpg4 else hjjuy<-1

if(jpg==TRUE) jpeg(filename = file, width = 8000, height = 4000, units = "px", pointsize = 14, bg = "white", res = 600) else hhjhk<-1


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
} ####end of function



if(min(varscale[!varscale==-9999])==0) iniF<-(-0.00001) else iniF<-legend.min



if (maxLon==180 & minLon==-180 & minLat==-90 & maxLat==90){
xl<-185
xr<-195
}


if(qw==1) main=main2 else hjjuy<-1
if(qw==2) main=main1 else hjjuy<-1
if(qw==3) main=main3 else hjjuy<-1
if(qw==4){
main=main4
ndigits=ndigits+2
color7<-rev(colscale[-1])
colscale<-append(colscale[1],color7)
}


par(lwd=lwdP,fg="black",family=family)

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)

legend.freq1=abs((legend.max-iniF)/(length(colscale)-1))

legend.freq=abs((legend.max-iniF)/(breaks-1))

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
image(varLon, varLat,varscale,xlim=c(minLon,maxLon),ylim=c(minLat,maxLat), axes=F, xaxs="i", yaxs="i", xlab="",ylab="", col=colscale, breaks=c(iniF,seq(iniF,legend.max,by=legend.freq1)))

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
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)

}
else{
if(iniF==0){
legend.freq=abs((legend.max-iniF)/(breaks-1))
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)

}
else{
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)


}

}




plotrix::color.legend(xl=x1, yb=minLat, xr= x2,
yt=maxLat, sequ, gradient="y", align="rb", cex=cex.legend, rect.col=colscale[-1])
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
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{
sequ<-(seq(iniF,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}



plotrix::color.legend(xl=minLon, yb=y1, xr=maxLon, yt=y2, sequ,
gradient="x", align="lt", cex=cex.legend, rect.col=colscale[-1])
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

if(jpg==TRUE) dev.off() else hhjk<-1
}



ObservedRichness<-matriz1
Records<-matriz4
Completeness<-matriz5
Slope<-matriz6


if(save=="RData"){
save(ObservedRichness, file=file1)
save(Records, file=file6)
save(Completeness, file=file7)
if(method=="accumulation"){
save(Slope,file = file8)
}
}



ZZ[1,1]<-"END"
ZZ[1,2]<-""
ZZ[2,1]<-""
ZZ[2,2]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
rm(datosf)
rm(datosL)
rm(datosx)
rm(matriz)
rm(matriz1)
rm(matriz4)
rm(matriz5)
rm(matriz6)
rm(values)
rm(values1)
rm(values2)
rm(values3)
rm(varscale)
rm(temp)
rm(temp4)
}
