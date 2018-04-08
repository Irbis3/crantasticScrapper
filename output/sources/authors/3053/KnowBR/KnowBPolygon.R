KnowBPolygon<-function(data,  format="A", shape=NULL, shapenames=NULL, admAreas=FALSE, Area="World", curve= "Rational", estimator=1,
cutoff=1, cutoffCompleteness= 0, cutoffSlope= 1,  extent=TRUE, minLon, maxLon, minLat, maxLat, int=30,
colbg="#FFFFFF", colcon="#C8C8C8", colf="black", pro = TRUE, inc = 0.005, exclude = NULL,
colexc = NULL, colfexc="black", colscale=rev(heat.colors(100)), legend.pos="y",
breaks=10, xl=0, xr=0, yb=0, yt=0, asp, lab = NULL, xlab = "Longitude", ylab = "Latitude", main1="Records",
main2="Observed richness", main3="Completeness", main4="Slope", cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.2, cex.legend=0.9,
family = "sans", font.main = 2, font.lab = 1, font.axis = 1, lwdP=0.6, lwdC=0.1, trans=c(1,1),
 ndigits=0,  save="CSV", file1 = "Species per site", file2 = "Estimators",
 file3 = "Standard error of the estimators", na = "NA", dec = ",", row.names = FALSE, Maps=TRUE,
jpg=TRUE, jpg1="Records.jpg", jpg2="Observed richness.jpg",  jpg3="Completeness.jpg", jpg4="Slope.jpg",cex=1.5, pch=15,
cex.labels=1.5, pchcol="red", ask=FALSE){


if(is.null(shape)){
if(Area=="World" & xl==0){
xl=182
}
if(Area=="World" & xr==0){
xr=188
}
}

if(!missing(minLon)){
if(!missing(maxLon)){
if(minLon==-180 & maxLon==180){
xl=182
xr=188
}
}
}


method="accumulation"
SpR<-FALSE
largematrix<-TRUE

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

if(!is.null(shape) & is.null(shapenames)){
stop("It is necessary to specify in the argment 'shapenames' the variable with the names of the polygons in the shape")
}

####End Checking

values1<-data.frame(1,2,3,4,5,6,7,8,9,10,11)
values2<-data.frame(1,2,3,4,5,6,7,8)
values3<-data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13)
sevalues1<-data.frame(1,2,3,4,5,6,7)
sevalues2<-data.frame(1,2,3,4,5,6)
sevalues3<-data.frame(1,2,3,4,5,6,7,8)
salA<-data.frame(c(2,5,6,1,0,6,5,8,7,4,9,8), nrow=4)
sal<-data.frame(c(2,5,6,1,0,6,5,8,7,4,9,8), nrow=4)
cu2<-NA;cu3<-NA;cu4<-NA;cu5<-NA
sp2<-NA;sp3<-NA;sp4<-NA;sp5<-NA
serandom<-NA;seexact<-NA;secoleman<-NA;serarefaction<-NA

if(format=="B"){
data[is.na(data)]<-0
}


if(format=="A"){

ZZ<-matrix(rep("",8),nrow=4) 

x<-na.exclude(data)

if(format=="B"){
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


}#Big Matrix


else{
datosf<-with(dt1, table(dt1[,5],dt1[,1]))
datosac<-with(temp, table(temp[,5],temp[,1]))
}

sums<-aggregate(datosf[,4],by=list(datosf[,1],datosf[,2],datosf[,3]),sum,na.rm=TRUE)
names(sums)<-c("Species","Longitude","Latitude","Records")

if(save=="RData"){

file1<-paste(file1,".RData", sep="")
save(sums, file=file1)

}
else{

file1<-paste(file1,".CSV", sep="")

if(dec=="."){
write.csv(x=sums, file = file1, row.names=row.names,na=na, fileEncoding = "")
}
else{
write.csv2(x=sums, file = file1, row.names=row.names,na=na, fileEncoding = "")
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


}

rm(x)

}##End format
else{
tableR<-data
}

if(format=="A"){
tableR<-data
}

if(format=="B"){
tableR<-as.data.frame(data)
}



#File adworld to a list


ppp<-1
pppp<-1


if(is.null(shape)){
out<-split(adworld,f = adworld$Area)
out<-out[c(-1,-2)]
if(Area!="World" & exists("adworld1")==TRUE){
out<-split(adworld1,f = adworld1$Area)
}
Areas<-names(out)
numero<-length(Areas)
out<-lapply(out, function(x) x[!(names(x) %in% c("Area"))])
out<-as.matrix(out)

}###End bucle internal shapes
else{

if(class(shape)=="list"){
data<-shape[[1]]
lsh<-length(shape)
if(lsh>1){
ss<-seq(2,lsh)
hh<-as.character(shape[ss])
data<-eval(parse(text=paste("subset(data,",noquote(shapenames), " %in% hh)", sep="")))
}
}
else{
data<-shape
if(class(data)=="character"){
data<-eval(parse(text=paste(".GlobalEnv$", data, sep="")))
}

}


numero<-length(data)
Areas<-eval(parse(text=paste("data$",noquote(shapenames),sep="")))

}


cut<-cutoff

leng1<-numero
uio<-1
pvalor<-1
suma<-1
sumas<-1

ZZ<-matrix(rep("",8),nrow=4) 

rm(dt1)
rm(datosf)
rm(temp)
rm(species1)
rm(species)
rm(div)
rm(Records)
rm(datosac)
rm(datos2)
rm(values1)
rm(values2)
rm(values3)
rm(pp)
rm(sums)

for(z in 1:numero){

end.time<-Sys.time() 
end.times <- format(end.time, "%b %d, %Y at %X")
ZZ[1,1]<-end.times
ZZ[2,1]<-paste(z,"from", numero,"polygons")
ZZ[2,2]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

if(is.null(shape)){
pp<-as.data.frame(out[z])
}
else{
pp<-as.data.frame(data@polygons[[z]]@Polygons[[1]]@coords)
}


if(format=="B"){
log<-mgcv::in.out(as.matrix(pp),as.matrix(tableR[, c(1,2)]))
}

if(format=="A"){
log<-mgcv::in.out(as.matrix(pp),as.matrix(tableR[, c(2,3)]))
}


if(any(log==TRUE)==TRUE){

if(format=="B"){
mm<-cbind(tableR,log)
dhh<-dim(mm)
tableR<-subset(mm,(mm[,dhh[2]] == FALSE))
tableR<-tableR[,c(-dhh[2])]


mm<-mm[mm[,dhh[2]],]

mm<-mm[,c(-1,-2,-dhh[2])]
mm<-mm[, apply(mm, 2, sum)!=0]

if(class(mm)=="integer"){
dimtt<-length(mm)
}
else{
dimt<-dim(mm)
dimtt<-dimt[1]
}
}

if(format=="A"){

mm<-cbind(tableR,log)
dhh<-dim(mm)
tableR<-subset(mm,(mm[,dhh[2]] == FALSE))
tableR<-tableR[,c(-dhh[2])]

mm<-mm[mm[,dhh[2]],]

mm<-mm[,c(-dhh[2])]


sp<-as.character(unique(mm[,1]))
lsp<-length(sp)
dco<-dim(mm)
Lm<-mm[,2:3]

for(as in 1:lsp){
col<-rep(0,dco[1])
r<-which(mm[,1]==sp[as])
col[r]<-1
Lm<-cbind(Lm,col)
}

names(Lm)<-c("Longitude","Latitude",sp)

mm<-Lm[,c(-1,-2)]

if(class(mm)=="integer"){
dimtt<-length(mm)
}
else{
dimt<-dim(mm)
dimtt<-dimt[1]
}


}








#####Method accumulation

R2random<-NA
R2exact<-NA



if(method=="accumulation"){

if(estimator==0 | estimator==2){

if(is.null(dim(mm))){
cu2<-NA
serandom<-NA
}
else{

cu<- vegan::specaccum(mm, method="random", permutations = 200)
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
}#NULL mm
}#estimator 0 and 2


if(estimator==0 | estimator==1){


if(is.null(dim(mm))){
cu3<-NA
seexact<-NA
}
else{
cu<- vegan::specaccum(mm, method="exact")
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
}#End NULL mm
}#End estimator 0 and 1


if(estimator==0){
Methods<-c(cu3,cu2)
Methods[Methods < 0] <- NA
seMethods<-c(seexact, serandom)
seMethods[seMethods < 0] <- NA
Methodssp<-c(sp3, sp2)
Methodssp[Methodssp < 0] <- NA
slope<-mean(Methodssp, na.rm=T)
pred<-mean(Methods, na.rm=T)
}



if(estimator==1){
Methods<-c(cu3)
Methods[Methods < 0] <- NA
seMethods<-c(seexact)
seMethods[seMethods < 0] <- NA
Methodssp<-c(sp3)
Methodssp[Methodssp < 0] <- NA
slope<-Methodssp
pred<-Methods
}

if(estimator==2){
Methods<-c(cu2)
Methods[Methods < 0] <- NA
seMethods<-c(serandom)
seMethods[seMethods < 0] <- NA
Methodssp<-c(sp2)
Methodssp[Methodssp < 0] <- NA
slope<-Methodssp
pred<-Methods
}

dimy<-dim(mm)

com<-(dimy[2]*100/pred)

pred<-round(pred)

records<-dimy[1]

residuals<-dimy[2]-pred


if(!is.null(dimy[2]) & !is.null(records) & estimator==0 & length(mm)>1){
if((records/dimy[2])>cut){

if(!is.na(slope) & slope>cutoffSlope){
com<-NA
}
if(!is.na(com) & com<cutoffCompleteness){
com<-NA
}

if(is.na(cu3)){
sp3<-NA
}

if(is.na(cu2)){
sp2<-NA
}

if(is.na(sp3) | is.na(sp2)){
slope<-NA
}



ratio<-records/dimy[2]
estimators<-data.frame(Areas[z],records, dimy[2], cu3,cu2,sp3,sp2,slope,com, ratio)
seestimators<-data.frame(Areas[z],records, dimy[2],seexact, serandom,R2exact,R2random)
}
else{
ratio<-records/dimy[2]
estimators<-data.frame(Areas[z],records, dimy[2], NA,NA,NA,NA,NA,NA,ratio)
seestimators<-data.frame(Areas[z],records, dimy[2],NA, NA,NA,NA)
}
colnames(estimators)<-c("Area","Records","Observed.richness", "Richness.exact", "Richness.random","Slope.exact", "Slope.random", "Mean.slope", "Completeness", "Ratio")
colnames(seestimators)<-c("Area","Records","Observed.richness","SE.exact", "SE.random","R2.exact","R2.random")
if(pppp==1){
finales<-estimators
finalsees<-seestimators
pppp<-2
}
else{
finales<-rbind(finales,estimators)
finalsees<-rbind(finalsees,seestimators)
}
}



if(!is.null(dimy[2]) & !is.null(records) & estimator==1 & length(mm)>1){

if(records/dimy[2]>cut){

if(!is.na(sp3) & sp3>cutoffSlope){
com<-NA
}
if(!is.na(com) & com<cutoffCompleteness){
com<-NA
}

if(is.na(cu3)){
sp3<-NA
}

ratio<-records/dimy[2]
estimators<-data.frame(Areas[z],records, dimy[2], cu3,sp3,com,ratio)
seestimators<-data.frame(Areas[z],records, dimy[2], seexact,R2exact)
}
else{
ratio<-records/dimy[2]
estimators<-data.frame(Areas[z],records, dimy[2], NA,NA,NA,ratio)
seestimators<-data.frame(Areas[z],records, dimy[2],NA,NA)
}
colnames(estimators)<-c("Area","Records","Observed.richness", "Richness","Slope", "Completeness","Ratio")
colnames(seestimators)<-c("Area","Records","Observed.richness","SE","R2")
if(pppp==1){
finales<-estimators
finalsees<-seestimators
pppp<-2
}
else{
finales<-rbind(finales,estimators)
finalsees<-rbind(finalsees,seestimators)
}
}

if(!is.null(dimy[2]) & !is.null(records) & estimator==2 & length(mm)>1){
if((records/dimy[2])>cut){

if(!is.na(sp2) & sp2>cutoffSlope){
com<-NA
}
if(!is.na(com) & com<cutoffCompleteness){
com<-NA
}

if(is.na(cu2)){
sp2<-NA
}

ratio<-records/dimy[2]
estimators<-data.frame(Areas[z],records, dimy[2], cu2,sp2,com,ratio)
seestimators<-data.frame(Areas[z],records, dimy[2], serandom,R2random)
}
else{
ratio<-records/dimy[2]
estimators<-data.frame(Areas[z],records, dimy[2], NA,NA,NA,ratio)
seestimators<-data.frame(Areas[z],records, dimy[2],NA,NA)
}
colnames(estimators)<-c("Area","Records","Observed.richness", "Richness","Slope", "Completeness","Ratio")
colnames(seestimators)<-c("Area","Records","Observed.richness","SE","R2")
if(pppp==1){
finales<-estimators
finalsees<-seestimators
pppp<-2
}
else{
finales<-rbind(finales,estimators)
finalsees<-rbind(finalsees,seestimators)
}
}

}#End method accumulation

}#End length log

rm(log)
rm(cu)

}###End for z


####Save files

end.time<-Sys.time() 
end.times <- format(end.time, "%b %d, %Y at %X")
ZZ[1,1]<-end.times
ZZ[2,1]<-"Saving files...."
ZZ[2,2]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

if(exists("finales")==FALSE){
stop("There are no records in any of the shapes")
}
else{
estimators<-finales
seestimators<-finalsees


if(save=="RData"){

file2<-paste(file2,".RData", sep="")
file3<-paste(file3,".RData", sep="")

save(estimators, file=file2)
save(seestimators, file=file3)
}
else{

file2<-paste(file2,".CSV", sep="")
file3<-paste(file3,".CSV", sep="")


if(dec=="."){
write.csv(x=estimators, file = file2, row.names=row.names,na=na, fileEncoding = "")
write.csv(x=seestimators, file = file3, row.names=row.names,na=na, fileEncoding = "")
}
else{
write.csv2(x=estimators, file = file2, row.names=row.names,na=na, fileEncoding = "")
write.csv2(x=seestimators, file = file3, row.names=row.names,na=na, fileEncoding = "")
}


}

}


if(Maps==TRUE){
#Map
temporal<-estimators

datos<-data

estimators<-na.exclude(estimators)
AreasS<-as.character(unique(estimators[,1]))

if(length(AreasS)<=1){
stop("Maps are not depicted because there is only one polygon with information about completeness (see the file Estimators)")
}


if(is.null(shape)){
data<-subset(adworld, adworld[,3]==AreasS)
}
else{
data<-eval(parse(text=paste("subset(data,",noquote(shapenames), " %in% AreasS)", sep="")))
}


if(method=="accumulation") graphics<-4 else graphics<-3

if(admAreas==TRUE | is.null(shape)){
d<-length(Area)
AA<-Area[1]
if (AA=="World"){
datos1<-adworld[2:5,]
}
else{
datos1<-rbind(adworld1,adworld2)
}
datos1<-na.exclude(datos1)
}


dimes<-dim(estimators)



for(gg in 1:graphics){

if(gg==1){
filejpg<-jpg1
main<-main1
tt<-2
AreasS<-as.character(unique(temporal[,1]))
if(is.null(shape)){
data<-subset(adworld, adworld[,3]==AreasS)
}
else{
data<-eval(parse(text=paste("subset(datos,",noquote(shapenames), " %in% AreasS)", sep="")))
}


var<-temporal[,tt]

ZZ[1,1]<-end.times
ZZ[2,1]<-"Printing plot"
ZZ[2,2]<-main1
}

if(gg==2){
filejpg<-jpg2
main<-main2
tt<-3

AreasS<-as.character(unique(temporal[,1]))
if(is.null(shape)){
data<-subset(adworld, adworld[,3]==AreasS)
}
else{
data<-eval(parse(text=paste("subset(datos,",noquote(shapenames), " %in% AreasS)", sep="")))
}

var<-temporal[,tt]

ZZ[1,1]<-end.times
ZZ[2,1]<-"Printing plot"
ZZ[2,2]<-main2
}

if(gg==3){
filejpg<-jpg3
main<-main3
tt<-dimes[2]-2
AreasS<-as.character(unique(estimators[,1]))
if(!is.null(shape)){
data<-eval(parse(text=paste("subset(datos,",noquote(shapenames), " %in% AreasS)", sep="")))
}
var<-estimators[,"Completeness"]

ZZ[1,1]<-end.times
ZZ[2,1]<-"Printing plot"
ZZ[2,2]<-main3
}

if(gg==4){
filejpg<-jpg4
main<-main4
tt<-dimes[2]-3
AreasS<-as.character(unique(estimators[,1]))
if(estimator==1 | estimator==2) var<-estimators[,"Slope"] else var<-estimators[,"Mean.slope"]

ZZ[1,1]<-end.times
ZZ[2,1]<-"Printing plot"
ZZ[2,2]<-main4
colscale<-rev(colscale)
ndigits=ndigits+2
}


write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)


if(!is.null(shape)){
if(class(var)=="character"){
variable<-datos@data[,var]
}
else{
variable<-var
}

if(class(variable)=="factor"){
variable<-as.numeric(levels(variable))[variable]
}
}
else{
variable<-as.numeric(var)
}

if(jpg==TRUE){
jpeg(filename = filejpg, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}

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



# Palette

if(!is.null(shape)){
colors<- colorRampPalette(colscale)(int)

# Attribute on shade to each shape
if(class(variable)=="factor"){
class<-cut(levels(variable)[variable], int)
}
else{
class<-cut(as.numeric(variable), int)
}

colors<-colors[class]
}


# Make the plot

if (missing(inc)) inc=0.005 else inc=inc


if(!is.null(shape)){
if (missing(maxLon)){
if(datos@bbox[1,2]<0) maxLon<-(datos@bbox[1,2]-datos@bbox[1,2]*inc) else maxLon<-(datos@bbox[1,2]+datos@bbox[1,2]*inc)
}
else {
maxLon<-maxLon
}
if (missing(minLon)){
if(datos@bbox[1,1]<0) minLon<-(datos@bbox[1,1]+datos@bbox[1,1]*inc) else minLon<-(datos@bbox[1,1]-datos@bbox[1,1]*inc)
}
else {
minLon<-minLon
}

if (missing(maxLat)){
if(datos@bbox[2,2]<0) maxLat<-(datos@bbox[2,2]-datos@bbox[2,2]*inc) else maxLat<-(datos@bbox[2,2]+datos@bbox[2,2]*inc)
}
else {
maxLat<-maxLat
}
if (missing(minLat)){
if(datos@bbox[2,1]<0) minLat<-(datos@bbox[2,1]+datos@bbox[2,1]*inc) else minLat<-(datos@bbox[2,1]-datos@bbox[2,1]*inc)
}
else {
minLat<-minLat
}
}
else{

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

}


legend.max=max(variable)

legend.min=min(variable)


if(legend.min<0) legend.min<-legend.min+legend.min*0.1/100 else legend.min<-legend.min-legend.min*0.1/100
if(legend.max<0) legend.max<-legend.max-legend.max*0.1/100 else legend.max<-legend.max+legend.max*0.1/100


Lati<-(maxLat+minLat)/2
if (pro==TRUE) aspe=(1/cos(Lati*pi/180)) else aspe=1
if (missing(asp)) asp=aspe else asp=asp

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)

#Legend position

if(min(variable)==0) ini<-(-0.00001) else ini<-legend.min


legend.freq1=abs((legend.max-ini)/(length(colscale)-1))
legend.freq=abs((legend.max-ini)/(breaks-1))


if(missing(legend.pos)){
if((maxLon-minLon)>260 & (maxLon-minLon)/(maxLat-minLat)>2.265) legend.pos="x" else legend.pos=legend.pos
}
if (legend.pos=="y") par(oma=c(0,0,0,1)) else  par(oma=c(0,0,2,0))


#Map

plot(0,0,xlim=c(minLon,maxLon),ylim=c(minLat,maxLat),xlab=xlab, main="", axes=TRUE, pty="s",
ylab = ylab, cex.lab=cex.lab, cex.main=cex.main, cex.axis= cex.axis,type="n",bty="o", font.lab=font.lab, font.axis=font.axis,lab=lab,yaxs="i",xaxs="i",yaxt="n",xaxt="n")
mtext(text=main,side=3, line=0.3, cex=cex.main, font=font.main)
axis(side=1,xlim=c(minLon,maxLon),lwd=lwdP, cex.axis=cex.axis)
axis(side=2,ylim=c(minLat,maxLat),lwd=lwdP, cex.axis=cex.axis)

if(colbg=="#FFFFFF") rect(0, 0, 0, 0, col = colbg) else rect(minLon, minLat, maxLon, maxLat, col = colbg)

if(admAreas==TRUE){
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
}


if(!is.null(shape)){
sp::plot(data, col=colors, xlim=c(minLon,maxLon),ylim=c(minLat,maxLat), add=TRUE, bg="transparent")
}
else{
leng<-length(variable)
rbPal <- colorRampPalette(colscale)
colors<- rbPal(100)[as.numeric(cut(as.numeric(variable),breaks = 100))]
for(kk in 1:leng){
if(Area=="World") dataP<-subset(adworld, adworld[,3]==AreasS[kk]) else dataP<-subset(adworld1, adworld1[,3]==AreasS[kk])
polygon(dataP$Lon,dataP$Lat,col=colors[kk], border=colf)
}
}

#Color legend


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

if(legend.min<0) legend.min<-legend.min+legend.min*0.1/100 else legend.min<-legend.min-legend.min*0.1/100
if(legend.max<0) legend.max<-legend.max-legend.max*0.1/100 else legend.max<-legend.max+legend.max*0.1/100



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
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}

plotrix::color.legend(xl=minLon, yb=y1, xr=maxLon, yt=y2, sequ,
gradient="x", align="lt", cex=cex.legend, rect.col=colscale[-1])
}


par(tmp)
if(jpg==TRUE){

dev.off()
}


}###End for graphics
}##End Maps

ZZ[1,1]<-"END"
ZZ[1,2]<-""
ZZ[2,1]<-""
ZZ[2,2]<-""
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
rm(tableR)
rm(out)
rm(dataP)
rm(datos)
rm(datos1)
rm(data)
}
