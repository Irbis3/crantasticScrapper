

VARSEDIM<-function(data, variables, group,method="overlap", stepwise=TRUE, VARSEDIG=TRUE,
minimum=TRUE, kernel="gaussian", cor=TRUE, file1="Overlap.csv",  file2="Coefficients.csv", file3="Predictions.csv", 
file4="Polar coordinates.csv", file="Resuts.txt", na="NA", dec=",", row.names=FALSE){

#Selection of variables
datosT<-data.frame(subset(data, select=group), subset(data, select=variables))
datos<-na.exclude(datosT)
groups<-as.character(unique(datos[,1]))
n<-length(groups)


ZZ<-matrix(c("","","","","","","",""), nrow=4)

###Running VARSEDIG among all taxa
for(zz in 1:n){
begin.time<-Sys.time() 
begin.times <- format(begin.time, "%b %d, %Y at %X") 
ZZ[2,1]<-paste(group," ", groups[zz]," (", zz, ")", " of ", n," taxa...", sep="")
ZZ[3,1]<-begin.times
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
datosT<-datos
datosT[,1]<-ifelse(datosT[,1]==groups[zz], yes=groups[zz], no="Others")

if(dec=="."){
write.csv(x=datosT,file = "Temp.csv", fileEncoding = "", row.names=row.names,na=na)
datosT<-read.csv(file="Temp.csv" ,header=TRUE)
}
else{
write.csv2(x = datosT,file = "Temp.csv", fileEncoding = "", row.names=row.names,na=na)
datosT<-read.csv2(file="Temp.csv" ,header=TRUE)
}

VARSEDIG(data=datosT, variables=variables, group=group, group1=groups[zz], group2="Others",
method=method, stepwise=stepwise, VARSEDIG=VARSEDIG,minimum=minimum, kernel=kernel, cor=cor,
file1=file1,  file2=file2, file3=file3, file4=file4, na=na, dec=dec, row.names=row.names)

data<-read.table("Output.txt",sep="\t")
write.table(x = data , file = file , append = TRUE , row.names = FALSE , col.names = FALSE)

}#En Loop

if(file.exists("Temp.csv")){
file.remove("Temp.csv")
}

}