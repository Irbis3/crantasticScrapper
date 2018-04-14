ContrMap<-function(data, vars, Species, Level=NULL, Taxon=NULL, jpg=FALSE){


####Checking the directory

list<-list.files(pattern=".ASC")

####Error if there are no ASC files in the folder
if(length(list)==0) {
stop("There are no ASC files in the folder")
}


####Selection of variables

if(!is.null(Level)){data<-subset(data,data[,Level] %in% Taxon)}

##########Function time to estimate remaining time
time<-function(t, from, to, c31="", c32="", c41="", c42=""){
ZZ<-matrix(rep("",8),nrow=4) 
ZZ[3,1]<-c31; ZZ[3,2]<-c32
ZZ[4,1]<-c41; ZZ[4,2]<-c42

end.time<-Sys.time() 
end.times<- format(end.time, "%b %d, %Y at %X")
run.time<-difftime(end.time,begin.time,units="secs")
run<-as.numeric(run.time)
run1<-(to-t)*run/(t-from)

if(!is.na(run1)){
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
if(minutes==Inf){
ZZ[1,1]<-end.times
ZZ[2,1]<-"It is not possible to estimate remaining time...." 
ZZ[2,2]<-""
}
else{
ZZ[1,1]<-end.times
ZZ[2,1]<-minutes
}
}
else{
ZZ[1,1]<-end.times
ZZ[2,1]<-"It is not possible to estimate remaining time...." 
ZZ[2,2]<-""
}
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)
}
##########End function función time



#Select the text before the "."

list<-sapply(strsplit(list, split='.', fixed=TRUE), function(x) (x[1]))

####Dimension of the raster

nombre<-paste(list[1], ".ASC" , sep = "")
r1<- raster::raster(nombre)

reso<-raster::res(r1)

t1<-raster::raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=reso)

dimm<-dim(t1)

####Building the rasters

n<-length(list)

h<-length(vars)

contar<-matrix(data = 0 , nrow = dimm[1] , ncol = dimm[2])
contar1<-array(data = 0 , dim=c(dimm[1] , dimm[2], h))

begin.time<-Sys.time()

for(z in 1:n){
pos<-which(data ==list[z], arr.ind = T)
d<-dim(pos)
if(d[1]>0){
time(t=z, from=1, to=n, c31=paste(z, "of", n), c41=list[z]) 
nombre<-paste(list[z], ".ASC" , sep = "")
r2<- raster::raster(nombre)
if(round(raster::xmin(r2))==-180 & round(raster::ymin(r2))==-90 & round(raster::xmax(r2))==180 & round(raster::ymax(r2))==90){
rr<-r2
}
else{
rr<-raster::merge(r2,t1)
}

m1<-raster::as.matrix(rr)
m1[is.na(m1)]<-0
m1[which(m1==(-9999))]<-0
contar<-contar+m1

for(j in 1:h){
valor<-data[pos[1,1],vars[j]]
if(valor!=0){
m1[which(m1==1)]<-valor
contar1[,,j]<-contar1[,,j]+m1
m1[which(m1==valor)]<-1
}
}
}
}

####Save the rasters
r<-raster::raster(nrow=dimm[1], ncol=dimm[2])

for(j in 1:h){

matrizfinal<-contar1[,,j]/contar
matrizfinal[is.na(matrizfinal)]<-(-9999)
matrizfinal[is.infinite(matrizfinal)]<-(-9999)

rr<-raster::setValues(r, matrizfinal)
name<-paste(vars[j],".ASC", sep="")
raster::writeRaster(rr, name,  NAflag= -9999, overwrite=TRUE) 

####Printing the maps

ZZ<-matrix(c("","","",""), nrow=2)

end.time<-Sys.time() 
end.times<- format(end.time, "%b %d, %Y at %X")
ZZ[1,1]<-end.times
ZZ[2,1]<-paste("Saving ASC file and printing map of the variable ", vars[j], sep="")
write.table(ZZ,"Inf.txt", row.names=FALSE,col.names=FALSE)

jpeg<-paste(vars[j],".jpg",sep="")

long<-seq(from=(-180+360/dimm[2]), to = 180 , by = 360/dimm[2])
matrizfinal<-rbind(long,matrizfinal)

lat<-seq(from=(90-180/dimm[1]), to = -90 , by = -180/dimm[1])
lat<-c(0,lat)
matrizfinal<-cbind(lat,matrizfinal)

Rmap(data=matrizfinal, jpg=jpg, filejpg=jpeg, main=vars[j])

}

}
