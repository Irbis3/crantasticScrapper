MapCell<-function(data, Area="World", minLon, maxLon, minLat, maxLat,
  colbg="#FFFFFF", colcon="#C8C8C8", colf="black",
  pro = TRUE,  inc = 0.005, exclude = NULL, colexc = NULL, colfexc="black",
  colscale=rev(heat.colors(100)),
  legend.pos="y", breaks=10, xl=0, xr=0, yb=0, yt=0, asp, lab = NULL,
  xlab = "Longitude", ylab = "Latitude", main=NULL, cex.main = 1.2,
  cex.lab = 1, cex.axis = 0.9, cex.legend=0.9,  family = "sans", font.main = 2,
  font.lab = 1, font.axis = 1, lwdP=0.6, lwdC=0.1, trans=c(1,1), log=c(0,0),
  ndigits=0, ini=NULL, end=NULL, jpg=FALSE, filejpg="Map.jpg"){

if(class(data)=="data.frame"){
data<-as.matrix(data)
}


if(class(data)=="RasterLayer"){

if(round(raster::xmin(data))==-180 & round(raster::ymin(data))==-90 & round(raster::xmax(data))==180 & round(raster::ymax(data))==90){
m1<-raster::as.matrix(data)
dimm<-dim(m1)
long<-seq(from=(-180+360/dimm[2]), to = 180 , by = 360/dimm[2])
m1<-rbind(long,m1)

lat<-seq(from=(90-180/dimm[1]), to = -90 , by = -180/dimm[1])
lat<-c(0,lat)
data<-cbind(lat,m1, deparse.level=0)
}
else{

reso<-raster::res(data)

r1<-raster::raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=reso)

data<-raster::resample(data,r1)

m1<-raster::as.matrix(data)
dimm<-dim(m1)

long<-seq(from=(raster::xmin(data)+(raster::xmax(data)-raster::xmin(data))/dimm[2]), to = raster::xmax(data) , by = (raster::xmax(data)-raster::xmin(data))/dimm[2])
m1<-rbind(long,m1)

lat<-seq(from=(raster::ymax(data)+(raster::ymin(data)-raster::ymax(data))/dimm[1]), to = raster::ymin(data) , by = (raster::ymin(data)-raster::ymax(data))/dimm[1])
lat<-c(0,lat)
data<-cbind(lat,m1, deparse.level=0)

}

}



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

####Checking

varscale<-data



if(!is.null(end)){
datos1<-replace(data, data>=end, end)
datos1[1,]<-varscale[1,]
datos1[,1]<-varscale[,1]
varscale<-datos1
rm(datos1)
codlegend<-paste(">",end)
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

ifelse(firstrow==ajuste[1,], yes=ajuste<-ajuste, no=ajuste<-rbind(firstrow,ajuste))

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

if (missing(Area)) Area="World" else Area=Area
if (missing(colbg)) colbg="transparent" else colbg=colbg
if (missing(colcon)) colcon="transparent" else colcon=colcon
if (missing(colf)) colf="black" else colf=colf
if (missing(colfexc)) colfexc="black" else colfexc=colfexc
if (missing(varscale)) varscale=NULL else varscale=varscale
color<-rev(heat.colors(100))
if (missing(colscale)) colscale<-color else colscale=colscale



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


if(jpg==TRUE) jpeg(filename = filejpg, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600) else hhjhk<-1





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

if (maxLon==180 & minLon==-180 & minLat==-90 & maxLat==90){
xl<-185
xr<-195
}
par(lwd=lwdP,fg="black",family=family)

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)

if(!is.null(end)){
legend.max<-end
}

legend.freq1=abs((legend.max-ini)/(length(colscale)-1))

legend.freq=abs((legend.max-ini)/(breaks-1))


if(missing(legend.pos)){
if((maxLon-minLon)>260 & (maxLon-minLon)/(maxLat-minLat)>2.265) legend.pos="x" else legend.pos=legend.pos
}



if (legend.pos=="y") par(oma=c(0,0,0,1)) else  par(oma=c(0,0,2,0))
image(varLon, varLat,varscale,xlim=c(minLon,maxLon),ylim=c(minLat,maxLat), axes=F, xaxs="i", yaxs="i", xlab="",ylab="", col=colscale, breaks=c(ini,seq(ini,legend.max,by=legend.freq1)))

par(new=T,lwd=lwdP)
plot(x,y,xlim=c(minLon,maxLon),ylim=c(minLat,maxLat),xlab=xlab, main="", axes=TRUE,
ylab = ylab, cex.lab=cex.lab, type="n",bty="l",
font.lab=font.lab, font.axis=font.axis,lab=lab,yaxs="i",xaxs="i",yaxt="n",xaxt="n")
mtext(text=main,side=3, line=0.3, cex=cex.main, font=font.main)

axis(side=1,xlim=c(minLon,maxLon),lwd=lwdP, cex.axis=cex.axis)
axis(side=2,ylim=c(minLat,maxLat),lwd=lwdP, cex.axis=cex.axis)

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

if(!is.null(end)){
lensequ<-length(sequ)
sequ[lensequ]<-codlegend
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
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
else{
sequ<-(seq(ini,legend.max,by=legend.freq))
sequ<-round(sequ, digits=ndigits)
}
if(!is.null(end)){
lensequ<-length(sequ)
sequ[lensequ]<-codlegend
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
