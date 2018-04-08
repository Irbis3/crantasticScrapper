SurveyQ<-function(data, Longitude=NULL, Latitude=NULL, cell=60, Areas=NULL,
variables = c("Slope","Completeness","Ratio"),  completeness=c(50,90),
slope=c(0.02,0.3), ratio=c(3,15), shape=NULL, shapenames=NULL, admAreas=TRUE,
Area="World", minLon, maxLon, minLat, maxLat, main=NULL, PLOTP=NULL,  PLOTB=NULL,
 POINTS=NULL,   XLAB=NULL, YLAB=NULL, XLIM=NULL, YLIM=NULL,
palette=c("blue","green","red"), COLOR=c("red","green","blue"),  colm="black", labels=TRUE,
sizelabels=1, LEGENDP=NULL, LEGENDM=NULL, file="Polar coordinates.csv",
na="NA", dec=",", row.names=FALSE, jpg=FALSE, filejpg="Map.jpg"){

if(missing(Latitude) & missing(Longitude) & missing(Areas)){
stop("It is necessary to specify the arguments Latitude and Longitude, if the file 'Estimators' was obtained with the function KnowB, or the argument Areas
if the file 'Estimators' was obtained with the function KnowBPolygon")
}


"%ni%" <- Negate( "%in%" )

###Function to add the labels of the areas to the plot
TE<- function(datosL=datosL, dim=dim, COLOR=NULL,sizelabels=1){
h<-0
for(z in 1:dim[1]){
if(h==6) h<-1 else h<-h+1
c<-plotrix::draw.circle(datosL[z,2], datosL[z,3], rangeX*1/100, nv = 6, border = "transparent", col = "transparent")
datosL[z,2]<-c$x[h]
datosL[z,3]<-c$y[h]
text(x = datosL[z,2] , y = datosL[z,3] , labels = datosL[z,1] , col = COLOR, cex=sizelabels)
}
}
####End function TE

####Function Bubbles

Bubbles<-function(data, varY, varX, varColor=NULL, palette= "cm.colors",
digitsC=0, ncolor=10, transparency=1, PLOTB=NULL, POINTS=NULL, COLEGEND=NULL, XLAB=NULL,
YLAB=NULL, XLIM=NULL, YLIM=NULL, LEGENDS=NULL){

datos<-data

datosT<-data.frame(subset(datos, select=varX), subset(datos, select=varY))

if(!is.null(varColor)){
datosT<-data.frame(datosT, subset(datos, select=varColor))
}

datos<-na.exclude(datosT)


par(font.lab=2, mar=c(4.5,4.5,3,5),cex.lab=1.5)


if(!is.null(XLAB)) xlab<-XLAB else xlab<-varX

if(!is.null(YLAB)) ylab<-YLAB else ylab<-varY


if(!is.null(XLIM)){
minsx<-XLIM[1]
maxsx<-XLIM[2]
}
else{
minsx<-min(datos[,varX])
maxsx<-max(datos[,varX])
XLIM<-c(minsx,maxsx)
}


if(!is.null(YLIM)){
minsy<-YLIM[1]
maxsy<-YLIM[2]
}
else{
minsy<-min(datos[,varY])
maxsy<-max(datos[,varY])
YLIM<-c(minsy,maxsy)
}


if(!is.null(varColor)){
maxC<-max(datos[,varColor])
minC<-min(datos[,varColor])
matriz<-matrix(c(maxC,minC, 1, ncolor),nrow = 2 , ncol = 2)
regC<-lm(matriz[,2]~matriz[,1])
}


if(palette== "heat.colors"){
rampa<-heat.colors(n=ncolor, alpha=transparency)
}

if(palette== "terrain.colors"){
rampa<-terrain.colors(n=ncolor, alpha=transparency)
}

if(palette== "gray.colors"){
rampa<-gray.colors(n=ncolor, alpha=transparency)
}


if(palette== "topo.colors"){
rampa<-topo.colors(n=ncolor, alpha=transparency)
}


if(palette== "cm.colors"){
rampa<-cm.colors(n=ncolor, alpha=transparency)
}

if(palette!= "heat.colors" & palette!= "topo.colors" & palette!= "gray.colors" & palette!= "cm.colors" & palette!= "terrain.colors"){
ramp <- colorRamp(palette)
rampa<-rgb(ramp(seq(0, 1, length = ncolor)), maxColorValue = 255)
}

cex<-1

if(!is.null(varColor)){
color<-round(regC$coefficients[1]+regC$coefficients[2]*datos[1,varColor])
pch<-16
}
else{
rampa<-"black"
pch<-1
color<-1
}

if(!is.null(PLOTB)){
scatterplotexe<-paste("plot(","x=datos[1,varX],", "y=datos[1,varY],", toString(x=PLOTB), ")")
eval(parse(text=scatterplotexe))
}
else{
scatterplotexe<-paste("plot(","x=datos[1,varX],", "y=datos[1,varY],","cex=cex,", "col=rampa[color],",
"xlim=XLIM,","ylim=YLIM,", "xlab=xlab,","ylab=ylab,", "pch=pch", ")")
eval(parse(text=scatterplotexe))
}

dimS<-dim(datos)
for(zz in 2:dimS[1]){
cex=1
if(!is.null(varColor)){
color<-round(regC$coefficients[1]+regC$coefficients[2]*datos[zz,varColor])
}
if(!is.null(POINTS)){
scatterplotexe<-paste("points(","x=datos[zz,varX],", "y=datos[zz,varY],", toString(x=POINTS), ")")
eval(parse(text=scatterplotexe))
}
else{
scatterplotexe<-paste("points(","x=datos[zz,varX],", "y=datos[zz,varY],","cex=cex,", "col=rampa[color],",
"xlim=XLIM,","ylim=YLIM,", "pch=pch", ")")
eval(parse(text=scatterplotexe))
}
}


ranX<-abs(XLIM[2]-XLIM[1])
ranY<-abs(YLIM[2]-YLIM[1])


x1<-XLIM[2]+ranX*8/100
x2<-XLIM[2]+ranX*12/100

if(palette== "heat.colors"){
rampa<-heat.colors(n=100, alpha=transparency)
}

if(palette== "terrain.colors"){
rampa<-terrain.colors(n=100, alpha=transparency)
}

if(palette== "gray.colors"){
rampa<-gray.colors(n=100, alpha=transparency)
}


if(palette== "topo.colors"){
rampa<-topo.colors(n=100, alpha=transparency)
}


if(palette== "cm.colors"){
rampa<-cm.colors(n=100, alpha=transparency)
}


if(palette!= "heat.colors" & palette!= "topo.colors" & palette!= "gray.colors" & palette!= "cm.colors" & palette!= "terrain.colors"){
ramp <- colorRamp(palette)
rampa<-rgb(ramp(seq(0, 1, length = 100)), maxColorValue = 255)
}

if(!is.null(varColor)){

int<-as.numeric(format((maxC-minC)/ncolor, digits=digitsC))
maxC<-as.numeric(format(maxC,digits=digitsC))
minC<-as.numeric(format(minC,digits=digitsC))

color<-minC
valor<-minC
for(zz in 1:ncolor){
valor<-valor+int
color<-append(color,valor)
}

y1<-YLIM[1]-YLIM[1]*3/100

y2<-YLIM[2]+YLIM[2]*1.5/100

if(!is.null(COLEGEND)){
scatterplotexe<-paste("plotrix::color.legend(","xl=x1,", "yb=y1,", "xr=x2,", "yt=y2,", toString(x=COLEGEND), ")")
eval(parse(text=scatterplotexe))
}
else{
scatterplotexe<-paste("plotrix::color.legend(","xl=x1,", "yb=y1,", "xr=x2,", "yt=y2,",
"legend=color,","gradient='y',", "align='rb',", "cex=1,", "rect.col=rev(rampa)",  ")")
eval(parse(text=scatterplotexe))
}

}


}####End function Bubble


####Function adareas

adareas<-function(data, Area="World", minLon, maxLon, minLat, maxLat,
colbg="#FFFFFF", colcon="#C8C8C8", colf="black", pro = TRUE,  inc = 0.005,
exclude = NULL, colexc = NULL, colfexc="black",  colscale=rev(heat.colors(100)),
legend.pos="y", breaks=10, xl=0, xr=0, yb=0, yt=0, asp, lab = NULL,  xlab = "Longitude",
ylab = "Latitude", main=NULL, cex.main = 1.2, cex.lab = 1, cex.axis = 0.9, cex.legend=0.9,
family = "sans", font.main = 2,  font.lab = 1, font.axis = 1, lwdP=0.6, lwdC=0.1,
trans=c(1,1), log=c(0,0), ndigits=0, ini=NULL, end=NULL, jpg=FALSE, filejpg="Map.jpg"){

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


####End function adareas



#Polar coordinates

n<-length(variables)

var<-variables

if(!is.null(Areas)){
datosT<-data.frame(subset(data, select=Areas), subset(data, select=var))
datos<-na.exclude(datosT)
selection<-datos[,-1]
}


if(!is.null(Longitude)){
datosT<-data.frame(subset(data, select=Longitude), subset(data, select=Latitude), subset(data, select=var))
datos<-na.exclude(datosT)
selection<-datos[,c(-1,-2)]
}

var<-colnames(selection)


#Standardization 1 to -1

datosE<-selection

for (z in 1:3){
matrixE<-matrix(c(-1, 1, min(selection[,z],na.rm=TRUE),max(selection[,z],na.rm=TRUE)), nrow = 2 , ncol = 2)
reg<-lm(matrixE[,1]~matrixE[,2])
datosC<-reg$coefficients[1]+selection[,z]*reg$coefficients[2]
datosE<-cbind(datosE,datosC)
}

datosE<-datosE[,-c(1:3)]

colnames(datosE)<-colnames(selection)

selection<-datosE

#Estimation of polar coordinates

angle<-pi/3

datosX<-selection[,1]
h<-0
for (z in 1:3){
h<-h+1
datosC<- ifelse(selection[,z] <=0, abs(selection[,z])*cos(angle*h+pi), abs(selection[,z])*cos(angle*h)) 
datosX<-data.frame(datosX,datosC)
}
datosX<-datosX[,-1]

XX<-apply(datosX,1,sum)


RX<-(max(XX)-min(XX))


datosY<-selection[,1]
h<-0
for (z in 1:3){
h<-h+1
datosC<- ifelse(selection[,z] <=0, abs(selection[,z])*sin(angle*h+pi), abs(selection[,z])*sin(angle*h)) 
datosY<-cbind(datosY,datosC)
}

datosY<-datosY[,-1]

YY<-apply(datosY,1,sum)

RY<-(max(YY)-min(YY))

datosF<-data.frame(XX,YY)


if(!is.null(Areas)){
datosF<-data.frame(datos[,1], datosF)
colnames(datosF)<-c("Area", "X","Y")
}

if(!is.null(Longitude)){
datosF<-data.frame(datos[,c(1,2)], datosF)
colnames(datosF)<-c("Longitude","Latitude", "X","Y")
}


if(dec=="."){
write.csv(x=datosF,file = file, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = datosF,file = file, fileEncoding = "", row.names=row.names,na=na)
}


if(dec=="."){
datosF<-read.csv(file=file ,header=TRUE)
}
else{
datosF<-read.csv2(file=file ,header=TRUE)
}


###Plot Polar coordinates
if(!is.null(XLAB)) xlab<-XLAB else xlab<-"POLAR COORDINATES X"

if(!is.null(YLAB)){
ylab<-YLAB
}
else{
ylab<-"POLAR COORDINATES Y"
}

if(is.null(Longitude)){
xx=datosF[,2]
yy=datosF[,3]
}
else{
xx=datosF[,3]
yy=datosF[,4]
}


if(is.null(XLIM)){
XLIM<-c(min(xx),max(xx))
}

if(is.null(YLIM)){
YLIM<-c(min(yy),max(yy))
}

rangeX<-abs(XLIM[2]-XLIM[1])

dev.new()

par(font.lab=2, mar=c(5,5,3,2),cex.lab=1.5)


if(!is.null(PLOTP)){
plotexe<-paste("plot(","x=xx,", "y=yy,", toString(x=PLOTP), ")")
eval(parse(text=plotexe))
}
else{
plotexe<-paste("plot(","x=xx,", "y=yy,", "xlab=xlab,","ylab=ylab,","xlim=XLIM,","ylim=YLIM,", "cex=0",")")
eval(parse(text=plotexe))
}



#####Polygons
if(!is.null(Areas)){
###Labels of good surveyed areas
datosT<-cbind(datosF,datos)
datosL<-subset(datosT,datosT[,variables[1]]<slope[1] & datosT[,variables[2]]>completeness[2] & datosT[,variables[3]]>ratio[2])
datosL<-datosL[,c(1,2,3)]
datosL<-datosL[order(datosL[,2]),]
dim<-dim(datosL)
eti<-rep("Good",dim[1])
datosL<-cbind(datosL,eti)
names(datosL)<-c("Area","X","Y","Survey")
ldata<-datosL

if(dim[1]>0){
if(labels==FALSE | !is.null(Longitude)) {
points(x = datosL[,2] , y = datosL[,3], pch=16, col=COLOR[1],cex=sizelabels)
}
else{
TE(datosL=datosL, dim=dim, COLOR=COLOR[1],sizelabels=sizelabels)
}
}

###Labels of poor surveyed areas
datosL<-subset(datosT,datosT[,variables[1]]>slope[2] & datosT[,variables[2]]<completeness[1] & datosT[,variables[3]]<ratio[1])
datosL<-datosL[,c(1,2,3)]
datosL<-datosL[order(datosL[,2]),]
dim<-dim(datosL)
eti<-rep("Poor",dim[1])
datosP<-cbind(datosL,eti)
names(datosP)<-c("Area","X","Y","Survey")

if(dim[1]>0){
if(labels==FALSE | !is.null(Longitude)) {
points(x = datosP[,2] , y = datosP[,3], pch=16, col=COLOR[3],cex=sizelabels)
}
else{
TE(datosL=datosP, dim=dim, COLOR=COLOR[3],sizelabels=sizelabels)
}
}

###Labels of fair surveyed areas
datosL<-subset(datosT,(datosT[,variables[1]]>=slope[1] & datosT[,variables[1]]<=slope[2]) | (datosT[,variables[2]]>=completeness[1] & datosT[,variables[2]]<=completeness[2] ) | (datosT[,variables[3]]>=ratio[1] & datosT[,variables[3]]<=ratio[2]))
datosL<-datosL[,c(1,2,3)]
datosL<-datosL[order(datosL[,2]),]
dim<-dim(datosL)
eti<-rep("Fair",dim[1])
datosFF<-cbind(datosL,eti)
names(datosFF)<-c("Area","X","Y","Survey")

if(dim[1]>0){
if(labels==FALSE | !is.null(Longitude)) {
points(x = datosFF[,2] , y = datosFF[,3], pch=16, col=COLOR[2],cex=sizelabels)
}
else{
TE(datosL=datosFF, dim=dim, COLOR=COLOR[2],sizelabels=sizelabels)
}
}

ldata<-rbind(ldata,datosFF, datosP)


#LEGEND
if(!is.null(LEGENDP)){
legendexe<-paste("legend(",toString(x=LEGENDP), ")")
eval(parse(text=legendexe))
}
else{
legendexe<-paste("legend(","x='bottomleft',", "legend=c('High quality survey','Fair quality survey','Poor quality survey'),", "pch=15,", "col=COLOR,", "bty='n'", ")")
eval(parse(text=legendexe))
}


dim<-dim(ldata)
valor<-rep(1, dim[1])
ldata<-cbind(ldata,valor)
names(ldata)<-c("Area","X","Y","Survey","Survey2")
ldata$Survey2[which(ldata$Survey=="Good")]<-3
ldata$Survey2[which(ldata$Survey=="Fair")]<-2
ldata$Survey2[which(ldata$Survey=="Poor")]<-1
ldata[,"Survey2"]<-as.numeric(ldata[,"Survey"])

catcol<-as.character(unique(ldata[,4]))
if(any(catcol=="Good")==TRUE) vp1="" else vp1=COLOR[1]
if(any(catcol=="Fair")==TRUE) vp2="" else vp2=COLOR[2]
if(any(catcol=="Poor")==TRUE) vp3="" else vp3=COLOR[3]
COLORF<-COLOR[ COLOR %ni% c(vp1,vp2,vp3)]

datosT<-ldata

if(!is.null(shape)){

if(class(shape)=="list"){
data<-shape[[1]]
lsh<-length(shape)
if(lsh>1){
ss<-seq(2,lsh)
hh<-as.character(shape[ss])
shapeT<-eval(parse(text=paste("subset(data,",noquote(shapenames), " %in% hh)", sep="")))
}
}
else{
shapeT<-shape
if(class(shapeT)=="character"){
shapeT<-eval(parse(text=paste(".GlobalEnv$", shapeT, sep="")))
}
}


AreasT<-data.frame(eval(parse(text=paste("shapeT$",noquote(shapenames),sep=""))))
names(AreasT)<-"Area"
datosT<-merge(AreasT,ldata, sort=FALSE)

}

if(jpg==TRUE){
jpeg(filename = filejpg, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}
else{
dev.new()
}

KnowBR::MapPolygon(data=datosT, polygonname="Area", Area=Area, var="Survey2", colscale=COLORF, jpg=FALSE, xl=500, xr=500, shape=shape, shapenames=shapenames,
minLon=minLon, maxLon=maxLon,minLat=minLat, maxLat=maxLat, admAreas=admAreas, main=main, colm=colm)

#LEGEND
if(!is.null(LEGENDM)){
legendexe<-paste("legend(",toString(x=LEGENDM), ")")
eval(parse(text=legendexe))
}
else{
legendexe<-paste("legend(","x='bottomleft',", "legend=c('High quality survey','Fair quality survey','Poor quality survey'),", "pch=15,", "col=COLOR,", "bty='n'", ")")
eval(parse(text=legendexe))
}
if(jpg==TRUE){
dev.off()
}

ldata<-ldata[,-5]

}#End Areas NULL

#####Cells

if(!is.null(Longitude)){

datosT<-cbind(datosF,datos)

###Labels of good surveyed areas
datosL<-subset(datosT,datosT[,variables[1]]<slope[1] & datosT[,variables[2]]>completeness[2] & datosT[,variables[3]]>ratio[2])
datosL<-datosL[,c(1,2,3,4)]
dim<-dim(datosL)
eti<-rep("Good",dim[1])
datosL<-cbind(datosL,eti)
names(datosL)<-c("Longitude","Latitude","X","Y","Survey")
ldata<-datosL

points(x = datosL[,3] , y = datosL[,4], pch=16, col=COLOR[1],cex=sizelabels)

###Labels of poor surveyed areas
datosL<-subset(datosT,datosT[,variables[1]]>slope[2] & datosT[,variables[2]]<completeness[1] & datosT[,variables[3]]<ratio[1])
datosL<-datosL[,c(1,2,3,4)]
dim<-dim(datosL)
eti<-rep("Poor",dim[1])
datosP<-cbind(datosL,eti)
names(datosP)<-c("Longitude","Latitude","X","Y","Survey")

points(x = datosP[,3] , y = datosP[,4], pch=16, col=COLOR[3],cex=sizelabels)

###Labels of fair surveyed areas
datosL<-subset(datosT,(datosT[,variables[1]]>=slope[1] & datosT[,variables[1]]<=slope[2]) | (datosT[,variables[2]]>=completeness[1] & datosT[,variables[2]]<=completeness[2] ) | (datosT[,variables[3]]>=ratio[1] & datosT[,variables[3]]<=ratio[2]))
datosL<-datosL[,c(1,2,3,4)]
dim<-dim(datosL)
eti<-rep("Fair",dim[1])
datosFF<-cbind(datosL,eti)
names(datosFF)<-c("Longitude","Latitude","X","Y","Survey")

ldata<-rbind(ldata,datosFF, datosP)

points(x = datosFF[,3] , y = datosFF[,4], pch=16, col=COLOR[2],cex=sizelabels)

#LEGEND
if(!is.null(LEGENDP)){
legendexe<-paste("legend(",toString(x=LEGENDP), ")")
eval(parse(text=legendexe))
}
else{
legendexe<-paste("legend(","x='bottomleft',", "legend=c('High quality survey','Fair quality survey','Poor quality survey'),", "pch=15,", "col=COLOR,", "bty='n'", ")")
eval(parse(text=legendexe))
}

dim<-dim(ldata)
valor<-rep(1, dim[1])
ldata<-cbind(ldata,valor)
names(ldata)<-c("Longitude","Latitude","X","Y","Survey","Survey2")
ldata$Survey2[which(ldata$Survey=="Good")]<-3
ldata$Survey2[which(ldata$Survey=="Fair")]<-2
ldata$Survey2[which(ldata$Survey=="Poor")]<-1
ldata[,"Survey2"]<-as.numeric(ldata[,"Survey"])

catcol<-as.character(unique(ldata[,5]))
if(any(catcol=="Good")==TRUE) vp1="" else vp1=COLOR[1]
if(any(catcol=="Fair")==TRUE) vp2="" else vp2=COLOR[2]
if(any(catcol=="Poor")==TRUE) vp3="" else vp3=COLOR[3]
COLORF<-COLOR[ COLOR %ni% c(vp1,vp2,vp3)]


if(jpg==TRUE){
jpeg(filename = filejpg, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}
else{
dev.new()
}


f<-cell/60
ff<-180/f
cc<-ff*2
matriz<-matrix(-9999, nrow=ff, ncol=cc)
col<-c(0,seq(from=-180+f, to=180, by=f))
row<-c(seq(from=-90, to=90-f, by=f))
names(matriz)<-NULL
matriz<-cbind(row,matriz)
matriz<-rbind(col,matriz)
colnames(matriz)<-NULL

le<-length(ldata[,1])
x<-matriz[1,-1]
y<-matriz[-1,1]

for(z in 1:le){
vx<-findInterval(ldata[z,1], x)
vy<-findInterval(ldata[z,2], y)
matriz[vy+1,vx+2]<-ldata[z,6]
}

matriz <- matriz[ nrow(matriz):2, ]
matriz<-rbind(col,matriz)
adareas(data=matriz, Area=Area,  jpg=FALSE, minLon=minLon, maxLon=maxLon,minLat=minLat, maxLat=maxLat, colcon="transparent",
xl=500, xr=500, colscale=append("transparent",COLORF))

rm(matriz)

#LEGEND
if(!is.null(LEGENDM)){
legendexe<-paste("legend(",toString(x=LEGENDM), ")")
eval(parse(text=legendexe))
}
else{
legendexe<-paste("legend(","x='bottomleft',", "legend=c('High quality survey','Fair quality survey','Poor quality survey'),", "pch=15,", "col=COLOR,", "bty='n'", ")")
eval(parse(text=legendexe))
}

if(jpg==TRUE){
dev.off()
}

ldata<-ldata[,-6]

}#End Longitude NULL

##Saving the file

if(dec=="."){
write.csv(x=ldata,file = file, fileEncoding = "", row.names=row.names,na=na)
}
else{
write.csv2(x = ldata,file = file, fileEncoding = "", row.names=row.names,na=na)
}

##Bubble chart

dev.new()
LRatio<-log(data[,variables[3]])
data<-cbind(data,LRatio)

Bubbles(data = data , varY = variables[2] , varX = "LRatio" , XLAB="log Ratio",varColor = variables[1] , digitsC = 1, PLOTB=PLOTB, palette=palette,
POINTS=POINTS)

}
