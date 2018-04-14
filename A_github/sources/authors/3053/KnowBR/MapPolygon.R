MapPolygon<-function(data, polygonname, var,  shape=NULL, shapenames=NULL, admAreas=TRUE, Area="World", minLon, maxLon, minLat, maxLat, int=30,  colbg="#FFFFFF",
colcon="#C8C8C8", colf="black", pro = TRUE, inc = 0.005, exclude = NULL, colexc = NULL, colfexc="black", colscale=rev(heat.colors(100)), colm="black", legend.pos="y", breaks=10, xl=0,
xr=0, yb=0, yt=0,  asp, lab = NULL,  xlab = "Longitude", ylab = "Latitude", main=NULL,
cex.main = 1.6,  cex.lab = 1.4, cex.axis = 1.2, cex.legend=0.9, family = "sans", font.main = 2,   font.lab = 1, font.axis = 1, lwdP=0.6, lwdC=0.1, trans=c(1,1), log=c(0,0),
ndigits=0, ini=NULL, end=NULL,   jpg=FALSE, filejpg="Map.jpg"){


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

d<-length(Area)
AA<-Area[1]

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



if(admAreas==TRUE){

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


if (AA=="World"){
datos1<-adworld[2:5,]
}
else{
datos1<-rbind(adworld1,adworld2)
}

datos1<-na.exclude(datos1)

}

datos<-data.frame(subset(data, select=polygonname), subset(data, select=var))
datos<-na.exclude(datos)

variable<-datos[,var]

if(is.null(shape)){


if(jpg==TRUE){
jpeg(filename = filejpg, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}



if(!is.null(end)){
variable<-replace(variable, variable>=end, end)
codlegend<-paste(">",end)
}



# Palette
colors<- colorRampPalette(colscale)(int)

# Attribute on shade to each shape
if(class(variable)=="factor"){
class<-cut(levels(variable)[variable], int)
}
else{
class<-cut(as.numeric(variable), int)
}


colors<-colors[class]


# Make the plot

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


legend.max=max(variable)

legend.min=min(variable)


if(legend.min<0) legend.min<-legend.min+legend.min*0.1/100 else legend.min<-legend.min-legend.min*0.1/100
if(legend.max<0) legend.max<-legend.max-legend.max*0.1/100 else legend.max<-legend.max+legend.max*0.1/100


Lati<-(maxLat+minLat)/2
if (pro==TRUE) aspe=(1/cos(Lati*pi/180)) else aspe=1
if (missing(asp)) asp=aspe else asp=asp

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)

#Legend position

if (missing(ini)){
if(min(variable)==0) ini<-0 else ini<-legend.min
}
else{
ini<-ini
}

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

Areas<-as.character(datos[,1])

leng<-length(Areas)
rbPal <- colorRampPalette(colscale)
if(!is.null(end)) vari<-append(variable,end) else vari<-variable
if(!is.null(ini)) vari<-append(vari,ini) else vari<-variable
colors<- rbPal(100)[as.numeric(cut(as.numeric(vari),breaks = 100))]
for(kk in 1:leng){
if(Area=="World") dataP<-subset(adworld, adworld[,3]==Areas[kk]) else dataP<-subset(adworld1, adworld1[,3]==Areas[kk])
polygon(dataP$Lon,dataP$Lat,col=colors[kk], border=colf)
}


#Color legend



if(!is.null(end)){
legend.max<-end
}


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


par(tmp)
if(jpg==TRUE){
dev.off()
}
}#End NULL shape


if(!is.null(shape)){
if(class(shape)=="list"){
dati<-shape[[1]]
lsh<-length(shape)
if(lsh>1){
ss<-seq(2,lsh)
hh<-as.character(shape[ss])
datos<-eval(parse(text=paste("subset(dati,",noquote(shapenames), " %in% hh)", sep="")))
}
}
else{
datos<-shape
if(class(datos)=="character"){
datos<-eval(parse(text=paste(".GlobalEnv$", datos, sep="")))
}
}



if(class(variable)=="factor"){
variable<-as.numeric(levels(variable))[variable]
}
 

if(jpg==TRUE){
jpeg(filename = filejpg, width = 8000, height = 4000, units = "px", pointsize = 14, quality = 1200, bg = "white", res = 600)
}


if(!is.null(end)){
variable<-replace(variable, variable>=end, end)
codlegend<-paste(">",end)
}

# Palette
colors<- colorRampPalette(colscale)(int)

# Attribute on shade to each shape
if(class(variable)=="factor"){
class<-cut(levels(variable)[variable], int)
}
else{
class<-cut(as.numeric(variable), int)
}

colors<-colors[class]


# Make the plot

if (missing(inc)) inc=0.005 else inc=inc


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

legend.max=max(variable)

legend.min=min(variable)


if(legend.min<0) legend.min<-legend.min+legend.min*0.1/100 else legend.min<-legend.min-legend.min*0.1/100
if(legend.max<0) legend.max<-legend.max-legend.max*0.1/100 else legend.max<-legend.max+legend.max*0.1/100



Lati<-(maxLat+minLat)/2
if (pro==TRUE) aspe=(1/cos(Lati*pi/180)) else aspe=1
if (missing(asp)) asp=aspe else asp=asp

tmp<-squishplot(xlim=c(minLon,maxLon), ylim=c(minLat,maxLat), asp=aspe)

#Legend position

if (missing(ini)){
if(min(variable)==0) ini<-0 else ini<-legend.min
}
else{
ini<-ini
}

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


sp::plot(datos, col=colm, xlim=c(minLon,maxLon),ylim=c(minLat,maxLat), add=TRUE, bg="transparent")

datos2<-eval(parse(text=paste("subset(datos,",noquote(shapenames), " %in% as.character(data[,polygonname]))", sep="")))

rbPal <- colorRampPalette(colscale)
if(!is.null(end)) vari<-append(variable,end) else vari<-variable
if(!is.null(ini)) vari<-append(vari,ini) else vari<-variable
colors<- rbPal(100)[as.numeric(cut(as.numeric(vari),breaks = 100))]

sp::plot(datos2, col=colors, xlim=c(minLon,maxLon),ylim=c(minLat,maxLat), add=TRUE, bg="transparent")

#Color legend

if(!is.null(end)){
legend.max<-end
}


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
if(length(colscale)>1){
plotrix::color.legend(xl=x1, yb=minLat, xr= x2,
yt=maxLat, sequ, gradient="y", align="rb", cex=cex.legend, rect.col=colscale[-1])
}
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
if(length(colscale)>1){
plotrix::color.legend(xl=minLon, yb=y1, xr=maxLon, yt=y2, sequ,
gradient="x", align="lt", cex=cex.legend, rect.col=colscale[-1])
}
}


par(tmp)
if(jpg==TRUE){
dev.off()
}

}#End not NULL shape


}