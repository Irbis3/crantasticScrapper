load("calls.RData")

# colors
maincolor <- "#FEEB96"
bgcolor <- broman::brocolors("bg")
fgcolor <- "white"
dotcolor <- "white"
gray <- "gray40"
purple <- broman::brocolors("web")["purple"]
green <- broman::brocolors("web")["green"]
orange <- broman::brocolors("web")["orange"]

calls <- sub("\\*", "", calls)

library(igraph)
iArrows <- igraph:::igraph.Arrows


library(B6BTBR07a)
data(topstuff)

ts.mousenum <- sapply(strsplit(rownames(topstuff), "_"), function(a) a[[2]])
ts.plate <- as.character(as.numeric(sapply(strsplit(as.character(topstuff[,3]), "[_:]"), function(a) a[2])))
ts.well <- sapply(strsplit(as.character(topstuff[,3]), "[_:]"), function(a) a[3])

ts.strain <- sapply(strsplit(rownames(topstuff), "[_:]"), function(a) a[3])
ts.strain[rownames(topstuff) == "MouseKitControl_755356"] <- "CNTL"

ro.plate <- ts.plate[match(rownames(calls), ts.mousenum)]
ro.well <- ts.well[match(rownames(calls), ts.mousenum)]

calls[,"DNA"] <- sub("dup", "", calls[,"DNA"])
bad <- which(rownames(calls) != calls[,"DNA"])
notfound <- grep("not", calls[,"DNA"])
omit <- grep("omit", calls[,"DNA"])
noexpr <- grep("maybe", calls[,"DNA"])
okay <- which(!is.na(calls[,"DNA"]) & rownames(calls) == calls[,"DNA"])

######################################################################

num <- rep(1:11, rep(8,11))
num[num < 10] <- paste(0, num[num<10], sep="")
wells <- paste(LETTERS[1:8], num, sep="")

ro.well.num <- match(ro.well, wells)
ro.plate.num <- as.numeric(ro.plate) + (ro.well.num-89/2)/87

######################################################################

fullplatenum <- paste(as.numeric(ro.plate), ro.well, sep=":")
names(fullplatenum) <- rownames(calls)
corrected.fullplatenum <- rep("", nrow(calls))
names(corrected.fullplatenum) <- rownames(calls)
corrected.fullplatenum[-c(notfound,omit)] <- fullplatenum[calls[-c(notfound,omit),"DNA"]]
corrected.fullplatenum[corrected.fullplatenum==""] <- NA

split.fullplatenum <- matrix(ncol=3, nrow=length(fullplatenum))
split.fullplatenum[,1] <- as.numeric(sapply(strsplit(fullplatenum, ":"), function(a) a[1]))
split.fullplatenum[,2] <- match(substr(sapply(strsplit(fullplatenum, ":"), function(a) a[2]), 1, 1), LETTERS[1:8])
split.fullplatenum[,3] <- as.numeric(substr(sapply(strsplit(fullplatenum, ":"), function(a) a[2]), 2, 3))

split.corrected.fullplatenum <- matrix(ncol=3, nrow=length(corrected.fullplatenum))
split.corrected.fullplatenum[,1] <- as.numeric(sapply(strsplit(corrected.fullplatenum, ":"), function(a) a[1]))
split.corrected.fullplatenum[,2] <- match(substr(sapply(strsplit(corrected.fullplatenum, ":"), function(a) a[2]), 1, 1), LETTERS[1:8])
split.corrected.fullplatenum[,3] <- as.numeric(substr(sapply(strsplit(corrected.fullplatenum, ":"), function(a) a[2]), 2, 3))

######################################################################
controlplatenum <- paste(ts.plate, ts.well, sep=":")[ts.strain=="B6" | ts.strain=="F1" | ts.strain=="BTBR"]
names(controlplatenum) <- ts.strain[ts.strain=="B6" | ts.strain=="F1" | ts.strain=="BTBR"]

split.controlplatenum <- matrix(ncol=3, nrow=length(controlplatenum))
split.controlplatenum[,1] <- as.numeric(sapply(strsplit(controlplatenum, ":"), function(a) a[1]))
split.controlplatenum[,2] <- match(substr(sapply(strsplit(controlplatenum, ":"), function(a) a[2]), 1, 1), LETTERS[1:8])
split.controlplatenum[,3] <- as.numeric(substr(sapply(strsplit(controlplatenum, ":"), function(a) a[2]), 2, 3))
rownames(split.controlplatenum) <- names(controlplatenum)
######################################################################

pdf("../Figs/plate_errors.pdf", height=6.5, width=10, pointsize=10)

par(mar=rep(0,4), bty="n")
par(bg=bgcolor, fg=fgcolor, col=fgcolor, col.axis=fgcolor)

xoff <- 0
yoff <- 2
par(bty="n")
plot(0,0,type="n", xlab="", ylab="", xlim=c(-xoff,142+xoff), ylim=c(38+yoff,-yoff),
     xaxt="n", yaxt="n")

u <- par("usr")

plx1 <- c(0,0,50,50,50,100,100)[c(6,2,5,3,4,7,1)]
plx2 <- (plx1+45)
ply1 <- c(7.5,22.5,0,15,30,7.5,22.5)[c(6,2,5,3,4,7,1)]
ply2 <- (ply1+10)
colx <- seq(min(plx1), min(plx2), len=14)
colx <- colx[-(1:2)] - diff(colx[1:2])
rowy <- seq(max(ply1), max(ply2), len=10)-max(ply1)
rowy <- rowy[-(1:2)] - diff(rowy[1:2])
plates <- 1628:1634

# plot circles for each well (black, gray, or hotpink)
z <- split.fullplatenum
z <- z[!is.na(z[,1]),]
for(pl in 1:7) {
  for(i in 1:8) {
    for(j in 1:12) {
      wh <- !is.na(z[,1]) & z[,1]==plates[pl] & z[,2]==i & z[,3]==j
      if(any(wh)) {


        if(sum(calls[,"DNA"] == rownames(calls)[wh]) > 1) # sample duplicate
          points(plx1[pl] + colx[j], ply1[pl] + rowy[i], cex=1.2, col="hotpink")

        else if(length(grep("^notMouse", calls[wh,"DNA"])) > 0)
          points(plx1[pl] + colx[j], ply1[pl] + rowy[i], cex=1.2, col=dotcolor)

        else # genotyped + has expression
          points(plx1[pl] + colx[j], ply1[pl] + rowy[i], cex=1.2, col=dotcolor)

        if(length(grep("^notMouse", calls[wh,"DNA"])) > 0) {  # unknown DNA; small orange arrow
          iArrows(plx1[pl] + colx[j] - diff(colx[1:2])*0.4,
                  ply1[pl] + rowy[i] - diff(rowy[1:2])*0.6,
                  plx1[pl] + colx[j],
                  ply1[pl] + rowy[i],
                  sh.col=orange, curve=0,
                  size=0.6, width=1, h.lwd=1, sh.lwd=2)

        }
      }
      else
        points(plx1[pl] + colx[j], ply1[pl] + rowy[i], cex=1.2, col=gray)

      if(length(grep("^maybe", calls[wh,"DNA"]))>0) # no expression data for this sample
        points(plx1[pl] + colx[j], ply1[pl] + rowy[i], pch=16, cex=0.8, col=gray)
    }
  }
}


# add small arrows for DNAs lost (purple: has expr data; green: no expr data)
lost <- which(is.na(match(rownames(calls), calls[,"DNA"])) & is.na(match(paste0("maybe", rownames(calls)), calls[,"DNA"])))
lost <- lost[!(lost %in% omit)]
lost.names <- rownames(calls)[lost]
lost.hasexpr <- sapply(lost.names, function(a,b) any(!is.na(b) & b==a), calls[,1:6])
z <- split.fullplatenum[lost,]
for(i in 1:nrow(z)) {
  if(z[i,1] == "1629") mult <- -1 else mult <- +1

  iArrows(plx1[match(z[i,1], plates)] + colx[z[i,3]],
          ply1[match(z[i,1], plates)] + rowy[z[i,2]],
          plx1[match(z[i,1], plates)] + colx[z[i,3]] + mult*diff(colx[1:2])*0.4,
          ply1[match(z[i,1], plates)] + rowy[z[i,2]] + diff(rowy[1:2])*0.6,
          sh.col=c(green, purple)[lost.hasexpr[i]+1], curve=0,
          size=0.6, width=1, h.lwd=1, sh.lwd=2)
}



# labels for controls
for(i in 1:nrow(split.controlplatenum)) {
  zz <- split.controlplatenum[i,]
  text(plx1[match(zz[1], plates)] + colx[zz[3]],
       ply1[match(zz[1], plates)] + rowy[zz[2]], rownames(split.controlplatenum)[i], cex=0.6)
}

arrowcol <- rep(fgcolor, length(corrected.fullplatenum))
names(arrowcol) <- names(fullplatenum)
wh <- which(!is.na(corrected.fullplatenum) & corrected.fullplatenum != fullplatenum)
z <- split.corrected.fullplatenum
z <- z[,1]*12*8 + z[,3]*8 + z[,2]
thearrows <- names(corrected.fullplatenum[wh][order(z[wh])])


# black dots at wells with correct dna
for(i in 1:nrow(split.corrected.fullplatenum)) {
  z <- split.corrected.fullplatenum[i,]
  if(is.na(z[1])) next

  if(fullplatenum[i] == corrected.fullplatenum[i])
    points(plx1[match(z[1], plates)] + colx[z[3]],
           ply1[match(z[1], plates)] + rowy[z[2]], pch=16, cex=0.8, col=dotcolor)
}


# save arrow info, if necessary
file <- "genotype_plates_arrows.csv"
if(file.exists(file)) {
  arrowinfo <- read.csv(file, as.is=TRUE)
} else {

  # to contain arrow info
  arrowinfo <- matrix("0", nrow=nrow(split.corrected.fullplatenum), ncol=4)
  colnames(arrowinfo) <- c("from", "to", "curve", "color")
  curarrow <- 1

  # capture "from" and "to" info
  for(i in 1:nrow(split.corrected.fullplatenum)) {
    z <- split.corrected.fullplatenum[i,]
    if(is.na(z[1])) next

    if(fullplatenum[i] != corrected.fullplatenum[i]) {
      zz <- split.fullplatenum[i,]

      arrowinfo[curarrow,1] <- paste(z, collapse=":")
      arrowinfo[curarrow,2] <- paste(zz, collapse=":")
      arrowinfo[curarrow,4] <- ((curarrow-1)%%3)+1
      curarrow <- curarrow + 1
    }
  }
  arrowinfo <- arrowinfo[1:(curarrow-1),]
  write.csv(arrowinfo, file=file, quote=FALSE, row.names=FALSE)
}
arrowcolors <- c(rgb(0.4,0.4,1), rgb(0.6, 0.6, 1), rgb(0.8, 0.8, 1))
arrowinfo[,4] <- arrowcolors[arrowinfo[,4]]

# the arrows
for(i in 1:nrow(split.corrected.fullplatenum)) {
  z <- split.corrected.fullplatenum[i,]
  if(is.na(z[1])) next

  if(fullplatenum[i] != corrected.fullplatenum[i]) {
    zz <- split.fullplatenum[i,]
    zp <- paste(z, collapse=":")
    zzp <- paste(zz, collapse=":")
    curve <- arrowinfo[arrowinfo[,1]==zp & arrowinfo[,2]==zzp,3]
    sh.col <- arrowinfo[arrowinfo[,1]==zp & arrowinfo[,2]==zzp,4]

    iArrows(plx1[match(z[1], plates)] + colx[z[3]],
            ply1[match(z[1], plates)] + rowy[z[2]],
            plx1[match(zz[1], plates)] + colx[zz[3]],
            ply1[match(zz[1], plates)] + rowy[zz[2]],
            sh.col=sh.col, curve=curve,
            size=0.6, width=1,
            h.lwd=1, sh.lwd=1)
  }
}



# plot the plates and labels
rect(plx1,ply1,plx2,ply2, lwd=2)
text((plx1+plx2)/2, ply1 - diff(rowy[1:2])*1.8, plates, col=maincolor, cex=1.3)
for(i in 1:7) {
  text(plx1[i] - diff(colx[1:2])*0.5, ply1[i] + rowy[1:8], LETTERS[1:8], cex=0.8)
  text(plx1[i] + colx[1:12], ply1[i] - diff(rowy[1:2])*0.5, 1:12, cex=0.8)
}

# add red X's for DNAs omitted
z <- split.fullplatenum[omit,]
for(i in 1:nrow(z))
  points(plx1[match(z[i,1], plates)] + colx[z[i,3]],
         ply1[match(z[i,1], plates)] + rowy[z[i,2]], cex=1.5, pch=4, col="violetred", lwd=2)



dev.off()
