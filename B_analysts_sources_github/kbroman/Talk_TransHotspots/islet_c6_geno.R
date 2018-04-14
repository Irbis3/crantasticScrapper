data.folder <- "../AttieData/"
source("myplotgeno.R")

pdf("../Figs/islet_c6_geno_C.pdf", height=6,width=10, pointsize=10)
layout(rbind(c(1,1,1), c(2,3,4)),width=c(8.5,4.5,2))

########################################
# Data for Panel A

load("../Data/Chr6.genotype.map.2013-11-08.RData")


batch0 <- c("rs13479058", "rs13479059", "rs4226339", "rs13479070",
            "rs13479071", "rs3024135", "rs8262456", "rs13479085", "rs13479086",
            "rs3089737", "rs13479087")

Mouse <- c("Mouse3326", "Mouse3478", "Mouse3330", "Mouse3131",
           "Mouse3550", "Mouse3477", "Mouse3125", "Mouse3300", "Mouse3312",
           "Mouse3220", "Mouse3535", "Mouse3373", "Mouse3605", "Mouse3523",
           "Mouse3149", "Mouse3351", "Mouse3119", "Mouse3448", "Mouse3229",
           "Mouse3364", "Mouse3633", "Mouse3275", "Mouse3518", "Mouse3472",
           "Mouse3247", "Mouse3638", "Mouse3587", "Mouse3171", "Mouse3439",
           "Mouse3291", "Mouse3055", "Mouse3096", "Mouse3643", "Mouse3182",
           "Mouse3619", "Mouse3194", "Mouse3176", "Mouse3287", "Mouse3206",
           "Mouse3130", "Mouse3421", "Mouse3625", "Mouse3460", "Mouse3213",
           "Mouse3414", "Mouse3505", "Mouse3054", "Mouse3146", "Mouse3419",
           "Mouse3232", "Mouse3630", "Mouse3450", "Mouse3558", "Mouse3139",
           "Mouse3577", "Mouse3616", "Mouse3607", "Mouse3516", "Mouse3253",
           "Mouse3542", "Mouse3200", "Mouse3487", "Mouse3473", "Mouse3603" )
# reorder key recombinant mice by Mouse Number within genotype
Mouse[32:60] <- Mouse[31+c(1, 3, 2,
                           5, 4, 6,
                           8, 7,
                           9, 13, 10, 12, 11,
                           16, 23, 17, 19, 14, 18, 21, 15, 22, 20,
                           28, 27, 29, 24, 26, 25)]

MouseNum <- substr(Mouse, 6, 10)

lname <- load("../Data/LDA.recomb.RData")
qtlgn <- cl.lda$pred.test
gn <-  genotype.test

load(paste0(data.folder, "input/aligned_geno_with_pmap.RData"))
map <- pmap[["6"]][batch0]

gn <- gn[Mouse,batch0]
qtlgn <- qtlgn[Mouse]
map <- map[batch0]

g <- as.matrix(gn)
n <- nrow(g)
p <- length(map)

flankm <- c("rs8262456","rs13479085")
qtlloc <- mean(map[flankm])

w1 <- 0.5
w2 <- (map[p]-map[1])/100*2


###########################################
# Panel A
hcolor <- "black"
hlwd <- 1
myplotgeno(map, g, axislab=c(1,7,8,11))
rect(32-w1,map[7]-w2, 60+w1,map[8]+w2,col=hgrey,border=hcolor,lwd=hlwd)
myplotgeno.eh(map,g, rect=c(32-w1,map[7]-w2, 60+w1,map[8]+w2))
i <- which(gn[,flankm[1]] != qtlgn)
j <- which(gn[,flankm[2]] != qtlgn)
segments(x0=j,y0=map[flankm[1]],y1=qtlloc,col=color[qtlgn[j]],lwd=3, lend=1, ljoin=1)
segments(x0=i,y0=map[flankm[2]],y1=qtlloc,col=color[qtlgn[i]],lwd=3, lend=1, ljoin=1)
addqtl(qtlloc,qtlgn)
addgn(g)
rect(32-w1,map[7]-w2, 60+w1,map[8]+w2,border=hcolor,lwd=hlwd)
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=TRUE)


########################################
# legend
textcolor <- color
textcolor[2] <- "#af5500"
textcolor[1] <- "#4a3aad"
text(u[1]+diff(u[1:2])*c(0.4, 0.5, 0.6), u[4]+diff(u[3:4])*0.08,
     c("BB", "BR", "RR"), col=textcolor, xpd=TRUE, cex=1.8)
points(u[1]+diff(u[1:2])*c(0.4, 0.5, 0.6)-1.5, rep(u[4]+diff(u[3:4])*0.08,3),
     pch=16, col=color, xpd=TRUE, cex=2.1)



########################################
# Data for panel B

uw <- (u[2]-u[1])*0.04
load("../Data/Chr6.genotype.map.2013-11-08.RData")

Mouse <- c("Mouse3182", "Mouse3096", "Mouse3643", "Mouse3619",
           "Mouse3194", "Mouse3287", "Mouse3130", "Mouse3213", "Mouse3206",
           "Mouse3232", "Mouse3421", "Mouse3625", "Mouse3253", "Mouse3616",
           "Mouse3607", "Mouse3414", "Mouse3419", "Mouse3450", "Mouse3460",
           "Mouse3505", "Mouse3054", "Mouse3146", "Mouse3139", "Mouse3630",
           "Mouse3516", "Mouse3542", "Mouse3577", "Mouse3176")
# reorder key recombinant mice by Mouse Number within genotype
Mouse <- Mouse[c(2,1,3,
                 5,4,
                 6,
                 7,8,
                 9,
                 10,
                 11,12,
                 13,15,14,
                 28,
                 16,17,18,
                 19,
                 20,
                 21,23,22,24,
                 25,26,27)]

MouseNum <- substr(Mouse, 6, 10)

batch1 <- c("rs3024135","rs8262456",
            "M142421188","M143213280","M144058889", "rs13479085","rs13479086")

gn <- gn[Mouse,batch1]
qtlgn <- qtlgn[Mouse]
map <- map[batch1]

g <- as.matrix(gn)
n <- nrow(g)
p <- length(map)

m1 <- "rs8262456"
m2 <- "M142421188"
flankm <- c(m1, m2)
qtlloc <- mean(map[flankm])

w1 <- 0.5
w2 <- (map[p]-map[1])/100*2


###########################################
# Panel B
myplotgeno(map,g, axislab=c(1,2,3,7))
rect(1-w1,map[m1]-w2,8+w1,map[m2]+w2,border=hcolor,lwd=hlwd,col=hgrey)

myplotgeno.eh(map,g,rect=c(1-w1,map[m1]-w2,8+w1,map[m2]+w2))
i <- which(gn[,m1] != qtlgn)
j <- which(gn[,m2] != qtlgn)
segments(x0=j,y0=map[m1],y1=qtlloc,col=color[qtlgn[j]],lwd=3, lend=1, ljoin=1)
segments(x0=i,y0=map[m2],y1=qtlloc,col=color[qtlgn[i]],lwd=3, lend=1, ljoin=1)
addqtl(qtlloc,qtlgn)
addgn(g)
rect(1-w1,map[m1]-w2,8+w1,map[m2]+w2,border=hcolor,lwd=hlwd)

u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=TRUE)


######################################################################
# Data for Panel C

load("../Data/Chr6.genotype.map.2013-11-08.RData")

Mouse <- c("Mouse3096", "Mouse3182", "Mouse3619", "Mouse3194", "Mouse3643",
           "Mouse3287", "Mouse3130", "Mouse3213")
MouseNum <- substr(Mouse, 6, 10)

batch2 <- c("rs8262456", "M141684769", "M141874184", "M141913339", "M141970650",
            "M141979254", "M142034947", "M142051511", "M142106246", "M142166806",
            "M142177340", "M142246802", "M142277172", "M142297143", "M142306955",
            "M142421188" )

gn <- gn[Mouse,batch2]
qtlgn <- qtlgn[Mouse]
map <- map[batch2]

g <- as.matrix(gn)
p <- length(map)
n <- nrow(g)

m1 <- "M141970650"
m2 <- "M141979254"
m3 <- "M142246802"
m4 <- "M142277172"

flankm <- c(m1,m4)
qtlloc <- mean(map[flankm]) - 0.01

w1 <- 0.5
w2 <- (map[p]-map[1])/100*2



##############################################################
# Panel C

myplotgeno(map,g, axislab=c(1,5,13,16),
           mar=c(2.6, 9.6, 3.1, 4.1))
u <- par("usr")
rect(u[1], map[m1], u[2], map[m4], col=hgrey)
myplotgeno.eh(map,g, rect=c(u[1], map[m1], u[2], map[m4]))
addqtl(qtlloc + 0.015,qtlgn)
rect(u[1], u[3], u[2], u[4], border=TRUE)


########################################
# data for panel D

data <- read.table("../Data/build37.txt")[,c(1,3,4)]
names(data) <- c("genename","start","end")
data[,2:3] <- data[,2:3]/(1e+6)
genepos <- range(data[,2:3])

##############################
# Panel D (gene locations)
par(col.lab="slateblue", mar=c(2.6, 0.6, 3.1, 4.1))
plot(0,0,type="n", bty="n", xlab="", ylab="", xaxt="n", yaxt="n",mgp=c(0,0,0),
     ylim=range(map)[2:1], xlim=c(1, 10))

segments(2.5, map[1], 2.5, map[p], lwd=2, lend=1, ljoin=1)
segments(2.5, map[m1], 2.5, map[m2], lwd=2,col=xocolor, lend=1, ljoin=1)
segments(2.5, map[m3], 2.5, map[m4],lwd=2,col=xocolor, lend=1, ljoin=1)
for(i in 1:p)
    segments(2, map[i], 3, map[i], lwd=1, lend=1, ljoin=1)

textadj <- c(0, 0.015)
for(i in c(4,5)){
  rect(3.6, data[i,2],4,data[i,3],col="grey80")
  text(3.7, data[i,2]+textadj[i-3], data[i,1],pos=4,cex=1.8,xpd=TRUE, font=3)
}

h <- 4
rect(h+0.6, data[8,2], h+1, data[8,3],col="grey80")
text(h+0.7, data[8,2]+0.015, data[8,1],pos=4,cex=1.8,xpd=TRUE, font=3)

dev.off()


######################################################################

pdf("../Figs/islet_c6_geno_B.pdf", height=6,width=10, pointsize=10)
layout(rbind(c(1,1,1), c(2,3,4)),width=c(8.5,4.5,2))

########################################
# Data for Panel A

load("../Data/Chr6.genotype.map.2013-11-08.RData")


batch0 <- c("rs13479058", "rs13479059", "rs4226339", "rs13479070",
            "rs13479071", "rs3024135", "rs8262456", "rs13479085", "rs13479086",
            "rs3089737", "rs13479087")

Mouse <- c("Mouse3326", "Mouse3478", "Mouse3330", "Mouse3131",
           "Mouse3550", "Mouse3477", "Mouse3125", "Mouse3300", "Mouse3312",
           "Mouse3220", "Mouse3535", "Mouse3373", "Mouse3605", "Mouse3523",
           "Mouse3149", "Mouse3351", "Mouse3119", "Mouse3448", "Mouse3229",
           "Mouse3364", "Mouse3633", "Mouse3275", "Mouse3518", "Mouse3472",
           "Mouse3247", "Mouse3638", "Mouse3587", "Mouse3171", "Mouse3439",
           "Mouse3291", "Mouse3055", "Mouse3096", "Mouse3643", "Mouse3182",
           "Mouse3619", "Mouse3194", "Mouse3176", "Mouse3287", "Mouse3206",
           "Mouse3130", "Mouse3421", "Mouse3625", "Mouse3460", "Mouse3213",
           "Mouse3414", "Mouse3505", "Mouse3054", "Mouse3146", "Mouse3419",
           "Mouse3232", "Mouse3630", "Mouse3450", "Mouse3558", "Mouse3139",
           "Mouse3577", "Mouse3616", "Mouse3607", "Mouse3516", "Mouse3253",
           "Mouse3542", "Mouse3200", "Mouse3487", "Mouse3473", "Mouse3603" )
# reorder key recombinant mice by Mouse Number within genotype
Mouse[32:60] <- Mouse[31+c(1, 3, 2,
                           5, 4, 6,
                           8, 7,
                           9, 13, 10, 12, 11,
                           16, 23, 17, 19, 14, 18, 21, 15, 22, 20,
                           28, 27, 29, 24, 26, 25)]

MouseNum <- substr(Mouse, 6, 10)

lname <- load("../Data/LDA.recomb.RData")
qtlgn <- cl.lda$pred.test
gn <-  genotype.test

load(paste0(data.folder, "input/aligned_geno_with_pmap.RData"))
map <- pmap[["6"]][batch0]

gn <- gn[Mouse,batch0]
qtlgn <- qtlgn[Mouse]
map <- map[batch0]

g <- as.matrix(gn)
n <- nrow(g)
p <- length(map)

flankm <- c("rs8262456","rs13479085")
qtlloc <- mean(map[flankm])

w1 <- 0.5
w2 <- (map[p]-map[1])/100*2


###########################################
# Panel A
hcolor <- "black"
hlwd <- 1
myplotgeno(map, g, axislab=c(1,7,8,11))
rect(32-w1,map[7]-w2, 60+w1,map[8]+w2,col=hgrey,border=hcolor,lwd=hlwd)
myplotgeno.eh(map,g, rect=c(32-w1,map[7]-w2, 60+w1,map[8]+w2))
i <- which(gn[,flankm[1]] != qtlgn)
j <- which(gn[,flankm[2]] != qtlgn)
segments(x0=j,y0=map[flankm[1]],y1=qtlloc,col=color[qtlgn[j]],lwd=3, lend=1, ljoin=1)
segments(x0=i,y0=map[flankm[2]],y1=qtlloc,col=color[qtlgn[i]],lwd=3, lend=1, ljoin=1)
addqtl(qtlloc,qtlgn)
addgn(g)
rect(32-w1,map[7]-w2, 60+w1,map[8]+w2,border=hcolor,lwd=hlwd)
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=TRUE)


########################################
# legend
textcolor <- color
textcolor[2] <- "#af5500"
textcolor[1] <- "#4a3aad"
text(u[1]+diff(u[1:2])*c(0.4, 0.5, 0.6), u[4]+diff(u[3:4])*0.08,
     c("BB", "BR", "RR"), col=textcolor, xpd=TRUE, cex=1.8)
points(u[1]+diff(u[1:2])*c(0.4, 0.5, 0.6)-1.5, rep(u[4]+diff(u[3:4])*0.08,3),
     pch=16, col=color, xpd=TRUE, cex=2.1)


########################################
# Data for panel B

uw <- (u[2]-u[1])*0.04
load("../Data/Chr6.genotype.map.2013-11-08.RData")

Mouse <- c("Mouse3182", "Mouse3096", "Mouse3643", "Mouse3619",
           "Mouse3194", "Mouse3287", "Mouse3130", "Mouse3213", "Mouse3206",
           "Mouse3232", "Mouse3421", "Mouse3625", "Mouse3253", "Mouse3616",
           "Mouse3607", "Mouse3414", "Mouse3419", "Mouse3450", "Mouse3460",
           "Mouse3505", "Mouse3054", "Mouse3146", "Mouse3139", "Mouse3630",
           "Mouse3516", "Mouse3542", "Mouse3577", "Mouse3176")
# reorder key recombinant mice by Mouse Number within genotype
Mouse <- Mouse[c(2,1,3,
                 5,4,
                 6,
                 7,8,
                 9,
                 10,
                 11,12,
                 13,15,14,
                 28,
                 16,17,18,
                 19,
                 20,
                 21,23,22,24,
                 25,26,27)]

MouseNum <- substr(Mouse, 6, 10)

batch1 <- c("rs3024135","rs8262456",
            "M142421188","M143213280","M144058889", "rs13479085","rs13479086")

gn <- gn[Mouse,batch1]
qtlgn <- qtlgn[Mouse]
map <- map[batch1]

g <- as.matrix(gn)
n <- nrow(g)
p <- length(map)

m1 <- "rs8262456"
m2 <- "M142421188"
flankm <- c(m1, m2)
qtlloc <- mean(map[flankm])

w1 <- 0.5
w2 <- (map[p]-map[1])/100*2
rect(u[1], u[3], u[2], u[4], border=TRUE)


###########################################
# Panel B
myplotgeno(map,g, axislab=c(1,2,3,7))
rect(1-w1,map[m1]-w2,8+w1,map[m2]+w2,border=hcolor,lwd=hlwd,col=hgrey)

myplotgeno.eh(map,g,rect=c(1-w1,map[m1]-w2,8+w1,map[m2]+w2))
i <- which(gn[,m1] != qtlgn)
j <- which(gn[,m2] != qtlgn)
segments(x0=j,y0=map[m1],y1=qtlloc,col=color[qtlgn[j]],lwd=3, lend=1, ljoin=1)
segments(x0=i,y0=map[m2],y1=qtlloc,col=color[qtlgn[i]],lwd=3, lend=1, ljoin=1)
addqtl(qtlloc,qtlgn)
addgn(g)
rect(1-w1,map[m1]-w2,8+w1,map[m2]+w2,border=hcolor,lwd=hlwd)

u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=TRUE)

dev.off()

######################################################################

pdf("../Figs/islet_c6_geno_A.pdf", height=6,width=10, pointsize=10)
layout(rbind(c(1,1,1), c(2,3,4)),width=c(8.5,4.5,2))

########################################
# Data for Panel A

load("../Data/Chr6.genotype.map.2013-11-08.RData")


batch0 <- c("rs13479058", "rs13479059", "rs4226339", "rs13479070",
            "rs13479071", "rs3024135", "rs8262456", "rs13479085", "rs13479086",
            "rs3089737", "rs13479087")

Mouse <- c("Mouse3326", "Mouse3478", "Mouse3330", "Mouse3131",
           "Mouse3550", "Mouse3477", "Mouse3125", "Mouse3300", "Mouse3312",
           "Mouse3220", "Mouse3535", "Mouse3373", "Mouse3605", "Mouse3523",
           "Mouse3149", "Mouse3351", "Mouse3119", "Mouse3448", "Mouse3229",
           "Mouse3364", "Mouse3633", "Mouse3275", "Mouse3518", "Mouse3472",
           "Mouse3247", "Mouse3638", "Mouse3587", "Mouse3171", "Mouse3439",
           "Mouse3291", "Mouse3055", "Mouse3096", "Mouse3643", "Mouse3182",
           "Mouse3619", "Mouse3194", "Mouse3176", "Mouse3287", "Mouse3206",
           "Mouse3130", "Mouse3421", "Mouse3625", "Mouse3460", "Mouse3213",
           "Mouse3414", "Mouse3505", "Mouse3054", "Mouse3146", "Mouse3419",
           "Mouse3232", "Mouse3630", "Mouse3450", "Mouse3558", "Mouse3139",
           "Mouse3577", "Mouse3616", "Mouse3607", "Mouse3516", "Mouse3253",
           "Mouse3542", "Mouse3200", "Mouse3487", "Mouse3473", "Mouse3603" )
# reorder key recombinant mice by Mouse Number within genotype
Mouse[32:60] <- Mouse[31+c(1, 3, 2,
                           5, 4, 6,
                           8, 7,
                           9, 13, 10, 12, 11,
                           16, 23, 17, 19, 14, 18, 21, 15, 22, 20,
                           28, 27, 29, 24, 26, 25)]

MouseNum <- substr(Mouse, 6, 10)

lname <- load("../Data/LDA.recomb.RData")
qtlgn <- cl.lda$pred.test
gn <-  genotype.test

load(paste0(data.folder, "input/aligned_geno_with_pmap.RData"))
map <- pmap[["6"]][batch0]

gn <- gn[Mouse,batch0]
qtlgn <- qtlgn[Mouse]
map <- map[batch0]

g <- as.matrix(gn)
n <- nrow(g)
p <- length(map)

flankm <- c("rs8262456","rs13479085")
qtlloc <- mean(map[flankm])

w1 <- 0.5
w2 <- (map[p]-map[1])/100*2


###########################################
# Panel A
hcolor <- "black"
hlwd <- 1
myplotgeno(map, g, axislab=c(1,7,8,11))
rect(32-w1,map[7]-w2, 60+w1,map[8]+w2,col=hgrey,border=hcolor,lwd=hlwd)
myplotgeno.eh(map,g, rect=c(32-w1,map[7]-w2, 60+w1,map[8]+w2))
i <- which(gn[,flankm[1]] != qtlgn)
j <- which(gn[,flankm[2]] != qtlgn)
segments(x0=j,y0=map[flankm[1]],y1=qtlloc,col=color[qtlgn[j]],lwd=3, lend=1, ljoin=1)
segments(x0=i,y0=map[flankm[2]],y1=qtlloc,col=color[qtlgn[i]],lwd=3, lend=1, ljoin=1)
addqtl(qtlloc,qtlgn)
addgn(g)
rect(32-w1,map[7]-w2, 60+w1,map[8]+w2,border=hcolor,lwd=hlwd)
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=TRUE)


########################################
# legend
textcolor <- color
textcolor[2] <- "#af5500"
textcolor[1] <- "#4a3aad"
text(u[1]+diff(u[1:2])*c(0.4, 0.5, 0.6), u[4]+diff(u[3:4])*0.08,
     c("BB", "BR", "RR"), col=textcolor, xpd=TRUE, cex=1.8)
points(u[1]+diff(u[1:2])*c(0.4, 0.5, 0.6)-1.5, rep(u[4]+diff(u[3:4])*0.08,3),
     pch=16, col=color, xpd=TRUE, cex=2.1)



dev.off()
