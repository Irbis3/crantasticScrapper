########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sat Jan 23 21:10:55 2010
########################################################
#1. Bar chart of the homicide rate in 2008
#2. Map of the homicide rate
#3. Bar chart of the difference in homicide rate 2008-2006
#4. Map of the same
#5. Small multiples of the evolution of the murder rate 1990-2008

source("library/utilities.r")

#############################################
#Constants
kyears.start <- 1990
kyear.zoom <- 1994 #The data before 1994 is iffy
kyears <- kyears.start:2008
#############################################3


cleanHom <- function(type="Total") {
  hom <- read.csv(bzfile("states/data/homicide-mun-2008.csv.bz2"), skip=4)
  names(hom)[1:4] <- c("Code", "County", "Year.of.Murder", "Sex")
  hom$County <- iconv(hom$County, "windows-1252", "utf-8")
  hom$Code <- iconv(hom$Code, "windows-1252", "utf-8")
  hom <- hom[grep("=CONCATENAR", hom$Code),]
  hom <- hom[-grep("Extranjero", hom$County),]
  hom <- hom[grep(type, hom$Sex),]
  hom$Year.of.Murder <- as.numeric(as.numeric(gsub('[[:alpha:]]', '',
                                 hom$Year.of.Murder)))
  hom <- subset(hom, Year.of.Murder >= kyears.start)
  #Get rid of the commas in the numbers: 155,000 to 155000
  col2cvt <- 5:ncol(hom)
  hom[ ,col2cvt] <- lapply(hom[ ,col2cvt],
                          function(x){as.numeric(gsub(",", "", x))})
  hom[is.na(hom)] <- 0
  hom$Tot <- apply(hom[ , col2cvt], 1, sum)
  hom$Code <- as.numeric(gsub(".*,([[:digit:]]+).", "\\1", hom$Code))
  hom
}

cleanPop <- function(type = "Total") {
  if(type == "Mujer")
    pop <- read.csv("conapo-pop-estimates/conapo-states-f.csv")
  else
    pop <- read.csv("conapo-pop-estimates/conapo-states.csv")
  pop$Code <- c(1:33)
  pop$State <- iconv(pop$State, "windows-1252", "utf-8")
  pop
}

mergeHomPopYear <- function(hom, pop, year = 2008) {
  hom2008 <- merge(subset(hom, Year.of.Murder == year),
                   pop[ ,c(1,year-kyears.start+2,ncol(pop))],
                   by="Code", all.x = TRUE)
  names(hom2008)[ncol(hom) + 2] <- "popyear"
  #The per 100,000  murder rate
  hom2008$Rate <- hom2008$Tot / hom2008$popyear * 100000
  hom2008$County <- cleanNames(hom2008)
  hom2008$County <- factor(hom2008$County)
  hom2008
}

redScale <- function(rate) {
  #the second highest value (Sinaloa)
  obs <- round(range(rate)[2]) + 1
  if(obs > 70) {
    obs <- round(-sort(-rate)[2]) + 1
  }
  index <- round(rate) + 1
  clr.inc <- colorRampPalette(brewer.pal(8, "Reds"))
  vec <- clr.inc(obs)[index]
  #special color for Chihuahua since it resembles a war zone,
  #otherwise the colors for the rest of the country would be
  #too light
  vec[rate > 70] <- "#410101"
  vec
}

barPlot <- function(hom2008, year="") {
  hom2008$color <- redScale(hom2008$Rate)
  hom2008$County <- reorder(hom2008$County, hom2008$Rate)
  xmax <- range(hom2008$Rate)[2] + 5
  hom.mean <- wtd.mean(hom2008$Rate, hom2008$popyear)
  ggplot(data = hom2008, aes(County, Rate)) +
    geom_bar(stat = "identity", aes(fill = color)) +
    scale_y_continuous(limits = c(0, xmax)) +
    coord_flip() +
    labs(x = "", y = "Homicides per 100,000") +
    opts(title = paste(config$title.barplot, year)) +
    opts(legend.position = "none") +
    scale_fill_identity(aes(breaks = color)) +
    geom_text(aes(label=round(Rate, digits = 1)), hjust = -.05,
            color = "gray50") +
    geom_hline(yintercept = hom.mean, alpha=.1, linetype=2)
}

#We need to order the variables by name to match them with the map
mapOrder <- function(df, varname = "County.x"){
  #df2 <- df
  df$County <- cleanNames(df, varname)
  #df$County <- iconv(df$County, "windows-1252", "utf-8")
  df$Code <- pmatch(df$County, (iconv(mexico.shp$NAME_1,"windows-1252","utf-8")))
  df.merge <- merge(data.frame(iconv(mexico.shp$NAME_1,"windows-1252","utf-8"), Code = 1:32),
                    df, by="Code", all.x = TRUE)
  df.merge
}

plotMap <- function(mexico.shp, colors, plotclr, legend="", title="") {
  plot(mexico.shp, col = colors, border="black", lwd=2)
  title(main = title)
  if (legend !="") {
    legend("topright", legend = legend,
      fill = plotclr, cex=0.8, bty="n")
  }
  par(bg = "white")
}

#return a data frame with the change in homicide rates
getDiff <- function(hom, pop, year1, year2) {
  if(year1>year2){
        temp <- year2
        year2 <- year1
        year1 <- temp
     }
  hom2008 <- merge(subset(hom, Year.of.Murder == year2),
                   pop[ ,c(1,year2-kyears.start+2,ncol(pop))],
                   by="Code", all.x=T)
  names(hom2008)[ncol(hom)+2] <- "popyear2"
  hom2008$Rate2008 <- hom2008$Tot / hom2008[[ncol(hom)+2]] * 100000
  hom2006 <- merge(subset(hom, Year.of.Murder == year1),
                   pop[ ,c(1,year1-kyears.start+2,ncol(pop))],
                   by="Code", all.x=T)
  names(hom2006)[ncol(hom)+2] <- "popyear1"
  hom2006$Rate2006 <- hom2006$Tot / hom2006[[ncol(hom)+2]] * 100000
  hom.diff <- merge(hom2008,hom2006, by ="Code")
  hom.diff$Diff <- hom.diff$Rate2008 - hom.diff$Rate2006
  hom.diff$County.x <- factor(hom.diff$County.x)
  hom.diff
}

#orange-blue scale for the difference barplot
greenReds <- function(difference){
  clr.inc <- colorRampPalette(brewer.pal(5, "Oranges"))
  clr.dec <- colorRampPalette(brewer.pal(5, "Blues"))
  #I (heart) R
  colors <- difference
  obs <- abs(round(range(difference)[2])) + 1
  index <- abs(round(difference[difference >= 0])) + 1
  colors[difference >= 0] <- clr.inc(obs)[index]

  index <- abs(round(difference[difference < 0])) + 1
  colors[difference < 0] <- clr.dec(obs)[index]

  colors
}

#Based on code from:
#http://learnr.wordpress.com/2009/06/01/ggplot2-positioning-of-barplot-category-labels/
barDiff <- function(hom.diff, year1="", year2="") {
  hom.diff$color <- greenReds(hom.diff$Diff)
  hom.diff$hjust <- ifelse(hom.diff$Diff > 0, 1.1, -.1)
  hom.diff$text.pos <- ifelse(hom.diff$Diff > 0, -.05, 1)
  hom.diff$County.x <- cleanNames(hom.diff, "County.x")
  hom.diff$County.x <- factor(hom.diff$County.x)
  hom.diff$County.x <- reorder(hom.diff$County.x, hom.diff$Diff)
  xmin <- range(hom.diff$Diff)[1] - 2
  xmax <- range(hom.diff$Diff)[2] + 2
  hom.mean08 <- wtd.mean(hom.diff$Rate2008, hom.diff$popyear2)
  hom.mean06 <- wtd.mean(hom.diff$Rate2006, hom.diff$popyear1)
  ggplot(hom.diff, aes(x=County.x, y=Diff, label=County.x,
                        hjust = hjust)) +
    geom_text(aes(y = 0, size=3)) +
    geom_bar(stat = "identity",aes(fill = color)) +
    scale_y_continuous(limits = c(xmin, xmax)) +
    coord_flip() +
    labs(x = "", y = "Change in Rate per 100,000") +
    scale_x_discrete(breaks = NA) +
    opts(legend.position = "none") +
    opts(title = paste(config$title.bardiff,
                       year1, "-", year2, ")", sep = "")) +
    scale_fill_identity(aes(breaks = color)) +
    geom_text(aes(label=round(Diff, digits = 1), hjust = text.pos),
              color="gray50") +
    geom_hline(yintercept = hom.mean08 - hom.mean06, alpha=.1,
               linetype=2)
}

####################################################
#Small Multiples Plot of Murders by State
####################################################
totalHom <- function(hom){
  total.hom <- ddply(hom, .(Year.of.Murder), function(df) sum(df$Tot))
  total.hom$pop <- unlist(pop[33, 2:(ncol(pop)-1)])
  total.hom$Rate <- (total.hom$V1 / total.hom$pop) * 100000
  total.hom
}

mergeHomPopS <- function(hom){
  mpop <- melt(subset(pop, State != "Nacional"), id=c("Code", "State"))
  mpop$variable <- as.numeric(substring(mpop$variable, 2))
  mpop$Year.of.Murder <- mpop$variable
  hom.mpop <- merge(hom, mpop, by=c("Code","Year.of.Murder"),
                    all.y = TRUE)
  if(any(is.na(hom.mpop$Tot)))
    hom.mpop[is.na(hom.mpop$Tot), ]$Tot <- 0
  hom.mpop$Rate <- hom.mpop$Tot / hom.mpop$value * 100000
  hom.mpop$State <- cleanNames(hom.mpop, "State")
  hom.mpop$State <- factor(hom.mpop$State)
  hom.mpop
}

cluster <- function(hom.mpop, nclusters){
  #k-means clustering to order the facets
  t <- cast(hom.mpop[,c(26:27,29)], State ~ variable, value = "Rate")
  t[is.na(t)] <- 0
  cl <- kmeans(t[,2:ncol(t)], nclusters)
  t$Cluster <- cl$cluster
  t <- merge(ddply(t, .(Cluster), function(df) mean(df$"2008")),
        t, by = "Cluster")
  t <- t[,c(1,2,3)]
  hom.mpop <- merge(hom.mpop, t, by = "State")
  hom.mpop <- ddply(hom.mpop, .(State), transform,
                    max = Rate[length(Rate)])
  hom.mpop$order <- hom.mpop$V1^3 + hom.mpop$max
  hom.mpop$State <- reorder(hom.mpop$State, -hom.mpop$order)
  hom.mpop
}

smallMultiples <- function(hom, pop, nclusters = 8){
  total.hom <- totalHom(hom)
  hom.mpop <- mergeHomPopS(hom)
  hom.mpop <- cluster(hom.mpop, nclusters)
  p <- ggplot(hom.mpop, aes(Year.of.Murder, Rate)) +
      geom_line(aes(color = factor(Cluster)), size = 1)
  p + facet_wrap(~ State, as.table = TRUE,
                 scale="free_y") +
      labs(x = "", y = "Homicide Rate") +
      opts(title = config$title.sm) +
      scale_x_continuous(breaks = seq(kyears.start, 2008, by = 4)) +
      theme_bw() +
      geom_line(data = total.hom, aes(Year.of.Murder, Rate),
                color="gray70", linetype = 2, size =.5) +
      opts(legend.position = "none") +
      opts(axis.text.x=theme_text(angle=60, hjust=1.2 )) +
      coord_cartesian(xlim = c(kyear.zoom, 2008))

}


if(config$sex == "Female") {
  type <- "Mujer"
  config$title.sm <- config$states$ftitle.sm
  config$title.bardiff <- config$states$ftitle.bardiff
  config$title.barplot <- config$states$ftitle.barplot
  nclust <- 4
} else {
  type <- "Total"
  config$title.sm <- config$states$mtitle.sm
  config$title.bardiff <- config$states$mtitle.bardiff
  config$title.barplot <- config$states$mtitle.barplot
  nclust <- 8
}

##########################################################
#Read the data
##########################################################
hom <- cleanHom(type)
pop <- cleanPop(type)

########################################################
#Barplot with the homicide rate in 2008
########################################################
year <- config$states$year
hom.year <- mergeHomPopYear(hom, pop, year)
print(barPlot(hom.year, year))
dev.print(png, file = "states/output/2008-homicide-bars.png",
          width = 480, height = 480)

########################################################
#Map with the homicide rate in 2008
########################################################
load("maps/map_mx.RData") #load mexico.shp

hom.year.map <- mapOrder(hom.year, "County")

Cairo(file="states/output/2008-homicide-map.png", width=480, height=480)
print(plotMap(mexico.shp, redScale(hom.year.map$Rate)))
dev.off()

###################################################################
#Bar plot of the change in homicide rate from the start of the
#drug war at the end of 2006 till 2008
###################################################################
year1 <- config$states$year1
year2 <- config$states$year2
hom.diff <- getDiff(hom, pop, year1, year2)
print(barDiff(hom.diff, year1, year2))
dev.print(png, file="states/output/2006-2008-change-homicide.png",
          width=480, height=480)

########################################################
#Map of the change in homicide rates
########################################################
hom.diff.map <- mapOrder(hom.diff, "County.x")
Cairo(file="states/output/2006-2008-change-homicide-map.png", width=480, height=480)
print(plotMap(mexico.shp, greenReds(hom.diff.map$Diff)))
dev.off()

########################################################
#Small Multiples of each state
########################################################
#This is how you get anti-aliasing in R
Cairo(file="states/output/homicide-small-multiples.png",
      type="png", width=960, height=600)
print(smallMultiples(hom, pop, nclust))
dev.off()

#The graph for Chihuahua looks similar to the hockey stick
#of global temperatures
#Coincidence? or is global warming to blame

