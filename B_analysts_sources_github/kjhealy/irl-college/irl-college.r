###--------------------------------------------------
### Irish school transition data
### Original spreadsheet from the Irish Times
### https://www.irishtimes.com/polopoly_fs/1.2451889.1449072744!/menu/standard/file/Schools%20Table.xlsx
###--------------------------------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(networkD3)
library(car)
library(corrplot)
library(ape)


## credit() and makeFootnote() functions below can be found here:
## https://github.com/kjhealy/kjhutils/blob/master/R/utility.R

## Make a "figures" subdirectory in the working directory if one
## doesn't exist already
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)


###--------------------------------------------------
### Setup
###--------------------------------------------------

## Read in school-level data
data.all <- read.csv("data/leaving-data-utf8.csv")

## Strip out some summary columns
ind <- colnames(data.all) %nin% c("Sat2015", "Uni2015", "Progression")
data <- data.all[,ind]

## Convert to long format
data.m <- gather(data, Institution, Students, UCD:SCHM)

## Aggregate to Institution totals by County
data.co <- data.m %>% group_by(County, Institution) %>%
    summarise(Students = sum(Students))

## Quick look at Institution and County totals
data.co %>% group_by(Institution) %>%
    summarize(N = sum(Students)) %>%
    arrange(desc(N)) %>%
    data.frame()

data.co %>% group_by(County) %>%
    summarize(N = sum(Students)) %>%
    arrange(desc(N)) %>%
    data.frame()


###--------------------------------------------------
### Colleges by relative area profile
###--------------------------------------------------

## Wide format for correlation plots and clustering
data.w <- spread(data.co, Institution, Students)
data.w <- data.frame(data.w)
rownames(data.w) <- data.w$Count
data.w <- data.w[, -1]

data.college.pct <- data.co %>% group_by(County) %>%
    mutate(Students = round(Students/sum(Students)*100, 2))
data.college.pct <- spread(data.college.pct, Institution, Students)
data.college.pct <- data.frame(data.college.pct)
rownames(data.college.pct) <- data.college.pct$County
data.college.pct <- data.college.pct[, -1]

### Correlation heatmap of institutions
pdf(file="figures/corplot-by-college-pct.pdf", width = 7.25, height = 7)
corrplot(cor(data.college.pct),
         method = "shade",
         order = "AOE",
#         hclust.method = "ward",
         type= "lower",
         tl.col = "gray30",
         tl.cex = 0.7,
         tl.srt = 45)
dev.off()


###--------------------------------------------------
### Areas by relative college profiles
###--------------------------------------------------

data.county.pct <- data.co %>% group_by(Institution) %>%
    mutate(Students = round(Students/sum(Students)*100, 2))
data.county.pct <- spread(data.county.pct, Institution, Students)
data.county.pct <- data.frame(data.county.pct)
rownames(data.county.pct) <- data.county.pct$County
data.county.pct <- data.county.pct[, -1]
data.county.pct <- t(data.county.pct)


### Correlation heatmap of counties
pdf(file="figures/corplot-by-county-pct.pdf", width = 7.25, height = 7)
corrplot(cor(data.county.pct),
         method = "shade",
         order = "AOE",
#         hclust.method = "ward",
         type= "lower",
         tl.col = "gray30",
         tl.cex = 0.7,
         tl.srt = 45)
dev.off()


### --------------------------------------------------
### Cluster plots
###--------------------------------------------------

cb.pal <- rev(c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00",
                    "#CC79A7"))

### Areas clustered by relative college profile
hc <- hclust(dist(data.college.pct), "ward.D2")
hcp <- as.phylo(hc)
clus5 <- cutree(hc, 4)

pdf(file="figures/area-cluster-radial-plot.pdf", width=10, height=10, pointsize=12)
par(mar=c(0.5,0.5,0.5,0.5), mai=c(0,0,0.5,0), mgp=c(0,0,0))
plot(hcp,
     type = "fan",
     tip.color = cb.pal[clus5], label.offset=0.1,
     edge.width=1.7, edge.color="#999999", font=2, rotate.tree=-85, cex=1.5)
title("\n Geographical Areas Clustered by Relative Similarity of College Destinations")
credit()
dev.off()


### Colleges clustered by relative area profile
hc <- hclust(dist(data.county.pct), "ward.D2")
hcp <- as.phylo(hc)
clus5 <- cutree(hc, 5)

pdf(file="figures/college-cluster-radial-plot.pdf", width=10, height=10, pointsize=12)
par(mar=c(0.5,0.5,0.5,0.5), mai=c(0,0,0.5,0), mgp=c(0,0,0))
plot(hcp,
     type = "fan",
     tip.color = cb.pal[clus5], label.offset=0.1,
     edge.width=1.7, edge.color="#999999", font=2, rotate.tree=-85, cex=1.5)
title("\nColleges Clustered by Relative Similarity of Student County of Origin")
credit()
dev.off()


###--------------------------------------------------
### Sankey plots of student flows, using networkD3
###--------------------------------------------------

data.d3 <- data.co

## Combine Tipperary in to one county
data.d3$County <- recode(data.d3$County, "c('Tipperary N.R.', 'Tipperary S.R.') = 'Tipperary'")


### Convenience functions.

### Produce a string that car's recode function can use
### See car documentation for the kind of string it needs
make.recode <- function(x){
    i <- length(x)
    paste(paste("'", x[1:i-1], "',", collapse="", sep=""), "'", x[i], "'", sep="")
}

### Take the original data, and a vector of county/area names, and a cutoff
### value. Return a vector of institution names whose enrollments fall
### below the cutoff for the given vector of counties/areas.
### We'll use this to create an "Other" category for the Sankey plot
get.others <- function(data=data.s3, sub=munster, cutoff=30) {
    data <- data[sub, ]
    out <- data %>% group_by(Institution) %>%
        summarize(N=sum(Students)) %>%
        arrange(desc(N)) %>%
        filter(N < cutoff)
    others <- as.character(out$Institution)
    return(others)
}

### Take the main data, subset it to a particular group of counties/areas,
### and recode a given vector of institutions to "Other". Then draw
### a Sankey plot for that subset of areas and institutions
sankey.nd3 <- function(data=data.d3,
                       subs=rep(TRUE, nrow(data)),
                       to.other=munster.other) {
    require(networkD3)
    require(car)
    require(dplyr)

    ## Subset rows
    data <- data[subs,]
    colnames(data) <- c("source", "target", "value")

    ## Recode some institutions to "Other" category
    ## Ugly nested paste because of the format car's recode() uses
    data$target <- recode(data$target, paste("c(", make.recode(to.other), ") = 'Other'", sep=""))

    ## Remove duplicate rows
    data <- data %>%
        group_by(source, target) %>%
        mutate(value = sum(value)) %>%
        distinct(source, target)

    data$source <- droplevels(data$source)
    data$target <- droplevels(data$target)

    ## Set up node labels -- sankeyNetwork() wants a vector of
    ## all source and target labels, so we concatenate them
    node.names <- c(levels(data$source), levels(data$target))
    node.names <- data.frame(node.names)
    colnames(node.names) <- "name"

    ## sankeyNetwork() wants source and target data as unique numbers
    ## rather than as factors. We coerce the source from factor
    ## to numeric values, and then subtract 1 because javascript
    ## indexing starts from 0. Then we coerce the target factor
    ## and make sure the numbering starts from one more than the
    ## maximum numeric value of the source vector.
    data$source <- as.numeric(data$source) - 1
    data$target <- as.numeric(data$target) + max(data$source)

    sankeyNetwork(Links = data, Nodes = node.names, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             units = "Students", fontSize = 12, nodeWidth = 40)
}

### Provincal memberships in the main data frame
munster <- str_detect(data.d3$County, "Cork|Kerry|Limerick|Clare|Tipperary|Waterford")
connacht <- str_detect(data.d3$County, "Galway|Leitrim|Mayo|Roscommon|Sligo")
leinster <- str_detect(data.d3$County, "Carlow|Kildare|Kilkenny|Laois|Longford|Louth|Meath|Offaly|Westmeath|Wexford|Wicklow")
ulster <- str_detect(data.d3$County, "Donegal|Cavan|Monaghan")
dublin <- str_detect(data.d3$County, "Dublin")

### Get a vector of the low-enrollment schools particular to each
### province or area.
munster.other <- get.others(data=data.d3, sub=munster, cutoff=60)
connacht.other <- get.others(data=data.d3, sub=connacht, cutoff=60)
leinster.other <- get.others(data=data.d3, sub=leinster, cutoff=300)
dublin.other <- get.others(data=data.d3, sub=dublin, cutoff=200)

### Produce Sankey flow diagrams. Subset the rows by subs, and
### recoding institutions in to.other to "Other". Then save
### the picture as an HTML file. It's not self-contained because
### I have the js libraries on the web server. If you omit
### the saveNetwork step the plot will render in a new browser window
sankey.nd3(data.d3, subs=munster, to.other=munster.other) %>% saveNetwork("munster.html", selfcontained = FALSE)
sankey.nd3(data.d3, subs=connacht, to.other=connacht.other) %>% saveNetwork("connacht.html", selfcontained = FALSE)
sankey.nd3(data.d3, subs=leinster, to.other=leinster.other) %>% saveNetwork("leinster.html", selfcontained = FALSE)
sankey.nd3(data.d3, subs=dublin, to.other=dublin.other) %>% saveNetwork("dublin.html", selfcontained = FALSE)
