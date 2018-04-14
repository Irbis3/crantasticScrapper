### cluster analysis

setwd("C:/Users/Cait/Dropbox/Research/Social Opportunity/june2016_6rem/csvs/cell counts")
setwd("C:/Users/caitl/Dropbox/Research/Social Opportunity/june2016_6rem/csvs/cell counts")

counts <- read.csv("cfos_counts_all.csv")
realmat <- read.csv("realcormat.csv") ## real removal
fakemat <- read.csv("fakecormat.csv") ## fake removal 

library(lattice)
library(pvclust)
library(ggplot2)
library(viridis)
library(reshape2)
library(cluster)
library(vegan)


### functions to make correlation matrices 

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


### reorder matrix according to hierarchical clustering rules 

reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}



### just alpha removed 

rcormat <- cor(realmat, use = "pairwise.complete.obs")

## hierarchical clustering 

real_dist <- as.dist(1-rcormat)
real_hclust <- hclust(1-real_dist)
plot(real_hclust) ## normal hierarchical clustering
plot(hclust(dist(rcormat))) ## hierarchically clustering raw distance
heatmap(rcormat,
         distfun = function(x) {as.dist(1-cor(x))},
        col = grey(0:1)[c(2,1)],
        main="Adjacency Matrix") ## Adjacency Matrix

#### column and row order (choosing own order that makes conceptual sense)

rowcols <- c("BNST", "LS", "AH", "mPOA", "VMH", "meA", "dlPAG", "vlPAG", "PMd", "PMv", "Cing", "IL", "PrL",
             "Pir", "RC", "CA1", "CA3", "DG", "CortA", "CeA", "BLA", "ARC", "LH", "Aud", "Vis")

order <- as.matrix(rowcols)
rownames(order) <- rowcols
rcormatx <- rcormat[rownames(order),rownames(order),drop=FALSE]


#### make fig 

rordered <- reorder_cormat(rcormat)
upper_tri <- get_upper_tri(rordered)
rmelt <- melt(upper_tri, na.rm = TRUE)
rmelt <- melt(rcormatx, na.rm = TRUE)


### good removed fig 

ggplot(rmelt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_viridis(option = "C", limit = c(-1, 1)) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


ggplot(rmelt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_viridis(option = "C", limit = c(-1, 1)) +
  coord_fixed()+
  newggtheme


#### just alpha remained 

fcormat <- cor(fakemat, use = "pairwise.complete.obs")

## hierarchical clustering 

fake_dist <- as.dist(1-fcormat)
fake_hclust <- hclust(1-fake_dist)
plot(fake_hclust) ## normal hierarchical clustering
plot(hclust(dist(fcormat))) ## hierarchically clustering raw distance
heatmap(rcormat,
        distfun = function(x) {as.dist(1-cor(x))},
        col = grey(0:1)[c(2,1)],
        main="Adjacency Matrix") ## Adjacency Matrix

### reorder to match removed 

rowcols <- c("BNST", "LS", "AH", "mPOA", "VMH", "meA", "dlPAG", "vlPAG", "PMd", "PMv", "Cing", "IL", "PrL",
             "Pir", "RC", "CA1", "CA3", "DG", "CortA", "CeA", "BLA", "ARC", "LH", "Aud", "Vis")

order <- as.matrix(rowcols)
rownames(order) <- rowcols
fcormatx <- fcormat[rownames(order),rownames(order),drop=FALSE]

upper_tri <- get_upper_tri(fcormatx)

meltx <- melt(fcormatx, na.rm = TRUE)

### good remained fig 

ggplot(meltx, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_viridis(option = "C",limit = c(-1, 1)) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


### order remained according to hierarchical clustering 

fordered <- reorder_cormat(fcormat)
upper_tri <- get_upper_tri(fordered)
fmelt <- melt(upper_tri, na.rm = TRUE)

ggplot(fmelt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_viridis(option = "C", limit = c(-1, 1)) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


### significance testing for hierarchical clusters 

rclustsig <- pvclust(realmat, method.dist="cor", method.hclust="average", nboot=1000)
plot(rclustsig)
pvrect(rclustsig, alpha = 0.95)
seplot(rclustsig)
print(rclustsig)


fclustsig <- pvclust(fakemat, method.dist="cor", method.hclust = "average", nboot = 1000)
plot(fclustsig)
pvrect(fclustsig, alpha = 0.95)
