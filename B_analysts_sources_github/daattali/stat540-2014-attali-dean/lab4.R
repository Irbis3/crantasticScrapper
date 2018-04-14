# Dean Attali
# Jan 29, 2014
# UBC STAT540

# lab 4 - two group comparisons and data aggregation

library(lattice)
library(plyr)
library(ggplot2)

# load the data
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

# extract data for one gene
set.seed(987)
(theGene <- sample(1:nrow(prDat), 1))
pDat <- data.frame(prDes, gExp = unlist(prDat[theGene, ]))
str(pDat)

# explore! what are sample means in wildtype/knockout?
aggregate(gExp ~ gType, pDat, FUN = mean)
ddply(pDat, ~ gType, summarize, gExp = mean(gExp))

# strip plot to sanity test 
stripplot(gType ~ gExp, pDat)
ggplot(pDat, aes(x = gExp, y = gType)) + geom_point()

# t-test comparing wildtype to nrl knockout
ttRes <- t.test(gExp ~ gType, pDat)
str(ttRes)


# create a numeric matrix for the mini dataset (3 genes for the 39 samples)
kDat <- readRDS("GSE4051_MINI.rds")
kMat <- as.matrix(kDat[c('crabHammer', 'eggBomb', 'poisonFang')])
str(kMat)

# median expression for each gene
median(kMat[ , 'eggBomb'])
apply(kMat, 2, median)
apply(kMat, 2, quantile, probs = 0.5)

# find gene with lowest expression per sample
apply(kMat, 1, min)
colnames(kMat)[apply(kMat, 1, which.min)]

rowSums(kMat)
all.equal(rowSums(kMat), apply(kMat, 1, sum))
all.equal(colMeans(kMat), apply(kMat, 2, mean))


# average expression of eggBomb over dev stages
aggregate(eggBomb ~ devStage, kDat, FUN = mean)
ddply(kDat, ~ devStage, summarize, exp = mean(eggBomb))

# aggregate based on combination of factors
aggregate(eggBomb ~ gType * devStage, kDat, FUN = mean)
ddply(kDat, .(gType, devStage), summarize, exp = mean(eggBomb))

# min and max of each genotype/devstage
aggregate(eggBomb ~ gType * devStage, kDat, FUN = range)


## TWO SAMPLE TESTS
# grab 6 genes: 3 interesting, 3 boring
keepGenes <- c("1431708_a_at", "1424336_at", "1454696_at",
               "1416119_at", "1432141_x_at", "1429226_at" )
miniDat <- subset(prDat, rownames(prDat) %in% keepGenes)
miniDat <- data.frame(gExp = as.vector(t(as.matrix(miniDat))),
                      gene = factor(rep(rownames(miniDat), each = ncol(miniDat)),
                                    levels = keepGenes))
miniDat <- data.frame(prDes, miniDat)
str(miniDat)

# plot
stripplot(gType ~ gExp | gene, miniDat,
          scales = list(x = list(relation = "free")),
          group = gType, auto.key = TRUE)
ggplot(miniDat, aes(x = gExp, y = gType, color = gType)) +
  facet_wrap(~ gene, scales="free_x") +
  geom_point(alpha = 0.7) +
  theme(panel.grid.major.x = element_blank())


# simple test: t test of genotypes in just the first gene
someDat <- droplevels(subset(miniDat, gene == keepGenes[1]))
t.test(gExp ~ gType, someDat)

# do a t-test per gene
# use dl because d = input = dataframe, l = output = list (that's the output of t.test)
ttRes <- dlply(miniDat, ~ gene, function(x) t.test(gExp ~ gType, x))
names(ttRes)
ttRes[["1454696_at"]]

# only care about the t statistic and p value?
ttRes <- ddply(miniDat, ~ gene, function(z) {
  zz <- t.test(gExp ~ gType, z)
  round(c(tStat = zz$statistic, pVal = zz$p.value), 4)
})
ttRes