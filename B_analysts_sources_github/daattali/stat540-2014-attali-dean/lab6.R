# Dean Attali
# Feb 13, 2014
# UBC STAT540

# lab 6 - fitting and interpreting linear models in - HIGH VOLUME

source('../common.R')

library(limma)
library(lattice)
library(ggplot2)

# load the data
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")

#### prove how lousy variance estimates can be when the number of samples is small
# generate data for 1000 genes, each gene has 3 observations from a normal(0,1) distribution,
# then look at the observed gene-wise variance
m <- 1000
n <- 3
x <- matrix(rnorm(m * n), nrow = m)
obsVars <- apply(x, 1, var)
summary(obsVars)
densityplot(~obsVars, n = 200)
# true variance is 1, but many many genes have variance less than 1/3 and some hvae over 4!


#### fit a linear model - explain geneexp of wildtype as fx of dev stage
wtDes <- subset(prDes, gType == "wt")
wtDat <- subset(prDat, select = prDes$gType == "wt")

# we accept the default "ref + treatment effects" design matrix for devstage
wtDesMat <- model.matrix(~devStage, wtDes)

# fit the model for all probes
wtFit <- lmFit(wtDat, wtDesMat)
wtEbFit <- eBayes(wtFit)

# which genes show a differential expression? by default, using F test that tests
# if any of the devstage coefficients (except the intercept) are non zero
topTable(wtEbFit)
# whoops, this seems to test whether any of the coeffiients INCLUDING the intercept are non-zero
# (notice the gigantic F statistic). We Want to only care about the non-reference dev stages...
cols <- grep("devStage", colnames(coef(wtEbFit)))
dsHits <- topTable(wtEbFit, coef = cols)

# plot data for 3 of these hits
hitsDat <- prepareData(wtDat, wtDes, row.names(dsHits[c(3,6,9),]))
makeStripplot(hitsDat)
# interesting.. even though we're only looking at wild-type data, we are getting
# "hits" - since there are 30k genes, some of them seem to be affected just by devstage!

#### mastering topTable
# how many probes have adjusted p values for the F test below 1e-5?
hitsList <- topTable(wtEbFit, coef = cols, number = Inf, p.value = 1e-5)
nrow(hitsList)

# scatterplot the P2 vs P10 t statistic for the test that the dev stage effect is zero
# note: need to sort by none so that the order of genes will be kept the same in both
p2 <- topTable(wtEbFit, coef = 'devStageP2', number = Inf, sort.by = "none")
p10 <- topTable(wtEbFit, coef = 'devStageP10', number = Inf, sort.by = "none")
p2vp10t <- data.frame(p2 = p2$t, p10 = p10$t)

ggplot(p2vp10t, aes(p2, p10)) +
  geom_point(alpha=0.1) +
  scale_x_continuous(limits=c(-21,20)) +
  scale_y_continuous(limits=c(-21,20)) +
  coord_fixed() +
  stat_density2d(aes(fill = ..level..), geom="polygon") +
  geom_abline(intercept=0, slope=1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))+
  xlab("t-statistic for P2 effect") + 
  ylab("t-statistic for P10 effect")+
  theme(legend.position = "none")

# densityplot of the associated p values
p2vp10p <- data.frame(p2 = p2$adj.P.Val, p10 = p10$adj.P.Val)
ggplot(p2vp10p) +
  geom_density(aes(x=p2, colour="red")) +
  geom_density(aes(x=p10,colour="blue")) +
  scale_colour_manual(name = "",
                      values = c('blue','red'),
                      labels = c('P10 adj p-val', 'P2 adj p-val')) +
  xlab("p2/p10 adj p-val") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))
# this is not what I expected - it seems like 
  
# weird, I expected P10 dev stage to be more different than the baseline, but
# it seems as though P10 is on average closer than P2


# focus on P10 effect: make scatterplot matrix of raw p values, BH p values and BY p values
p10pvals <- data.frame(raw = topTable(wtEbFit, coef = 'devStageP10', number = Inf, sort.by = "none", adjust.method = "none")$P.Value,
                       bh = topTable(wtEbFit, coef = 'devStageP10', number = Inf, sort.by = "none", adjust.method = "BH")$adj.P.Val,
                       by = topTable(wtEbFit, coef = 'devStageP10', number = Inf, sort.by = "none", adjust.method = "BY")$adj.P.Val)
plotmatrix(p10pvals)

# the raw and BH-adjusted p values are very well correlated, as expected! not sure what BY values are...



#### contrasts

# Let's try to distinguish genes that have stable expression at the last three developmental stages
# (P6, P10, and 4_weeks) from those that do not
conf.matrix <- makeContrasts(P10VsP6 = devStageP10 - devStageP6, fourweeksVsP10 = devStage4_weeks - devStageP10,
                             levels = wtDesMat)
wtFitCont <- contrasts.fit(wtFit, conf.matrix)
wtEbFitCont <- eBayes(wtFitCont)
topTable(wtEbFitCont)

# let's look at hte top 4 genes
top4Hits <- row.names(topTable(wtEbFitCont)[1:4, ])
t4hData <- prepareData(.genes=top4Hits, .dat=wtDat, .des=wtDes)
makeStripplot(t4hData)

# looks good - there is indeed a difference between P10 and 4weeks (but not between P6 to P10 in these cases)

# now let's try to find probes that show a change in both P6 -> P10 and P10 -> 4weeks, but in opposite directions
cutoff <- 1e-02
wtResCont <- decideTests(wtEbFitCont, p.value = cutoff, method = "global")
summary(wtResCont)

# let's see a Venn diagram for this
vennDiagram(wtResCont)
# this shows us there are 10 genes that are found in both time frame sets, BUT it doesn't say whether
# any of these genes are down/up or down/down or up/up. Just says 10 of them have nonzero change

# which probes were down regulatd from P6 to P10?
down1 <- names(which(wtResCont[ , "P10VsP6"] < 0))
makeStripplot(prepareData(wtDat, wtDes, down1[1:4]))

# which were upregulated in that timeframe?
up1 <- names(which(wtResCont[ , "P10VsP6"] > 0))
makeStripplot(prepareData(wtDat, wtDes, up1[1:4]))

# get the same hits but for P10 -> 4weeks
down2 <- names(which(wtResCont[ , "fourweeksVsP10"] < 0))
up2 <- names(which(wtResCont[ , "fourweeksVsP10"] > 0))

# now see if there are any probes that went down and then up, or up and then down
intersect(down1, up2)
intersect(up1, down2)

# BINGO! let's see it :)
makeStripplot(prepareData(wtDat, wtDes,
                          names(which(wtResCont[ , "P10VsP6"] > 0 & wtResCont[ , "fourweeksVsP10"] < 0))))
