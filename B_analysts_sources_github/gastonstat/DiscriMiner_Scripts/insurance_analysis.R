##################################################################################
# Description:  Discriminant Analysis of insurance dataset with DiscriMiner
#               DISQUAL: Discriminant Analysis on Qualitative Variables
# Data:         insurance dataset
#               (dataset available in DiscriMiner)
# Source:       Saporta M., Niang N. (2006) Correspondence Analysis and
#               Classification. 
# Copyright:    Copyright (c) 2012, under the Simplified BSD License
#               http://opensource.org/licenses/bsd-license.php
# Author:       Gaston Sanchez
# url:	        www.gastonsanchez.com/discriminer
##################################################################################

# required packages
library(DiscriMiner)
library(FactoMineR)
library(ggplot2)

# load data insurance
data(insurance)

# structure of the data frame
str(insurance)


# ===============================================================================
# Exploratory and Descriptive Analysis
# ===============================================================================

# how many observations per category in each variable
summary(insurance)

# Multiple correspondence analysis using FactoMineR
# (group variable as supplementary)
mc1 = MCA(insurance, quali.sup=1, ncp=11, graph=FALSE)

# create data frames for ggplot
mcdf1 = as.data.frame(mc1$var$coord)
mcdf2 = as.data.frame(mc1$quali.sup$coord)
colnames(mcdf1) = paste("Dim", 1:ncol(mcdf1), sep="")
colnames(mcdf2) = paste("Dim", 1:ncol(mcdf2), sep="")

# MCA plot
ggplot() +
  geom_hline(yintercept=0, colour="gray75", size=1) +
  geom_vline(xintercept=0, colour="gray75", size=1) +
  geom_text(data=mcdf1, aes(x=Dim1, y=Dim2, label=rownames(mcdf1)),
            colour="gray30", alpha=0.8) + 
  geom_text(data=mcdf2, aes(x=Dim1, y=Dim2, label=rownames(mcdf2)),
            colour="turquoise3", size=9, alpha=0.7) + 
            opts(title="Principal Plane of MCA")

# inspect factor coordinates and test values for all categories
round(cbind(mc1$var$coord[,1:3], mc1$var$v.test[,1:3]), 3)
round(cbind(mc1$quali.sup$coord[,1:3], mc1$quali.sup$v.test[,1:3]), 3)


# ===============================================================================
# DISQUAL Analysis with "disqual" function (basic usage)
# =============================================================================== 

# disqual function (with default arguments)
myDisq1 = disqual(insurance[,-1], insurance[,1])

# raw coefficients (discriminant functions)
myDisq1$raw_coefs

# normalized coefficients (range: 0 - 1000)
myDisq1$norm_coefs

# confusion matrix
myDisq1$confusion


# ===============================================================================
# disqual (learn-test validation)
# =============================================================================== 

# create learning-testing sets
set.seed(789)
learn_set = sample(1:1106, 1000)
test_set = (1:1106)[-learn_set]

# disqual with "learntest" validation
myDisq2 = disqual(insurance[,-1], insurance[,1], validation="learntest", 
             learn=learn_set, test=test_set)

# confusion matrix
myDisq2$confusion


# ===============================================================================
# disqual (cross validation)
# =============================================================================== 

## cross-validation
myDisq3 = disqual(insurance[,-1], insurance[,1], validation="crossval")
myDisq3$confusion


# ===============================================================================
# DISQUAL Analysis step-by-step
# =============================================================================== 
# With DiscriMiner we can also perform a DISQUAL analysis in a
# step-by-step procedure

## STEP 1: Perform a MCA on explanatory qualitative variable
# Multiple correspondence analysis using DiscriMiner
mca1 = easyMCA(insurance[,-1])

# inspect eigenvalues
round(mca1$values, 4)

# take a look at MCA components (coordinate factors)
head(mca1$components)

# inspect MCA coefficients
# (used in linear combinations to create MCA components)
mca1$coefficients

# inspect discrimination power of MCA components
dp = discPower(mca1$components, insurance[,1])
round(dp, 4)
# which MCA components can be discarded? 
which(dp$p_value > 0.05)

## STEP 2: Calculate Fisher's linear discriminant function
# within-class covariance matrix
W = getWithin(mca1$components, insurance[,1])

# group means
GM = groupMeans(mca1$components, insurance[,1])
g1 = GM[,1] 
g2 = GM[,2]

# coeffs discriminant functions
Betas = solve(W) %*% (g1 - g2)

# create dummy variable of group variable (binnary coded)
y = rep(0, nrow(insurance))
y[insurance[,1] == "good"] = 1 # good

# correlations and coefficients
CorCoef = data.frame(correlations=cor(mca1$components, y), coefficients=Betas)
round(CorCoef, 4)

## STEP 3: Calculate DISQUAL raw coefficients (manually)
# super-indicator matrix (TDC)
Z = binarize(insurance[,-1])

# scorecard raw coeffs (discarding F2, F4, F7)
raw = mca1$coefficients[,c(1,3,5,6,8:11)] %*% Betas[c(1,3,5,6,8:11)]
raw

## STEP 4: Normalizing coefficients (range: 0-1000)
cats_per_var = sapply(insurance[,-1], nlevels)
fin = cumsum(cats_per_var)
ini = fin - cats_per_var + 1

# num of variables
p = ncol(insurance[,-1])

# transformed coefficients
trans = rep(0, sum(cats_per_var))
for (j in 1:p)
{
  tmp = raw[ini[j]:fin[j]]
  trans[ini[j]:fin[j]] = tmp - min(tmp)
}
trans = trans * (1000/sum(trans))

# scorecard with transformed coefficients
SC = data.frame(RawCoeffs=raw, NormCoeffs=trans)
head(SC)

## STEP 5: Calculate final score
# final score values
Score = Z %*% trans
Sbad = sort(Score[insurance$Claims=="bad"])
Sgood = sort(Score[insurance$Claims=="good"])

## STEP 6: CDF (cummulative distribution function)
# CDF good claims
sg_cdf = numeric(length(Sgood))
for (i in 1:length(Sgood)) {
  sg_cdf[i] = sum(Sgood <= Sgood[i]) / length(Sgood)
}
# CDF good claims
sb_cdf = numeric(length(Sbad))
for (i in 1:length(Sbad)) {
  sb_cdf[i] = sum(Sbad > Sbad[i]) / length(Sbad)
}

# plot CDF of the score for the two groups
par(mar = c(4.5, 4.5, 3, 4))
plot(Sgood, type="n", col="red", xlim=c(0,1000), ylim=c(0,1), 
     axes=FALSE, xlab="Score", ylab="Proportion")
axis(side=2, at=seq(0,1,.10), pos=0, labels=seq(0,100,10), las=2, col="gray70", 0.8)
axis(side=4, at=seq(0,1,.10), pos=1000, labels=seq(0,100,10), las=2, col="gray70", 0.8)
axis(side=1, at=seq(0,1000,200), pos=0, labels=seq(0,1000,200), col="gray70", 0.8)
abline(v=c(400,600), col="gray80", lwd=2.5)
lines(Sgood, sg_cdf, col="steelblue", lwd=3)
lines(Sbad, 1-sb_cdf, col="tomato", lwd=3)
title("CDF of the score for the two groups", cex.main=0.9)
legend(800, .16, legend=c("good", "bad"), col=c("steelblue", "tomato"), 
       lwd=3, pt.cex=0.5, text.col="gray50", box.col="gray50")
