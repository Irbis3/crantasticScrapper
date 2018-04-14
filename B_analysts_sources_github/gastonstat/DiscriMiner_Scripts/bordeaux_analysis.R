##################################################################################
# Description:  Discriminant Analysis of bordeaux dataset with DiscriMiner
# Data:         bordeaux dataset
#               (dataset available in DiscriMiner)
# Source:       Tenenhaus M. (2007) Statistique. Paris: Dunod
# Copyright:    Copyright (c) 2012, under the Simplified BSD License
#               http://opensource.org/licenses/bsd-license.php
# Author:       Gaston Sanchez
# url:	        www.gastonsanchez.com/discriminer
##################################################################################

# required packages
library(DiscriMiner)
library(ggplot2)
library(reshape)

# load data bordeaux
data(bordeaux)

# structure of the data frame
str(bordeaux)


# ===============================================================================
# Exploratory and Descriptive Analysis
# ===============================================================================

# how many observations in each group
table(bordeaux$quality)

# pairs plot
pairs(bordeaux[,2:5], col=bordeaux$quality, pch=19)

# melting the data frame (useful for ggplots)
bordeaux_melt = melt(bordeaux[,-1], id.var="quality")

# boxplots with ggplot
ggplot(bordeaux_melt, aes(x=quality, y=value)) + 
  geom_boxplot(aes(fill=quality), colour="gray40") + 
  facet_wrap(~ variable, scale="free_y") + 
  opts(title="Boxplots of Wines variables")

# density curves with ggplot
ggplot(bordeaux_melt, aes(x=value)) + 
  geom_density(aes(colour=quality, fill=quality), alpha=0.6) + 
  facet_wrap(~ variable, scale="free") + 
  opts(title="Density Plots of Wines variables")

# group means
groupMeans(bordeaux[,2:5], bordeaux[,6])

# group variances
groupVars(bordeaux[,2:5], bordeaux[,6])

# group std deviations
groupStds(bordeaux[,2:5], bordeaux[,6])

# group medians
groupMedians(bordeaux[,2:5], bordeaux[,6])

# group quantile 10%
groupQuants(bordeaux[,2:5], bordeaux[,6], 0.10)

# correlation matrix (global)
cor(bordeaux[,2:5])

# correlation matrix of good wines
cor(bordeaux[bordeaux$quality=="good",2:5])

# correlation matrix of medium wines
cor(bordeaux[bordeaux$quality=="medium",2:5])

# correlation matrix of bad wines
cor(bordeaux[bordeaux$quality=="bad",2:5])


# ===============================================================================
# Sum of squares and covariances
# ===============================================================================

# sum of squares decomposition
TSS = totalSS(bordeaux[,2:5])
BSS = betweenSS(bordeaux[,2:5], bordeaux[,6])
WSS = withinSS(bordeaux[,2:5], bordeaux[,6])
# checking that TSS = BSS + WSS
TSS
BSS + WSS


# variance decomposition
Tcov = var(bordeaux[,2:5])
Bcov = betweenCov(bordeaux[,2:5], bordeaux[,6])
Wcov = withinCov(bordeaux[,2:5], bordeaux[,6])
# Tcov = Bcov + Wcov
Tcov
Bcov + Wcov


# discriminant power of variables
discPower(bordeaux[,2:5], bordeaux$quality)

# another way to obtain the correlation ratios (manually)
B = betweenCov(bordeaux[,2:5], bordeaux[,6])
V = totalCov(bordeaux[,2:5])
round(diag(B) / diag(V), 3)

# correlation ratio between temperature and quality
corRatio(bordeaux$temperature, bordeaux$quality)

# correlation ratio between temperature and quality
FRatio(bordeaux$temperature, bordeaux$quality)


# ===============================================================================
# Descriptive Discriminant Analysis (DDA)
# ===============================================================================
# DDA aims to study the relations  between the
# grouping variable and the explanatory variables 

# running a descriptive discriminant analysis
d1 = desDA(bordeaux[,2:5], bordeaux$quality)
d1

# discriminant power
d1$power

# eigenvalues
round(d1$values, 4)

# discriminant variables
d1$discrivar

# data frame to plot scores (i.e. factor coordinates)
wines_df = data.frame(year=bordeaux$year, d1$scores, quality=bordeaux$quality)
# plot scores
ggplot(data=wines_df, aes(x=z1, y=z2, colour=quality)) + 
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(label=year), size=4) +
  labs(x="Score 1", y="Score 2") +
  opts(title="Discriminant Map - Bordeaux Wines (years)")


# ===============================================================================
# Linear Discriminant Analysis (LDA)
# ===============================================================================
# LDA aims to study the relations  between the
# grouping variable and the explanatory variables 

# linear discriminant analysis
bord_da1 = linDA(bordeaux[,2:5], bordeaux$quality)
bord_da1

# linear discriminant analysis with equal priors
bord_da2 = linDA(bordeaux[,2:5], bordeaux$quality, prior=c(1/3,1/3,1/3))
bord_da2

# Classify wine from year 1958
# wine from 1958: (3000, 1100, 20, 300)
bor58 = c(3000, 1100, 20, 300)
# classify it
lin58 = classify(bord_da2, bor58)
lin58

