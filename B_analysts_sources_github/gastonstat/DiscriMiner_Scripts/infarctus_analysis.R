##################################################################################
# Description:  Discriminant Analysis of infarctus dataset with DiscriMiner
# Data:         infarctus dataset
#               (dataset available in DiscriMiner)
# Source:       Saporta M. (2006) Probabilites, analyse des donnees et statistique
#               Paris: Editions Technip
# Copyright:    Copyright (c) 2012, under the Simplified BSD License
#               http://opensource.org/licenses/bsd-license.php
# Author:       Gaston Sanchez
# url:	        www.gastonsanchez.com/discriminer
##################################################################################

# required packages
library(DiscriMiner)
library(ggplot2)
library(reshape)

# load data infarctus
data(infarctus)
infa = infarctus

# structure of the data frame
str(infa)


# ===============================================================================
# Exploratory and Descriptive Analysis
# ===============================================================================

# how many observations in each group
table(infa$PRONO)

# correlation matrix
cor(infa[,-8])

# pairs plot
pairs(infa[,-8], col=infa$PRONO, pch=19)

# melting the data frame
# (useful for ggplots)
infa_melt = melt(infa, id.var="PRONO")

# boxplots with ggplot
ggplot(infa_melt, aes(x=PRONO, y=value)) + 
  geom_boxplot(aes(fill=PRONO), colour="gray40") + 
  facet_wrap(~ variable, scale="free_y") + 
  opts(title="Boxplots of Infarctus variables")

# density plots
ggplot(infa_melt, aes(x=value)) + 
  geom_density(aes(colour=PRONO, fill=PRONO), alpha=0.6) + 
  facet_wrap(~ variable, scale="free") + 
  opts(title="Density Plots of Infarctus variables")

# group means
groupMeans(infa[,-8], infa$PRONO)

# group variances
groupVars(infa[,-8], infa$PRONO)

# group std deviations
groupStds(infa[,-8], infa$PRONO)

# group medians
groupMedians(infa[,-8], infa$PRONO)

# group quantile 10%
groupQuants(infa[,-8], infa$PRONO, prob=0.25)

# correlation matrix (global)
cor(infa[,-8])


# ===============================================================================
# Sum of squares and covariances
# ===============================================================================

# sum of squares decomposition
TSS = totalSS(infa[,-8])
BSS = betweenSS(infa[,-8], infa$PRONO)
WSS = withinSS(infa[,-8], infa$PRONO)
# checking that TSS = BSS + WSS
TSS
BSS + WSS


# variance decomposition
Tcov = var(infa[,-8])
Bcov = betweenCov(infa[,-8], infa$PRONO)
Wcov = withinCov(infa[,-8], infa$PRONO)
# Tcov = Bcov + Wcov
Tcov
Bcov + Wcov


# discriminant power
discPower(infa[,-8], infa$PRONO)


# ===============================================================================
# Descriptive Discriminant Analysis (DDA)
# ===============================================================================

# descriptive discriminant analysis
infa_dda = desDA(infa[,-8], infa$PRONO)
# discriminant variables
round(infa_dda$discrivar, 4)


# ===============================================================================
# Geometric Predictive Discriminant Analysis (GDA)
# ===============================================================================

# geometric discriminant analysis
infa_gda = geoDA(infa[,-8], infa$PRONO)
# geometric rules of discrimination
infa_gda$functions

