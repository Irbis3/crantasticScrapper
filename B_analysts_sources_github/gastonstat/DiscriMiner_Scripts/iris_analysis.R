##################################################################################
# Description:  Discriminant Analysis of iris dataset with DiscriMiner
# Data:         iris dataset
#               (iris comes in R by default)
# Copyright:	  Copyright (c) 2012, under the Simplified BSD License
# Author:	      Gaston Sanchez
# url:	        www.gastonsanchez.com/discriminer
##################################################################################

# load required packages
library(DiscriMiner)
library(ggplot2)
library(reshape)

# load dataset
data(iris)


# ===============================================================================
# Exploratory analysis
# ===============================================================================

# how many observations in each group
table(iris$Species)

# since we only have 4 variables,
# we can start with some exploratory visualizations
plotmatrix(iris[,1:4], colour="gray20") +
  geom_smooth(method="lm")

# another option
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}

# expand iris data frame for pairs plot
gg1 = makePairs(iris[,-5])

# new data frame mega iris
mega_iris = data.frame(gg1$all, Species=rep(iris$Species, length=nrow(gg1$all)))

# pairs plot
ggplot(mega_iris, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point(aes(colour=Species), na.rm = TRUE, alpha=0.8) + 
  stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
               data = gg1$densities, position = "identity", colour = "grey20", geom = "line")

# melting the data frame
# (useful for ggplots)
iris_melt = melt(iris, id.var="Species")

# boxplots with ggplot
ggplot(iris_melt, aes(x=Species, y=value)) + 
  geom_boxplot(aes(fill=Species), colour="gray40") + 
  facet_wrap(~ variable, scale="free_y") + 
  opts(title="Boxplots of Iris variables")

# densities
ggplot(iris_melt, aes(x=value)) + 
  geom_density(aes(colour=Species, fill=Species), alpha=0.6) + 
  facet_wrap(~ variable, scale="free_y") + 
  opts(title="Density Plots of Iris variables")

# scatter plots with curve densities
ggplot(data=iris, aes(x=Petal.Width, y=Petal.Length)) + 
  geom_density2d(aes(group=Species), colour="gray70") + 
  geom_point(aes(colour=Species)) + 
  opts(title="Scatter diagram with Density contour lines")


# ===============================================================================
# Descriptive analysis
# ===============================================================================

## Group Summary Statistics
# group means
groupMeans(iris[,1:4], iris[,5])

# group variances
groupVars(iris[,1:4], iris[,5])

# group std deviations
groupStds(iris[,1:4], iris[,5])

# group medians
groupMedians(iris[,1:4], iris[,5])

# group quantile 10%
groupQuants(iris[,1:4], iris[,5], 0.10)

## discriminant power of variables
# Wilk's Lambda (WL)
# WL varies from 0 to 1
# the lower the value, the better the model
discPower(iris[,1:4], iris[,5])

# or alternatively for single variables
corRatio(iris[,1], iris[,5])
FRatio(iris[,1], iris[,5])

## Correlation Matrices
# correlation matrix (global)
cor(iris[,1:4])

# correlation matrix of setosa
cor(iris[iris$Species=="setosa",1:4])

# correlation matrix of versicolor
cor(iris[iris$Species=="versicolor",1:4])

# correlation matrix of virginica
cor(iris[iris$Species=="virginica",1:4])

## Scatter matrices (ie dispersion matrices)
# sum of squares decomposition
TSS = totalSS(iris[,1:4])
BSS = betweenSS(iris[,1:4], iris[,5])
WSS = withinSS(iris[,1:4], iris[,5])
# check that TSS = BSS + WSS
TSS
BSS + WSS

## Covariance matrices
# variance decomposition
Tcov = totalCov(iris[,1:4])
Bcov = betweenCov(iris[,1:4], iris[,5])
Wcov = withinCov(iris[,1:4], iris[,5])
Tcov
Bcov + Wcov

# dividing by total number of observations
Tcov2 = totalCov(iris[,1:4], div_by_n=TRUE)
Bcov2 = betweenCov(iris[,1:4], iris[,5], div_by_n=TRUE)
Wcov2 = withinCov(iris[,1:4], iris[,5], div_by_n=TRUE)
Tcov2
Bcov2 + Wcov2


# ===============================================================================
# Descriptive Discriminant Analysis (Fisher DA)
# ===============================================================================

# descriptive discriminant analysis
d1 = desDA(iris[,1:4], iris[,5])
# eigenvalues
d1$values
# discriminant axes (linear disc functions)
d1$axes

# compared to lda in MASS
library(MASS)
dis1 = lda(iris[,1:4], iris$Species)
dis1$scaling

# plot on discriminant axes
dadf = data.frame(d1$scores, Species=iris$Species)
ggplot(data=dadf, aes(x=f1, y=f2)) + 
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_point(aes(colour=Species)) + 
  opts(title="Discriminant Plot")  

# compare to function "predict"
p1 = predict(dis1)
plot(p1$x, col=iris$Species)
# comparing scores
plot(d1$scores[,1], p1$x[,1])

# ===============================================================================
# Geometric Predictive Discriminant Analysis
# classification based on the distance from a point to a centroid
# This is a linear rule which is why we speak of linear discriminant analysis
# (check Tuffery discussion, page 340)
# ===============================================================================
# Each observation is classed in the group 'k' for which
# the distance to the centroig 'g_k' is minimal, 
# the distance being 'Winv' (Mahalanobis)
# this rule should not be used if the two groups 
# have different a-priori probabilities or variances

# geometric predictive discriminant analysis
mygeoda = geoDA(iris[,1:4], iris[,5])

# classification functions
mygeoda$functions

# confusion matrix
mygeoda$confusion

# predict -vs- classify
set.seed(222)
samp1 = sample(1:150, 20)
pre_lda = predict(dis1, iris[samp1,1:4])
cla_lda = classify(mygeoda, iris[samp1,1:4])
# compare results
pre_lda$class
cla_lda$pred_class


# ===============================================================================
# Quadratic Discriminant Analysis
# classification based on the distance from a point to a centroid
# This is a linear rule which is why we speak of linear discriminant analysis
# (check Tuffery discussion, page 340)
# ===============================================================================

# geometric predictive discriminant analysis
myqda = quaDA(iris[,1:4], iris[,5])

# confusion matrix
myqda$confusion

# compared to lda in MASS
dis2 = qda(iris[,1:4], iris$Species)
dis2

# predict -vs- classify
set.seed(333)
samp2 = sample(1:150, 20)
pre_qda = predict(dis2, iris[samp2,1:4])
cla_qda = classify(myqda, iris[samp2,1:4])
# compare results
pre_qda$class
cla_qda$pred_class





# ==============================================================
# discriminant axes
Winv = solve(Wcov)
WinvB_eig = eigen(Winv %*% Bcov)
lam = WinvB_eig$values
U = WinvB_eig$vectors
round(lam, 4)
# normalizing vectors u
alpha = sqrt(diag(t(U) %*% Wcov %*% U))
Unorm = U %*% diag(1/alpha)
t(Unorm[,1]) %*% Wcov %*% Unorm[,1]

# equivalent
Vinv = solve(Tcov)
VinvB_eig = eigen(Vinv %*% Bcov)
mu = VinvB_eig$values
round(mu, 4)
w = VinvB_eig$vectors
