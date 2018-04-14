# Dean Attali
# Jan 29, 2014
# UBC STAT540

# lab 5 - fitting and interpreting linear models - LOW VOLUME

library(plyr)
library(ggplot2)
library(reshape2)

# load the data
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")

# output a data frame with only data from the input genes
prepareData <- function(.genes) {  
  # step 1: transpose the data matrix
  prDatT <- as.data.frame(t(prDat))
  
  # step 2: retain only the columns corresponding to genes we're interested in
  # NOTE: instead, can use 'prDat[.genes, ]' in step 1
  colNumsKeep <- which(colnames(prDatT) %in% .genes)
  prDatT <- prDatT[, colNumsKeep, drop = F]
  
  # step 3: add the sample id info to each row
  prDatT$sidChar <- row.names(prDatT)
  
  # step 4: "melt" the dataframe from wide format to long format
  # this will achieve having just two columns for every sample: gene, and expression
  prDatT <- melt(prDatT,
                 id.vars = c("sidChar"),
                 measure.vars = .genes,
                 variable.name = "gene",
                 value.name = "gExp")
  
  # step 5: combine the gene/expression data with the metadata per sample
  prDatT <- merge(prDes, prDatT)
  
  # done!
  return(prDatT)
}


# test the function to create a dataframe with 2 genes of interest
luckyGenes <- c("1419655_at", "1438815_at")
jDat <- prepareData(luckyGenes)
str(jDat)


# do a stripplot of a mini dataset
# assumes that the dataframe has columns "devStage", "gExp", "gType"
makeStripplot <- function(.data) {
  p <- ggplot(.data, aes(x = devStage, y = gExp, color = gType, group = gType))
  p <- p + geom_line(stat = "summary", fun.y = mean)
  p <- p + geom_point(position = position_jitter(width = .2))
  p <- p + facet_wrap(~ gene)
  
  print(p)
}


# test the stripplot function
makeStripplot(jDat)


# do a two-sample t-test of a single gene at two different developmental stages
testGene <- prepareData('1456341_a_at')
t.test(gExp ~ devStage,
       testGene,
       subset = devStage %in% c("P2", "4_weeks"),
       var.equal = T)




# do a one-way anova (fit linear model with categorical covariate) only using wild type
testGene <- prepareData('1438786_a_at')
makeStripplot(testGene)
fit <- lm(gExp ~ devStage, testGene, subset = (gType == 'wt'))
fitSummary <- summary(fit)



# peform inference for a contrast
# test the difference between P2 and P10 since they look similar in the plot
coefs <- coef(fit)
contMat <- matrix(c(0, 1, 0, -1, 0), nrow = 1)
obsDiff <- contMat %*% coefs

# check that the reported number is correct
sampMeans <-
  ddply(subset(testGene, gType == 'wt'), ~ devStage, summarize, gExp = mean(gExp))
with(sampMeans, gExp[devStage == "P2"] - gExp[devStage == "P10"])

# make sure the diagonal in the variance-covariance matrix is the square of the fit std errors
identical(fitSummary$coefficients[, 2],
          sqrt(diag(vcov(fit))))


# calculate estimated standard error
estSe <- contMat %*% vcov(fit) %*% t(contMat)

# create a test statistic: observed effect / estimated error
testStat <- obsDiff/estSe

# calculate two-sided p-value
2 * (1 - pt(abs(testStat), df = df.residual(fit)))
# p-value is 0.4, pretty large, so we cannot reject the null hypo that P2 = P10



# fit linear model with two categorical covariates (genotype and dev stage interacting)
testGene <- prepareData("1448690_at")
makeStripplot(testGene)
fitBig <- lm(gExp ~ gType * devStage, testGene)
# quick look into the results
summary(fitBig)$coef

# fit again, but this time without allowing for interaction effects
fitSmall <- lm(gExp ~ gType + devStage, testGene)
# quick look into the results
summary(fitSmall)$coef

# use anova to see if reduction in sum of squared residuals is big enough given more complex model
anova(fitSmall, fitBig)
# p-value is very close to 1, so conclude that interaction terms are not necessary 