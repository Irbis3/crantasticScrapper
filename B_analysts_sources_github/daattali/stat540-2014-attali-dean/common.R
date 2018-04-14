# This script contains some common R code that is used by multiple scripts

library(plyr)
library(ggplot2)
library(reshape2)


# output a data frame with only data from the input genes
# must pass in the data dataframe, the design dataframe, and a list of genes
# this function is specific to the photoRec dataset - it uses variable names from there
prepareData <- function(.dat, .des, .genes) {  
  # step 1: transpose the data matrix, keeping only the genes we're interested in
  .dat <- as.data.frame(t(.dat[.genes, ]))
  
  # step 2: add the sample id info to each row
  .dat$sidChar <- row.names(.dat)
  
  # step 3: "melt" the dataframe from wide format to long format
  # this will achieve having just two columns for every sample: gene, and expression
  .dat <- melt(.dat,
               id.vars = c("sidChar"),
               measure.vars = .genes,
               variable.name = "gene",
               value.name = "gExp")
  
  # step 4: combine the gene/expression data with the metadata per sample
  .dat <- merge(.des, .dat)
  
  # done!
  return(.dat)
}

# do a stripplot of a mini dataset
# assumes that the dataframe has columns "devStage", "gExp", "gType"
makeStripplot <- function(.dat) {
  p <- ggplot(.dat, aes(x = devStage, y = gExp, color = gType, group = gType))
  p <- p + geom_line(stat = "summary", fun.y = mean)
  p <- p + geom_point(position = position_jitter(width = .2))
  p <- p + facet_wrap(~ gene)
  
  print(p)
}
