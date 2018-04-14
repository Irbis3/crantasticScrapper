# Dean Attali
# Jan 22, 2014
# UBC STAT540

# explore the use of ggplot for visuals, mainly using the photoRec dataset

library(ggplot2)

# load the data
prDat <- read.table("GSE4051_MINI.txt", header = TRUE, row.names = 1)
str(prDat)

# rearrange the developmental stage factor levels in chronological order
devStageLevels = c('E16', 'P2', 'P6', 'P10', '4_weeks')
prDat$devStage <- factor(prDat$devStage, levels = devStageLevels)

##### quick plot
qplot(crabHammer, eggBomb, data = prDat)

##### scatter plot
p <- ggplot(prDat, aes(x = crabHammer, y = eggBomb))
str(p)
p <- p + geom_point()
print(p)
(p <- p + stat_smooth())
(p <- p + theme_bw() + 
   xlab("Expression of crabHammer") + 
   ylab("Expression of eggBomb") + 
   ggtitle("Scatterplot for expression levels"))

# plot both eggBomb and poisonFang vs crabHammer
nDat <-
  with(prDat,
       data.frame(sample, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"),
                                        each = nrow(prDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) + 
  geom_point() +
  stat_smooth(se = F))

# use only one line for both together
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) + 
   geom_point() + 
   stat_smooth(se = F, aes(group = 1)))

# separate panels
(p <- ggplot(nDat, aes(crabHammer, geneExp)) + 
   geom_point() + 
   facet_wrap(~ probeset))

# differentiate developmental stage
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = devStage)) + 
   geom_point() + 
   facet_wrap(~ probeset))


#### stripplot
oDat <-
  with(prDat,
       data.frame(sample, devStage, gType,
                  probeset = factor(rep(c("crabHammer", "eggBomb",
                                          "poisonFang"), each = nrow(prDat))),
                  geneExp = c(crabHammer, eggBomb, poisonFang)))
str(oDat)

# plot expression level of each gene
(p <- ggplot(oDat, aes(geneExp, probeset)) + 
   geom_point(position = position_jitter(height = 0.1)))

# gene expression change over course of development
(p <- ggplot(oDat, aes(devStage, geneExp)) + 
   geom_point())

# not too informative. show separate panel per gene, and add genotype information
(p <- p + facet_wrap(~ probeset) + aes(color = gType))

# add averages
(p <- p + stat_summary(fun.y = mean, geom = "point", shape = 4, size = 4))



##### density plots
(p <- ggplot(oDat, aes(geneExp)) + 
   geom_density())

# little more complex, showing the data points on the bottom
(p <- ggplot(oDat, aes(geneExp)) + 
   stat_density(geom = "line", position = "identity") + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))

# change bandwidth with "adjust"
(p <- ggplot(oDat, aes(geneExp)) + 
   stat_density(geom = "line", position = "identity", adjust = 0.5) + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))

# different panels per genotype
(p + facet_wrap(~gType))

# different colour per genotype
(p + aes(color = gType))


#### boxplot
# by genotype
(p <- ggplot(oDat, aes(devStage, geneExp)) + 
   geom_boxplot() + facet_wrap(~gType))

# violin plot
(p <- ggplot(oDat, aes(devStage, geneExp)) + 
   geom_violin())


### heatmaps

# load in full data and design matrix
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

# choose 50 probes out of the 30k to work with
yo <- sample(1:nrow(prDat), size = 50)
hDat <- prDat[yo, ]
colnames(hDat) <- with(prDes,
                       paste(devStage, gType, sidChar, sep="_"))

# transform the data to tall format
prDatTall <- data.frame(sample = rep(colnames(hDat), each = nrow(hDat)),
                        probe = rownames(hDat),
                        expression = unlist(hDat))

# create a blue -> purple palette
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
jBuPuPalette <- jBuPuFun(paletteSize)

# plot!
ggplot(prDatTall, aes(x = probe, y = sample, fill = expression)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  scale_fill_gradient2(low = jBuPuPalette[1],
                       mid = jBuPuPalette[paletteSize/2],
                       high = jBuPuPalette[paletteSize],
                       midpoint = (max(prDatTall$expression) + min(prDatTall$expression)) / 2,
                       name = "Expression")

# same thing in lattice:
hDat <- as.matrix(t(hDat))
heatmap(hDat, Rowv = NA, Colv = NA, scale="none", col = jBuPuPalette)


#### overplotting and matrix plots

# choose two samples to plot against each other
(yo <- sample(1:ncol(prDat), size = 2))
bDat <- data.frame(y = prDat[[yo[1]]], z = prDat[[yo[2]]])
str(bDat)

# plot against each other
(p <- ggplot(bDat, aes(z, y)) + 
   geom_point())

# wayyyy too many points! use transparency to get a clearer (pun not intended) picture
(p <- ggplot(bDat, aes(z, y)) + 
   geom_point(alpha = 0.1))

# using density2d with colour gradients
(p <- ggplot(bDat, aes(z, y)) + 
   stat_density2d(geom = "tile", contour = F, aes(fill = ..density..)) + 
   scale_fill_gradient(low = "white", high = "blue"))

# using binned hexagons
(p <- ggplot(bDat, aes(z, y)) + 
   stat_binhex())

# using plotmatrix
(yo <- sample(1:ncol(prDat), size = 4))
pairDat <- subset(prDat, select = yo)
str(pairDat)
(p <- plotmatrix(pairDat) + 
   stat_binhex())