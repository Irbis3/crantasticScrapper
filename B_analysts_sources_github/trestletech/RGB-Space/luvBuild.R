library(knitr)
library(colorspace)
knit("LuvColorAnalysis.Rhtml")
source("loadPalettes.R")
pal <- getSequentialLuv(2, allSequential)@coords
plotLuvCols(pal[,1], pal[,2], pal[,3])
writeWebGL(dir=".", filename="LuvColorAnalysis.html", template="LuvColorAnalysis.html", width=400, prefix="rgb")

plotLuvCols(pal[,1], pal[,2], pal[,3], pca=1)
writeWebGL(dir=".", filename="LuvColorAnalysis.html", template="LuvColorAnalysis.html", width=400, prefix="pca")

plotDimensionalScoring(allSequential, normAvgs)
writeWebGL(dir=".", filename="LuvColorAnalysis.html", template="LuvColorAnalysis.html", width=400, prefix="hcl")

plotLuvCols(csp[,"L"], csp[,"C"], csp[,"H"], 1)
writeWebGL(dir=".", filename="LuvColorAnalysis.html", template="LuvColorAnalysis.html", width=400, prefix="colorspace")
