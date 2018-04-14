library(knitr)
knit("ColorAnalysis.Rhtml")
source("loadPalettes.R")
pal <- allSequential[[2]]
plotCols(pal[,1], pal[,2], pal[,3])
writeWebGL(dir=".", filename="ColorAnalysis.html", template="ColorAnalysis.html", width=400, prefix="rgb")

plotCols(pal[,1], pal[,2], pal[,3], pca=1)
writeWebGL(dir=".", filename="ColorAnalysis.html", template="ColorAnalysis.html", width=400, prefix="pca")

