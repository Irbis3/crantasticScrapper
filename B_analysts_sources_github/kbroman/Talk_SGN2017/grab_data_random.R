# Get LOD curve and data for a single time point
# and convert to JSON file for interactive graph

set.seed(69456948)

library(qtl)
load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")

# reduce the set of markers
marker_subset <- NULL
for(i in names(f2$geno))
    marker_subset <- c(marker_subset,
                       pickMarkerSubset(pull.map(f2, chr=i)[[1]], 0.5))
f2 <- pull.markers(f2, marker_subset)
f2$pheno <- cbind(weight=growthSm[,5], f2$pheno[,c("ID", "sex", "pgm")])
sex <- as.numeric(f2$pheno$sex)

# create matrix with original phenotypes plus
# a bunch of permuted versions
n.perm <- 100
phe <- f2$pheno[,1]
phem <- phe
for(i in 1:n.perm)
  phem <- cbind(phem, sample(phe))
colnames(phem) <- c("wt_5wks", paste0("perm", 1:n.perm))
f2$pheno <- cbind(phem, f2$pheno[,-1,drop=FALSE])
f2 <- calc.genoprob(f2, step=1, error.prob=0.002, map.function="c-f")
out <- scanone(f2, phe=1:(n.perm+1), addcovar=sex, method="hk", n.cluster=parallel::detectCores())

# marker index within lod curves
map <- pull.map(f2)
names(out)[3] <- "lod"
outspl <- split(out[,1:3], out[,1])
mar <- map
for(i in seq(along=map)) {
  mar[[i]] <- match(names(map[[i]]), rownames(outspl[[i]]))-1
  names(mar[[i]]) <- names(map[[i]])
}
markers <- lapply(mar, names)

outspl <- lapply(split(out, out[,1]), function(a) {
  b <- as.list(a[,2:3])
  b[[2]] <- t(as.matrix(a[,-(1:2)]))
  dimnames(b[[2]]) <- NULL
  b })

f2i <- pull.geno(fill.geno(f2, err=0.002, map.function="kosambi"))
g <- pull.geno(f2)
xmar <- markernames(f2, chr="X")
g[,xmar] <- reviseXdata("f2", "full", getsex(f2), geno=g[,xmar], cross.attr=attributes(f2))
f2i[,xmar] <- reviseXdata("f2", "full", getsex(f2), geno=f2i[,xmar], cross.attr=attributes(f2))
f2i[is.na(g) | f2i != g] <- -f2i[is.na(g) | f2i != g]
f2i <- as.list(as.data.frame(f2i))
individuals <- f2$pheno$ID

# write data to JSON file
library(RJSONIO)
cat0 <- function(...) cat(..., sep="", file="data_random.json")
cat0a <- function(...) cat(..., sep="", file="data_random.json", append=TRUE)
cat0("{\n")
cat0a("\"random\": true,\n\n")
cat0a("\"phenotype\" : \"", "Weight (g) at 5 weeks", "\",\n\n")
cat0a("\"chr\" :\n", RJSONIO::toJSON(chrnames(f2)), ",\n\n")
cat0a("\"lod\" :\n", RJSONIO::toJSON(outspl, digits=8), ",\n\n")
cat0a("\"markerindex\" :\n", RJSONIO::toJSON(mar), ",\n\n")
cat0a("\"markers\" :\n", RJSONIO::toJSON(markers), ",\n\n")
phem <- as.matrix(phem)
dimnames(phem) <- NULL
cat0a("\"phevals\" :\n", RJSONIO::toJSON(t(phem), digits=6), ",\n\n")
cat0a("\"geno\" :\n", RJSONIO::toJSON(f2i), ",\n\n")
cat0a("\"individuals\" :\n", RJSONIO::toJSON(individuals), ",\n\n")
cat0a("\"jitter\" :\n", RJSONIO::toJSON(runif(nind(f2), -1, 1)), "\n\n")
cat0a("}\n")
