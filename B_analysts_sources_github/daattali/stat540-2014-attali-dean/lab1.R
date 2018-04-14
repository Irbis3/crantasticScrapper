# Dean Attali
# Jan 13, 2014
# UBC STAT540
# simple little script that just looks at basic info in a small genomic dataset

# read input and sanity check import
prDat <- read.table("GSE4051_MINI.txt", header = TRUE, row.names = 1)
str(prDat)

nrow(prDat)
# there are 39 rows

ncol(prDat)
# there are 6 columns

# inspect first 4 rows
head(prDat, 4)

# inspect random 4 rows
prDat[sample(nrow(prDat), 4), ]

# each row corresponds to a mouse

colnames(prDat)
# variable names are "sample"     "devStage"   "gType"      "crabHammer" "eggBomb"    "poisonFang"

str(prDat)
# sample: int, devStage: factor, gType: factor, crabHammer: num, eggBomb: num, poisionFang: num

# checks that each sample occurs exactly once 1-39
all(sort(prDat$sample) == 1:39)
all.equal(sort(prDat$sample), 1:39)
identical(table(prDat$sample), table(1:39))
all(sort(prDat$sample) == seq(1, 39, 1))
all(sort(prDat$sample) == seq_len(39))

levels(prDat$devStage)
# devStage has: "4_weeks" "E16"     "P10"     "P2"      "P6"
levels(prDat$gType)
# gType has: "NrlKO" "wt"

summary(prDat$devStage)
# 4_weeks     E16     P10      P2      P6 
# 8       7       8       8       8 
table(prDat$gType)
# NrlKO    wt 
# 19    20 

# cross tabulation of devstage and gtype
table(prDat$devStage, prDat$gType)
# NrlKO wt
# 4_weeks     4  4
# E16         3  4
# P10         4  4
# P2          4  4
# P6          4  4
# looks like they were trying to get 4 samples from each genotype at each time point, but missed one

# some statistics of one of the genes
min(prDat$crabHammer)   # 8.214
max(prDat$crabHammer)   # 10.34
range(prDat$crabHammer)    # 8.214 10.340
summary(prDat$crabHammer)   #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                            # 8.214   8.938   9.611   9.428   9.830  10.340
fivenum(prDat$crabHammer)   # 8.214  8.938  9.611  9.830 10.340
mean(prDat$crabHammer)     # 9.427821
median(prDat$crabHammer)  # 9.611
quantile(prDat$crabHammer)  # 0%    25%    50%    75%   100% 
                            # 8.214  8.938  9.611  9.830 10.340


# keep only the rows with poisonFang > 7.5
weeDat <- subset(prDat, poisonFang > 7.5)
nrow(weeDat)  # there are 9 rows with poisonFang > 7.5

# break them down by developmental stage and genotype
addmargins(table(weeDat$devStage, weeDat$gType))
addmargins(with(weeDat, table(devStage, gType)))

# get only the gene expression values for sample 16 and 38
prDat[c("Sample_16", "Sample_38"), 4:6]

# in which samples is eggBomb less than the 10% percentile?
rownames(prDat[prDat$eggBomb < quantile(prDat$eggBomb, 0.1), ])
# samples 25, 14, 3, 35