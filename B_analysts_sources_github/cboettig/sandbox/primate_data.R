require(ape, quietly = TRUE, save = FALSE)
nuc_set <- read.nexus("nuc.ucld.trees")
traits <- read.csv("Primate_brain_comparisons.csv")
nuc <- nuc_set[[1]]

