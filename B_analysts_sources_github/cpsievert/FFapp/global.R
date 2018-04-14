dat <- read.csv("wk1.csv", header=TRUE, stringsAsFactors=FALSE) #add control for comparing weeks in the future?
names(dat) <- tolower(names(dat))
pos <- as.character(unique(dat$category))