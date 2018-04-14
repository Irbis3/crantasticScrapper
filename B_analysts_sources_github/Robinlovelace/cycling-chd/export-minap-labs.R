# Aim: load and 'scramble' some minap data

# library(readstata13)
# f <- "N:/Faculty-of-Medicine-and-Health/LIGHT/Cardiovascular Epidemiology/Robin Lovelace/Full MINAP_RL.dta"
# sample_data <- read.dta13(f)
# saveRDS(sample_data, "data/sample_data.Rds")
sample_data <- readRDS("data/sample_data.Rds")
names(sample_data)
labname <- get.label.name(sample_data)
labname <- labname[labname != ""]

x <- data.frame(shortname =names(readstata13::get.varlabel(sample_data)),
                longname = readstata13::get.varlabel(sample_data))

# write.csv(x, "labnames.csv")

# library(jsonlite)
# labout <- as.list(1:length(labname))
# for(i in 1:length(labname)){
#   labout[[i]] <- readstata13::get.label(sample_data, label.name = labname[i])
#   labout[[i]] <- paste0(labout[[i]], " = ", names(readstata13::get.label(sample_data, label.name = labname[i])))
# }

# labout <- setNames(labout, labname)
# names(labout)

# x <- toJSON(labout, pretty = T)
# writeLines(x, "labnames.json")


set.seed(54)
write.csv(sample_data[sample(x = nrow(sample_data), size = 100),], "testdat.csv")

labs = read.csv("labnames.csv")
labs
labs$shortname
selection = which(labs$shortname == "ecg_place")
selection
labs[selection,]

