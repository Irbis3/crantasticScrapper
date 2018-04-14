# ws ----------------------------------------------------------------------
rm(list = ls())
source("code/99_helpers.R")
library(tidyverse)
library(stringr)

# data --------------------------------------------------------------------
data <- readRDS("input/data_load.rds")
glimpse(data)

# created -----------------------------------------------------------------
newvars <- paste("created",
                 c(
                   paste("day", c("y", "m", "d"), sep = "_"),
                   paste("time", c("h", "m", "s"), sep = "_")
                 ), sep = "_")

data <- separate(data, created, newvars, sep = "-| |:")

data <- mutate_at(data, vars(starts_with("created_")), as.numeric)

glimpse(data)


# photos ------------------------------------------------------------------
data <- mutate(data, photos_n = map_dbl(data$photos, length))
hchart(data$photos_n)

# export ------------------------------------------------------------------
saveRDS(data, "data/data_feat_misc.rds")


