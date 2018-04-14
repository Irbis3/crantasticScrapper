library(dplyr)
library(rvest)
library(stringr)

library(network)
library(sna)

library(ggplot2)
library(grid)

dir.create("data", showWarnings = FALSE)
dir.create("photos-tk", showWarnings = FALSE)
dir.create("photos-ek", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

dir.create("raw", showWarnings = FALSE)

dir.create("raw/tk", showWarnings = FALSE)
dir.create("raw/tk/bill-lists", showWarnings = FALSE)
dir.create("raw/tk/bill-pages", showWarnings = FALSE)
dir.create("raw/tk/mp-pages", showWarnings = FALSE)

dir.create("raw/ek", showWarnings = FALSE)
dir.create("raw/ek/bill-lists", showWarnings = FALSE)
dir.create("raw/ek/bill-pages", showWarnings = FALSE)
dir.create("raw/ek/mp-pages", showWarnings = FALSE)

plot = TRUE
mode = "fruchtermanreingold"

source("functions.r")
source("parties.r")
source("data-tk.r") # slow
source("data-ek.r")
source("build.r")

save(list = ls(pattern = "^(net|edges|bills)_nl_(tk|ek)\\d{4}$"),
     file = "data/net_nl.rda")
