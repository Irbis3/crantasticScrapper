setwd("~/Desktop")
library(ecoengine)
library(plyr)
library(lubridate)
library(dplyr)
# rMaps is not available on CRAN yet.
# Install from here: https://github.com/ropensci/rmaps#installation
library(rMaps)

# I do not recommend running the next code block unless you have 20+ minutes to spare.
# Start from line 48 and read the csv from here: https://gist.github.com/karthik/9196883
rodent_pages <- ee_pages(ee_observations(order = "Rodentia", progress = FALSE, quiet = TRUE))
page_breaks <- split(1:rodent_pages, ceiling(seq_along(1:rodent_pages)/1000))
rodent_data <- list()
# This step will take a while, as it downloads ~50 mb of data with pauses to avoid hammering API
for(i in 1:length(rodent_data)) {
	rodent_data <- ee_observations(order = "Rodentia", page = page_breaks[[1]])
	write.csv(rodent_data$data, file = paste0("rodent_data", i, ".csv"))
}


files <- list.files(pattern = "rodent_data")
all_rodent_data <- ldply(files, function(x) {
		read.csv(x, header = TRUE)
}, .progress = "text")

# This saves all 235,000 observations into a ~700 kb file.
save(all_rodent_data, file = "results.Rda", compress = "bzip2")

load("results.Rda")
states <- read.csv("state_table.csv", header = TRUE)
state_small <- states[, 1:3]
names(state_small)[2] <- "state_province"
rodents <- join(all_rodent_data, state_small, by = "state_province")
rodents$begin_date <- ymd(rodents$begin_date)
rodents$year <- year(rodents$begin_date)

# Now boiling down the code into something I can visualize using rMaps
# Number of rodent specimens over the years collected by the museum

rodents_summary <- rodents[!is.na(rodents$year), ] %.%
 regroup(list(quote(abbreviation), quote(year))) %.%
  summarise(n = n())
names(rodents_summary) <- c("State", "Year", "Counts")


library(rMaps)
# csv is here: https://gist.github.com/karthik/9196883
rodents_summary <- read.csv("rodents_summary.csv", header = TRUE)
# Something's wrong here. Slider works one way. But it wont reset state when you go back
# Is there any way to speed up the animation? 
ichoropleth(Counts ~ State, data = rodents_summary, animate = "Year", pal = "Reds", ncuts = 4)
