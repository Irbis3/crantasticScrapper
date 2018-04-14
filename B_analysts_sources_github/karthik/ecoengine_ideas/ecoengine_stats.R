

library(ecoengine)
library(plyr)
library(lubridate)
classes <- c("Mammalia", "Aves", "Amphibia", "Osteichthyes", "Chondrichthyes")
start <- ymd("1900-01-01")

results <- list()
for(i in 1:25) {
	end <- start + days(1825)
	counts <- list()
	counts <- sapply(classes, function(x) {
		ee_observations(clss = x, min_date = as.character(start), 
		max_date = as.character(end), progress = FALSE, quiet = TRUE)$results
	})
	df <- data.frame(t(counts))
	df$begin <- year(start)
	start <- end
	results[[i]] <- df
}

all_results <- ldply(results)
write.csv(all_results, file = "all_results.csv")

all_results <- read.csv("all_results.csv")
library(ggplot2)
df <- melt(all_results, id.vars = "begin")


clss_plot <- ggplot(df, aes(x = begin, y = value, fill = factor(variable))) + 
geom_bar(stat = "identity", position = "stack") + 
scale_fill_manual(values = brewer.pal(5, "Set1")) + ggtitle("Distribution of specimens across major classes") + 
guides(fill=guide_legend(title="Class"))

ggsave(clss_plot, file = "clss_plot.png", width = 6, height = 4)






