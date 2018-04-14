library(dplyr)
library(ggplot2)

leagues <- read.csv("data/league-clean.csv", stringsAsFactors = FALSE, comment = "#")

leagues$stage <- factor(leagues$stage, 
  levels = c("Group Stage", "Round of 16", "Quarterfinals",
    "Semifinals", "Finals"))
leagues$rank <- factor(leagues$rank, levels = 10:1)

# basic layout
ggplot(leagues, aes(stage, rank)) +
  geom_tile()

# or
ggplot(leagues, aes(1, rank)) +
  geom_tile() +
  facet_grid(~ stage)

# add fill colours
ggplot(leagues, aes(stage, rank)) +
  geom_tile(aes(fill = region), width = 0.8, height = 0.9) +
  scale_fill_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  theme_minimal()

# add text
ggplot(leagues, aes(stage, rank)) +
  geom_tile(aes(fill = region), width = 0.8, height = 0.9) +
  geom_text(aes(label = labels), size = 3) +
  scale_fill_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  theme_minimal()

# add connections
# first attempt
ggplot(leagues, aes(stage, rank)) +
  geom_tile(aes(fill = region), width = 0.8, height = 0.9) +
  geom_text(aes(label = labels), size = 3) +
  geom_line(aes(group = league)) +
  scale_fill_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  theme_minimal()

# might need to create a separate connections dataset
# to use with geom_segment + geom_point
get_connects <- function(league_df){
  n <- length(league_df)
  ind_connect <- which(diff(as.numeric(league_df$stage)) == 1)
  with(league_df, 
    data.frame(
      x = stage[ind_connect], xend =stage[ind_connect+1],
      y = rank[ind_connect], yend = rank[ind_connect + 1],
      league = league[ind_connect],
      region = region[ind_connect]
    )
  )  
}

league_gp <- group_by(leagues, league)
connections <- do(league_gp, get_connects(.))

# add connections
# take two
ggplot(leagues, aes(stage, rank)) +
  geom_tile(aes(fill = region), width = 0.8, height = 0.9) +
  geom_text(aes(label = labels), size = 3) +
  geom_segment(aes(x = as.numeric(x) + 0.4, xend = as.numeric(xend) - 0.4,
    y = y, yend = yend), data = connections, inherit.aes = FALSE) +
  scale_fill_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  theme_minimal()

ggplot(leagues, aes(stage, rank)) +
  geom_tile(aes(fill = region), width = 0.8, height = 0.9) +
  geom_text(aes(label = labels), size = 3) +
  geom_segment(aes(x = as.numeric(x) + 0.45, xend = as.numeric(xend) - 0.45,
    y = y, yend = yend, colour = region), 
    data = connections, inherit.aes = FALSE, size = 1) +
  geom_point(aes(x = as.numeric(x) + 0.45, y = y, colour = region), data = connections, 
    inherit.aes = FALSE) +
  geom_point(aes(x = as.numeric(xend) - 0.45, y = yend, colour = region), data = connections, 
    inherit.aes = FALSE) + 
  scale_fill_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  scale_colour_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  theme_minimal()

# fix other theme issues and minor positioning issues
ggplot(leagues, aes(stage, rank)) +
  geom_tile(aes(fill = region), width = 0.8, height = 0.9) +
  geom_text(aes(x = as.numeric(stage) - 0.38, label = league), size = 3, hjust = 0) +
  geom_text(aes(x = as.numeric(stage) + 0.38, label = paste(percent, "%", sep = "")), size = 3, hjust = 1) +
  geom_segment(aes(x = as.numeric(x) + 0.41, xend = as.numeric(xend) - 0.41,
    y = y, yend = yend, colour = region), 
    data = connections, inherit.aes = FALSE, size = 1) +
  geom_point(aes(x = as.numeric(x) + 0.41, y = y, colour = region), data = connections, 
    inherit.aes = FALSE) +
  geom_point(aes(x = as.numeric(xend) - 0.41, y = yend, colour = region), data = connections, 
    inherit.aes = FALSE) + 
  scale_fill_manual(values = c("#E3ECE1", "#D0DFE9", "#F5E8C8")) +
  scale_colour_manual(values = c("#C8DAC4", "#B1CADA", "#EED8A2")) +
  theme_minimal() +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
    panel.grid = element_blank(), axis.title = element_blank(),
    legend.position = "bottom") 

ggsave("league-ranks.png", height = 3, width =13)
