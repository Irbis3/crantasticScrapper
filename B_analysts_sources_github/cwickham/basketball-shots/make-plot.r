library(dplyr)
library(ggplot2)
shots <- read.csv("LAshots.csv")

head(shots)
qplot(x, y, data = shots)
shots$points[shots$result == "missed"] <- 0

shots_df <- group_by(tbl_df(shots), x, y)

shots_sum <- summarise(shots_df,
  num_shots = length(result),
  num_made = sum(result == "made"),
  prop_made = mean(result == "made"),
  avg_points = mean(points),
  total_points = sum(points))
write.csv(shots_sum, file = "shots_sum.csv", row.names = FALSE)

head(shots_sum)

ggplot(shots_sum, aes(x, y)) +
  geom_point(aes(size = num_shots, colour = avg_points))
# what's that crazy point pulling off the scale?

subset(shots_sum,num_shots > 3000)
# right under basket?

subset(shots_sum, x == 25 & y == 6)
head(subset(shots, x == 25 & y == 6))
# lots of layups
table(subset(shots, x == 25 & y == 6)$type)

ggplot(subset(shots_sum, !(x == 25 & y == 6)), aes(x, y)) +
  geom_point(aes(size = num_shots, colour = avg_points))

ggplot(subset(shots_sum, !(x == 25 & y == 6)), aes(x, y)) + 
  geom_point(aes(color = avg_points, size = num_shots)) +
  ylim(0, 35) + 
  scale_colour_distiller("Points", palette = "RdYlGn", trans = "reverse") +
  scale_size("Attempts", trans = "sqrt", range = c(0.5, 10)) +
  coord_equal() + 
  theme_classic(18) +
  theme(axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank()) +
  guides(color = "none", size = "none")