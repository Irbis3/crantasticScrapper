
library(ggplot2)

d <- read.csv("data/gapminderDataFiveYear.txt", sep = "\t")

head(d)

d <- subset(d, pop>1e6)

ggplot(d) + geom_point(aes(x = gdpPercap, y = lifeExp))

ggplot(d) + geom_point(aes(x = gdpPercap, y = lifeExp, color = continent))

ggplot(d) + geom_point(aes(x = gdpPercap, y = lifeExp)) + scale_x_log10()

ggplot(d) + geom_point(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) + 
    scale_x_log10()

ggplot(d) + geom_point(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) + 
    scale_x_log10() + facet_wrap( ~ year) 

ggplot(d) + geom_point(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) + 
    scale_x_log10() + facet_wrap( ~ year) + 
    geom_smooth(aes(x = gdpPercap, y = lifeExp), se = FALSE, method = "loess", color = "black")

