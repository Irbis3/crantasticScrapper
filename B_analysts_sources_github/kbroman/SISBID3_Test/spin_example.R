#' load libraries
library(babynames)

#' First few rows of the babynames data.
head(babynames) 

#' filter out Karl and Carl
library(dplyr)
karl <- babynames %>% filter(name=="Karl" | name=="Carl", sex=="M")

#' Here's a plot of the Karls.
library(ggplot2)
karl %>% ggplot(aes(x=year, y=prop, color=name)) + geom_line()
