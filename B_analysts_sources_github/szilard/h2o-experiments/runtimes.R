library(ggplot2)

d <- read.csv("./df-sweep2/runtimes.csv")
colnames(d) <- c("nodes","cores","time")

ggplot(d, aes(x = cores, y = 1/time, color = as.factor(nodes))) +
    geom_line() + geom_point() +
    scale_x_log10(breaks = c(1,2,4,8,16,32)) + 
    scale_y_log10(breaks = seq(0.2,1.2,0.2))
