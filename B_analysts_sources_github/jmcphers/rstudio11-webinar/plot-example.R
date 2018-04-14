# use ggplot2 for plotting
library(ggplot2)

# first layer: dots
p <- ggplot(mpg, aes(displ, hwy)) +
     geom_point(aes(color = class))
p

# second layer: smoother
p <- p + geom_smooth(se = FALSE) 
p

# third layer: title
p <- p + labs(title = "Fuel efficiency generally decreases with engine size")
p
