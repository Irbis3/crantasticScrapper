library(ggplot2)
library(Cairo)
theme_set(theme_bw())
tj <- read.csv("data/rates.csv")

scale <- "number of\nhomicides"
scale <- "número de\nhomicidios"
title <- "Homicide Rates in the Metropolitan Area of Tijuana (1990-2009)"
title <- "Tasa de Homicidios en la Zona Metropolitana de Tijuana (1990-2009)"

Cairo("graphs/tijuana.png", width = 800, height = 500)
print(ggplot(tj, aes(year, rate)) +
  geom_line() +
  geom_point(aes(size = homicides)) +
  scale_area(scale, to = c(1, 10),
             breaks = rev(c(200, 550, 900, 1250))) +
  opts(title = title) +
  scale_y_continuous(limits = c(0, max(tj$rate))) +
  xlab("año") +
  ylab("tasa de homicidios"))
  #geom_rect(aes(xmin = 2007, xmax = 2010, ymin = -Inf, ymax = Inf),
   #         fill = alpha("red", .01), legend = FALSE)
dev.off()
ggsave("pdfs/tijuana.pdf", width = 8, height = 5)

