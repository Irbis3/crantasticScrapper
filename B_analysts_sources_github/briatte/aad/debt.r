## An overview of the Reinhart & Rogoff data, from an exercise by Cosma Shalizi.
## 2014-10-09

## -- references ---------------------------------------------------------------

# Carmen M. Reinhart and Kenneth S. Rogoff, "Growth in a Time of Debt",
# American Economic Review 100 (2010): 573–578

# Thomas Herndon, Michael Ash and Robert Pollin, "Does High Public Debt 
# Consistently Stifle Economic Growth? A Critique of Reinhart and Rogoff", 
# Political Economy Research Institute, University of Massachusetts-Amherst, 
# Working Paper 322 (2013)
# http://www.peri.umass.edu/236/hash/31e2ff374b6377b2ddec04deaa6388b1/publication/566/

## -- package ------------------------------------------------------------------

library(ggplot2)

## -- dataset ------------------------------------------------------------------

file = "debt.csv"
if(!file.exists(file))
  download.file("http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/11/debt.csv",
                file, mode = "wb")

debt = read.csv(file)

## -- variables ----------------------------------------------------------------

str(debt)
debt = debt[, -1]

# decade variable
debt$Decade = factor(10 * debt$Year %/% 10)

head(debt)
table(debt$Country)

## -- plots --------------------------------------------------------------------

# axis titles
ra = "\npublic debt / GDP (%)"
gr = "real GDP growth (%)\n"

# fig. 1
qplot(data = debt, y = growth, x = Year, geom = "line") + 
  facet_wrap(~ Country) +
  labs(x = NULL, y = gr)

# fig. 2
qplot(data = debt, y = ratio, x = Year, geom = "line") + 
  facet_wrap(~ Country) +
  labs(x = NULL, y = ra)

# fig. 3
ggplot(data = debt, aes(y = growth, x = ratio)) + 
  geom_point(color = "grey50") +
  geom_smooth(method = "loess", size = 1, color = "black", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 200, by = 100)) +
  facet_wrap(~ Decade, nrow = 1) +
  labs(y = gr, x = ra, title = "Corrected Reinhart and Rogoff data, 1946-2009\n") +
  theme_linedraw(12)

# uncomment to save last plot
# ggsave("reinhart-rogoff.png", width = 11, height = 8)

## have a nice day
