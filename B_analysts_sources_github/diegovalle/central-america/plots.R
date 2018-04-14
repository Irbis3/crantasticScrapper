########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Thu Sep  6 17:34:21 2012
## Email: diegovalle at gmail.com
## Purpose: Choropleths of Central America and Mexico violence
## Copyright (c) Diego Valle-Jones. All rights reserved



q <- ggplot(ca.map2, aes(long, lat, group=group)) +
  geom_polygon(aes(fill = rate), color = "black", size = .05) +
  geom_polygon(data = ca.map.borders,
               aes(long, lat, group=group),
               color = "gray10", size = .4, fill = NA) +
  scale_fill_gradientn("homicide\nrate", colours=brewer.pal(9, "Reds")) +
  guides(fill = guide_colorbar(colours = topo.colors(10))) +
  coord_map() +
  theme_nothing()

p <- q+ geom_polygon(data = states.map.borders,
               aes(long, lat, group=group),
               color = "gray10", size = .2, fill = NA) +
  ggtitle("Homicide Rates in Southern Mexico and Central America, by Municipality or Department\n(Mexican Rates from Vital Statistics)")
ggsave("graphs/central-america.png", dpi = 100,
       width = 9.6, height = 8, plot = p)


p <- q +
  geom_polygon(data = states.map.borders,
               aes(long, lat, group=group, fill = rate),
               color = "gray10", size = .2) +
  coord_map() +
  ggtitle("Homicide Rates in Southern Mexico and Central America, by State or Department\n(Mexican Rates from Police Sources)")
ggsave("graphs/central-america-states.png", dpi = 100,
       width = 9.6, height = 8, plot = p)
