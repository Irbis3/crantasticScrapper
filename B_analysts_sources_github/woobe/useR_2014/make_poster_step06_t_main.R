## =============================================================================
## Mini script - print text in title (main)
## =============================================================================

## CrimeMap

txt_t_main_cm1 <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 6, label = "Crime Hot Spots in Central London", size = 15, colour = col_text) +
  annotate("text", x = 5, y = 2, label = "January 2014 - 2,913 Records", size = 12, colour = col_text2) +
  theme_blank

txt_t_main_cm2 <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 6, label = "Exploring Different Crime Categories", size = 15, colour = col_text) +
  annotate("text", x = 5, y = 2, label = "Faceting with ggplot2", size = 12, colour = col_text2) +
  theme_blank

print(txt_t_main_cm1, vp = vplayout(set_pixel["t_main", 2:3], 21:702))
print(txt_t_main_cm2, vp = vplayout(set_pixel["t_main", 2:3], 723:1404))


## rCrimemap

txt_t_main_rcm1 <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 6, label = "All Crimes in UK (excl. Scotland)", size = 15, colour = col_text) +
  annotate("text", x = 5, y = 2, label = "January 2014 - 425,692 Records", size = 12, colour = col_text2) +
  theme_blank

txt_t_main_rcm2 <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 6, label = "Exploring Crimes in Central London", size = 15, colour = col_text) +
  annotate("text", x = 5, y = 2, label = "Navigating and Zooming with rMaps", size = 12, colour = col_text2) +
  theme_blank

print(txt_t_main_rcm1, vp = vplayout(set_pixel["t_main", 2:3], 1425:2106))
print(txt_t_main_rcm2, vp = vplayout(set_pixel["t_main", 2:3], 2127:2808))
