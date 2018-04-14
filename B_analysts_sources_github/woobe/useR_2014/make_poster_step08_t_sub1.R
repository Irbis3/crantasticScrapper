## =============================================================================
## Mini script
## =============================================================================

## CrimeMap
title_mot <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Motivation", size = 15, colour = col_text) +
  theme_blank

title_dep <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Dependencies", size = 15, colour = col_text) +
  theme_blank

title_dat <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Crime Data", size = 15, colour = col_text) +
  theme_blank

print(title_mot, vp = vplayout(set_pixel["t_sub1", 2:3], 21:468))
print(title_dep, vp = vplayout(set_pixel["t_sub1", 2:3], 489:936))
print(title_dat, vp = vplayout(set_pixel["t_sub1", 2:3], 957:1404))

print(title_mot, vp = vplayout(set_pixel["t_sub1", 2:3], 1425:1872))
print(title_dep, vp = vplayout(set_pixel["t_sub1", 2:3], 1893:2340))
print(title_dat, vp = vplayout(set_pixel["t_sub1", 2:3], 2361:2808))
