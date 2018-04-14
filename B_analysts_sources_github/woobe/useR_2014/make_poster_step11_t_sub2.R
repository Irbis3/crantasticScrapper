## =============================================================================
## Mini script
## =============================================================================

## CrimeMap
title_sim <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Simple", size = 15, colour = col_text) +
  theme_blank

title_cus <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Customizable", size = 15, colour = col_text) +
  theme_blank

title_mob <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Mobile-Friendly", size = 15, colour = col_text) +
  theme_blank

print(title_sim, vp = vplayout(set_pixel["t_sub2", 2:3], 21:468))
print(title_cus, vp = vplayout(set_pixel["t_sub2", 2:3], 489:936))
print(title_mob, vp = vplayout(set_pixel["t_sub2", 2:3], 957:1404))


## rCrimemap
title_int <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Intuitive", size = 15, colour = col_text) +
  theme_blank

title_rea <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Reactive", size = 15, colour = col_text) +
  theme_blank

title_sel <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 5, label = "Web-Ready", size = 15, colour = col_text) +
  theme_blank

print(title_int, vp = vplayout(set_pixel["t_sub2", 2:3], 1425:1872))
print(title_rea, vp = vplayout(set_pixel["t_sub2", 2:3], 1893:2340))
print(title_sel, vp = vplayout(set_pixel["t_sub2", 2:3], 2361:2808))



