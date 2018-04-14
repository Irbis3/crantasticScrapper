## =============================================================================
## Mini script - print text in Sub header
## =============================================================================

## Title CM
title_cm <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 6.75, label = "CrimeMap (Web Application)", size = 21, colour = col_text) +
  annotate("text", x = 5, y = 2.75, label = "http://bit.ly/bib_crimemap", size = 15, colour = col_link_cm) +
  theme_blank

print(title_cm, vp = vplayout(set_pixel["sub_header", 2:3], 21:1404))

## Title RCM
title_rcm <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 6.75, label = "rCrimemap (R Package)", size = 21, colour = col_text) +
  annotate("text", x = 5, y = 2.75, label = "http://bit.ly/rCrimemap", size = 15, colour = col_link_rcm) +
  theme_blank

print(title_rcm, vp = vplayout(set_pixel["sub_header", 2:3], 1425:2808))
