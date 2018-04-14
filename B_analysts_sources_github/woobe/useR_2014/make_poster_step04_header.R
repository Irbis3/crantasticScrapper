## =============================================================================
## Mini script - print Header
## =============================================================================

title_header <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.25, label = "Interactive Spatial Data Visualization", size = 32, colour = col_header) +
  annotate("text", x = 5, y = 4.35, label = "Exploring Two Different Options with Case Studies based on UK Crime Data", size = 14, colour = col_header) +
  annotate("segment", x = 3, xend = 7, y = 2.85, yend = 2.85, size = 2, colour = col_header_line) +
  annotate("text", x = 5, y = 1.6, label = "Jo-fai Chow, Hydroinformatics EngD Candidate, University of Exeter, UK", size = 12, colour = col_header) +
  theme_blank

print(title_header, vp = vplayout(set_pixel["header",2:3], 21:2808))


