## =============================================================================
## Mini script
## =============================================================================

## A
sub1_a <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Only requires three inputs:', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = '(1) Location, (2) Start Date', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'and (3) Length of Analysis.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Even my parents can do it!', size = 10, colour = col_text2) +
  theme_blank

## B
sub1_b <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Advanced settings are just a', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'few clicks away. Tweak the', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'heatmaps to your liking.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'So, what is your favorite color?', size = 10, colour = col_text2) +
  theme_blank

## C
sub1_c <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Shiny automatically adjusts', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'display layout based on', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'screen resolution.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'You can also theme it up with CSS!', size = 10, colour = col_text2) +
  theme_blank

## D
sub1_d <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Navigate and zoom in/out', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'like using any digital map', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'with mouse or touchscreen.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Explore large areas with ease!', size = 10, colour = col_text2) +
  theme_blank

## E
sub1_e <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Colors of 2D density plots', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'automatically adjusted and', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'updated after zooming.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Truly reactive visualization!', size = 10, colour = col_text2) +
  theme_blank

## F
sub1_f <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Heatmaps can be saved and', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'viewed as HTML files. Easy', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'to create, publish and share.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Awesome rCharts workflow!', size = 10, colour = col_text2) +
  theme_blank

print(sub1_a, vp = vplayout(set_pixel["c_sub2", 2:3], 21:468))
print(sub1_b, vp = vplayout(set_pixel["c_sub2", 2:3], 489:936))
print(sub1_c, vp = vplayout(set_pixel["c_sub2", 2:3], 957:1404))

print(sub1_d, vp = vplayout(set_pixel["c_sub2", 2:3], 1425:1872))
print(sub1_e, vp = vplayout(set_pixel["c_sub2", 2:3], 1893:2340))
print(sub1_f, vp = vplayout(set_pixel["c_sub2", 2:3], 2361:2808))


