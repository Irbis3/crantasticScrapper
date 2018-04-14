## =============================================================================
## Mini script
## =============================================================================

## A
sub1_a <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = '"If you want to learn sth', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'new, find an intersting', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'problem and dive into it!"', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Sebastian Thrun - Intro to A.I.', size = 10, colour = col_text2) +
  theme_blank

## B
sub1_b <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'ggmap, ggplot2, grid, plyr,', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'markdown, png, RCurl,', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'jsonlite, shiny, shinyapps', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'D. Kahle, H. Wickham, RStudio ...', size = 10, colour = col_text2) +
  theme_blank

## C
sub1_c <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Street level crime data', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'downloaded as JSON and ', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'converted into data frame.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Source: data.police.uk', size = 10, colour = col_text2) +
  theme_blank



## D
sub1_d <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = '"You can create interactive', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'heatmaps using Leaflet JS', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'with rMaps, interested?"', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Ramnath Vaidyanathan', size = 10, colour = col_text2) +
  theme_blank

## E
sub1_e <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'ggmap, dplyr, plyr,', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'rCharts, rjson, rMaps,', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'Leaflet (JavaScript)', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'R. Vaidyanathan, V. Agafonkin, ...', size = 10, colour = col_text2) +
  theme_blank

## F
sub1_f <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5, y = 7.50, label = 'Data batch downloaded as', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 5.75, label = 'CSV, converted and cached', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 4.00, label = 'to streamline the process.', size = 12, colour = col_text) +
  annotate("text", x = 5, y = 2.25, label = 'Source: data.police.uk', size = 10, colour = col_text2) +
  theme_blank

print(sub1_a, vp = vplayout(set_pixel["c_sub1", 2:3], 21:468))
print(sub1_b, vp = vplayout(set_pixel["c_sub1", 2:3], 489:936))
print(sub1_c, vp = vplayout(set_pixel["c_sub1", 2:3], 957:1404))

print(sub1_d, vp = vplayout(set_pixel["c_sub1", 2:3], 1425:1872))
print(sub1_e, vp = vplayout(set_pixel["c_sub1", 2:3], 1893:2340))
print(sub1_f, vp = vplayout(set_pixel["c_sub1", 2:3], 2361:2808))

