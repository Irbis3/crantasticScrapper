## =============================================================================
## Mini script
## =============================================================================


## =============================================================================
## A
## =============================================================================

img_thrun <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_thrun.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_ai <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_ai.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_udacity <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_udacity.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

## A
print(img_thrun, vp = vplayout(set_pixel["p_sub1", 2:3], 41:162.67))
print(img_ai, vp = vplayout(set_pixel["p_sub1", 2:3], 183.67:305.33))
print(img_udacity, vp = vplayout(set_pixel["p_sub1", 2:3], 326.33:448))


## =============================================================================
## B
## =============================================================================
img_kahle <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_kahle.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_wickham <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_wickham.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_rstudio <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_rstudio.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_kahle, vp = vplayout(set_pixel['p_sub1', 2:3], 509:630.67))
print(img_wickham, vp = vplayout(set_pixel['p_sub1', 2:3], 651.67:773.33))
print(img_rstudio, vp = vplayout(set_pixel['p_sub1', 2:3], 794.33:916))


## =============================================================================
## C
## =============================================================================

img_icon_red <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_icon_red.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_icon_yellow <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_icon_yellow.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_icon_blue <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_icon_blue.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

txt_web <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 4.3, y = 3.6, label = "WEB", size = 9, colour = col_file) +
  theme_blank

txt_json <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 4.5, y = 3.6, label = "JSON", size = 9, colour = col_file) +
  theme_blank

txt_rda <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 4.5, y = 3.6, label = "RDA", size = 9, colour = col_file) +
  theme_blank

print(img_icon_red, vp = vplayout(set_pixel['p_sub1', 2:3], 977:1098.67))
print(txt_web, vp = vplayout(set_pixel['p_sub1', 2:3], 977:1098.67))

print(img_icon_yellow, vp = vplayout(set_pixel['p_sub1', 2:3], 1119.67:1241.33))
print(txt_json, vp = vplayout(set_pixel['p_sub1', 2:3], 1119.67:1241.33))

print(img_icon_blue, vp = vplayout(set_pixel['p_sub1', 2:3], 1262.33:1384))
print(txt_rda, vp = vplayout(set_pixel['p_sub1', 2:3], 1262.33:1384))


## =============================================================================
## D
## =============================================================================

## Ramnath, CrimeMap + interactive

img_ramnath <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_ramnath.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_rMaps5 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_rMaps5.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_rMaps4 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_rMaps4.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_rMaps5, vp = vplayout(set_pixel['p_sub1', 2:3], 1445:1566.67))
print(img_ramnath, vp = vplayout(set_pixel['p_sub1', 2:3], 1587.67:1709.33))
print(img_rMaps4, vp = vplayout(set_pixel['p_sub1', 2:3], 1730.33:1852))

## =============================================================================
## E
## =============================================================================

## rCharts, rMaps, Leaflet
img_rCharts <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_rCharts.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_rMaps1 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_rMaps1.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_leaflet <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_leaflet.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_rCharts, vp = vplayout(set_pixel['p_sub1', 2:3], 1913:2034.67))
print(img_rMaps1, vp = vplayout(set_pixel['p_sub1', 2:3], 2055.67:2177.33))
print(img_leaflet, vp = vplayout(set_pixel['p_sub1', 2:3], 2198.33:2320))


## =============================================================================
## F
## =============================================================================

txt_csv <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 4.5, y = 3.6, label = "CSV", size = 9, colour = col_file) +
  theme_blank

txt_rda <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 4.5, y = 3.6, label = "RDA", size = 9, colour = col_file) +
  theme_blank

print(img_icon_red, vp = vplayout(set_pixel['p_sub1', 2:3], 2381:2502.67))
print(txt_web, vp = vplayout(set_pixel['p_sub1', 2:3], 2381:2502.67))

print(img_icon_yellow, vp = vplayout(set_pixel['p_sub1', 2:3], 2523.67:2645.33))
print(txt_csv, vp = vplayout(set_pixel['p_sub1', 2:3], 2523.67:2645.33))

print(img_icon_blue, vp = vplayout(set_pixel['p_sub1', 2:3], 2666.33:2788))
print(txt_rda, vp = vplayout(set_pixel['p_sub1', 2:3], 2666.33:2788))

