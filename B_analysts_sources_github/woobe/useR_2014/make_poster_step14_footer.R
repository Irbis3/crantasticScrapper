## =============================================================================
## Mini script - print Header
## =============================================================================


## =============================================================================

## left (useR logo)
img_useR <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_useR.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_blank
#print(shade, vp = vplayout(set_pixel["footer",2:3], 21:200))
print(img_useR, vp = vplayout(set_pixel["footer",2:3], 21:200))

## left (useR Text)
txt_useR <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 1, y = 8, label = "The R User Conference", size = 10, colour = col_text, hjust=0) +
  annotate("text", x = 1, y = 5, label = "June 30 - July 3, 2014", size = 10, colour = col_text, hjust=0) +
  annotate("text", x = 1, y = 2, label = "UCLA, Los Angeles", size = 10, colour = col_text, hjust=0) +
  theme_blank
print(txt_useR, vp = vplayout(set_pixel["footer",2:3], 181:1000))



## =============================================================================

## Text (Left)
txt_tw <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5.0, y = 7, label = "tweet to @matlabulous", size = 11, colour = col_text2) +
  annotate("text", x = 5.0, y = 3, label = "jofai.chow@gmail.com", size = 11, colour = col_text2) +
  theme_blank
print(txt_tw, vp = vplayout(set_pixel["footer",2:3], 489:1404))

## Text (Right)
txt_git <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5.0, y = 7, label = "github.com/woobe", size = 11, colour = col_text2) +
  annotate("text", x = 5.0, y = 3, label = "bl.ocks.org/woobe", size = 11, colour = col_text2) +
  theme_blank
print(txt_git, vp = vplayout(set_pixel["footer",2:3], 1425:2340))

## Text (Middle)
txt_web <- qplot(1:9, 1:9, geom = "blank") +
  annotate("text", x = 5.0, y = 5, label = "jofaichow.co.uk", size = 19, colour = col_text) +
  theme_blank
print(txt_web, vp = vplayout(set_pixel["footer",2:3], 21:2808))

## Image Twitter
img_twitter <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_twitter.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

## Image GitHub
img_github <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_github.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

## Image CM Logo
img_logo <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_logo1.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

## Image JC
img_jchow <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_jchow.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank


print(img_twitter, vp = vplayout(set_pixel["footer",2:3], 489:936))
print(img_jchow, vp = vplayout(set_pixel["footer",2:3], 957:1404))
print(img_logo, vp = vplayout(set_pixel["footer",2:3], 1425:1872))
print(img_github, vp = vplayout(set_pixel["footer",2:3], 1893:2340))


## =============================================================================

## Right (scienceposters logo)
img_scienceposters <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_scienceposters.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_blank
#print(shade, vp = vplayout(set_pixel["footer",2:3], 2360:2808))
print(img_scienceposters, vp = vplayout(set_pixel["footer",2:3], 2360:2808))

