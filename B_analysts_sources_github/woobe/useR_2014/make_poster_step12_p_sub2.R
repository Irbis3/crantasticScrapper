## =============================================================================
## Mini script
## =============================================================================

## Mini Images
img_thrun <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_thrun_semi.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank


## =============================================================================
## A
## =============================================================================

## 1 2 3, push button, crimemap
img_cm_123 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_cm_123.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_cm_button <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_cm_button.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_cm_output <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_cm_output.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_cm_123, vp = vplayout(set_pixel['p_sub2', 2:3], 41:162.67))
print(img_cm_button, vp = vplayout(set_pixel['p_sub2', 2:3], 183.67:305.33))
print(img_cm_output, vp = vplayout(set_pixel['p_sub2', 2:3], 326.33:448))


## =============================================================================
## B
## =============================================================================

## CrimeMap1, 2, 3
img_cm_adv <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_cm_adv.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_cmap1 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_cmap1.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_cmap_x9 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_cmap_x9.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_cm_adv, vp = vplayout(set_pixel['p_sub2', 2:3], 509:630.67))
print(img_cmap1, vp = vplayout(set_pixel['p_sub2', 2:3], 651.67:773.33))
print(img_cmap_x9, vp = vplayout(set_pixel['p_sub2', 2:3], 794.33:916))


## =============================================================================
## C
## =============================================================================

## desktop, iPhone, Nexus 7
img_nexus7a <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_nexus7a.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_nexus7b <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_nexus7b.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_nexus7c <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_nexus7c.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_nexus7a, vp = vplayout(set_pixel['p_sub2', 2:3], 977:1098.67))
print(img_nexus7b, vp = vplayout(set_pixel['p_sub2', 2:3], 1119.67:1241.33))
print(img_nexus7c, vp = vplayout(set_pixel['p_sub2', 2:3], 1262.33:1384))


## =============================================================================
## D
## =============================================================================

## iPad, mouse, people looking at map

img_mouse <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_mouse.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_rcmap_hand <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_rcmap_hand.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_nav <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_nav.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_mouse, vp = vplayout(set_pixel['p_sub2', 2:3], 1445:1566.67))
print(img_nav, vp = vplayout(set_pixel['p_sub2', 2:3], 1587.67:1709.33))
print(img_rcmap_hand, vp = vplayout(set_pixel['p_sub2', 2:3], 1730.33:1852))



## =============================================================================
## E
## =============================================================================

## zoom out / no zoom / zoom in
 
img_zoom1 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_zoom1.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_zoom2 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_zoom2.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_zoom3 <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_zoom3.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_zoom1, vp = vplayout(set_pixel['p_sub2', 2:3], 1913:2034.67))
print(img_zoom2, vp = vplayout(set_pixel['p_sub2', 2:3], 2055.67:2177.33))
print(img_zoom3, vp = vplayout(set_pixel['p_sub2', 2:3], 2198.33:2320))



## =============================================================================
## F
## =============================================================================

## chrome, firefox, safari
img_chrome <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_chrome.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_firefox <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_firefox.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

img_safari <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_safari.png"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_blank

print(img_chrome, vp = vplayout(set_pixel['p_sub2', 2:3], 2381:2502.67))
print(img_firefox, vp = vplayout(set_pixel['p_sub2', 2:3], 2523.67:2645.33))
print(img_safari, vp = vplayout(set_pixel['p_sub2', 2:3], 2666.33:2788))

