## =============================================================================
## Mini script - print background
## =============================================================================

bg_img <- qplot(1:1, 1:1, geom = "blank") +
  annotation_custom(img2grob("./image/img_background3.jpg"), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_blank

print(bg_img, vp = vplayout(1:2000, 1:2828))
