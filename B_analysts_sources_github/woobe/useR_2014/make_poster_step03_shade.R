## =============================================================================
## Mini script - print shade
## =============================================================================

## Create shade gg objects
shade <- qplot(1:1, 1:1, geom = "blank") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           colour = "transparent", fill = col_shade, alpha = 0.75, size = 1) +
  theme_blank

shade_cm <- qplot(1:1, 1:1, geom = "blank") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           colour = "transparent", fill = col_shade_cm, alpha = 0.35, size = 1) +
  theme_blank

shade_rcm <- qplot(1:1, 1:1, geom = "blank") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           colour = "transparent", fill = col_shade_rcm, alpha = 0.35, size = 1) +
  theme_blank


## =============================================================================
## Left Side - CrimeMap
## =============================================================================

## Print sub headers
print(shade_cm, vp = vplayout(set_pixel["sub_header",2:3], 21:1404))

## Print title (main)
print(shade_cm, vp = vplayout(set_pixel["t_main",2:3], 21:702))
print(shade_cm, vp = vplayout(set_pixel["t_main",2:3], 723:1404))

## Print content (main)
print(shade_cm, vp = vplayout(set_pixel["c_main",2:3], 21:702))
print(shade_cm, vp = vplayout(set_pixel["c_main",2:3], 723:1404))

## Print title (sub1)
print(shade_cm, vp = vplayout(set_pixel["t_sub1",2:3], 21:468))
print(shade_cm, vp = vplayout(set_pixel["t_sub1",2:3], 489:936))
print(shade_cm, vp = vplayout(set_pixel["t_sub1",2:3], 957:1404))

## Print pic (sub1)
print(shade_cm, vp = vplayout(set_pixel["p_sub1",2:3], 21:468))
print(shade_cm, vp = vplayout(set_pixel["p_sub1",2:3], 489:936))
print(shade_cm, vp = vplayout(set_pixel["p_sub1",2:3], 957:1404))

## Print content (sub1)
print(shade_cm, vp = vplayout(set_pixel["c_sub1",2:3], 21:468))
print(shade_cm, vp = vplayout(set_pixel["c_sub1",2:3], 489:936))
print(shade_cm, vp = vplayout(set_pixel["c_sub1",2:3], 957:1404))

## Print title (sub2)
print(shade_cm, vp = vplayout(set_pixel["t_sub2",2:3], 21:468))
print(shade_cm, vp = vplayout(set_pixel["t_sub2",2:3], 489:936))
print(shade_cm, vp = vplayout(set_pixel["t_sub2",2:3], 957:1404))

## Print pic (sub2)
print(shade_cm, vp = vplayout(set_pixel["p_sub2",2:3], 21:468))
print(shade_cm, vp = vplayout(set_pixel["p_sub2",2:3], 489:936))
print(shade_cm, vp = vplayout(set_pixel["p_sub2",2:3], 957:1404))

## Print content (sub2)
print(shade_cm, vp = vplayout(set_pixel["c_sub2",2:3], 21:468))
print(shade_cm, vp = vplayout(set_pixel["c_sub2",2:3], 489:936))
print(shade_cm, vp = vplayout(set_pixel["c_sub2",2:3], 957:1404))


## =============================================================================
## Left Side - rCrimemap
## =============================================================================

## Print sub headers
print(shade_rcm, vp = vplayout(set_pixel["sub_header",2:3], 1425:2808))

## Print title (main)
print(shade_rcm, vp = vplayout(set_pixel["t_main",2:3], 1425:2106))
print(shade_rcm, vp = vplayout(set_pixel["t_main",2:3], 2127:2808))

## Print content (main)
print(shade_rcm, vp = vplayout(set_pixel["c_main",2:3], 1425:2106))
print(shade_rcm, vp = vplayout(set_pixel["c_main",2:3], 2127:2808))

## Print title (sub1)
print(shade_rcm, vp = vplayout(set_pixel["t_sub1",2:3], 1425:1872))
print(shade_rcm, vp = vplayout(set_pixel["t_sub1",2:3], 1893:2340))
print(shade_rcm, vp = vplayout(set_pixel["t_sub1",2:3], 2361:2808))

## Print pic (sub1)
print(shade_rcm, vp = vplayout(set_pixel["p_sub1",2:3], 1425:1872))
print(shade_rcm, vp = vplayout(set_pixel["p_sub1",2:3], 1893:2340))
print(shade_rcm, vp = vplayout(set_pixel["p_sub1",2:3], 2361:2808))

## Print content (sub1)
print(shade_rcm, vp = vplayout(set_pixel["c_sub1",2:3], 1425:1872))
print(shade_rcm, vp = vplayout(set_pixel["c_sub1",2:3], 1893:2340))
print(shade_rcm, vp = vplayout(set_pixel["c_sub1",2:3], 2361:2808))

## Print title (sub2)
print(shade_rcm, vp = vplayout(set_pixel["t_sub2",2:3], 1425:1872))
print(shade_rcm, vp = vplayout(set_pixel["t_sub2",2:3], 1893:2340))
print(shade_rcm, vp = vplayout(set_pixel["t_sub2",2:3], 2361:2808))

## Print pic (sub2)
print(shade_rcm, vp = vplayout(set_pixel["p_sub2",2:3], 1425:1872))
print(shade_rcm, vp = vplayout(set_pixel["p_sub2",2:3], 1893:2340))
print(shade_rcm, vp = vplayout(set_pixel["p_sub2",2:3], 2361:2808))

## Print content (sub2)
print(shade_rcm, vp = vplayout(set_pixel["c_sub2",2:3], 1425:1872))
print(shade_rcm, vp = vplayout(set_pixel["c_sub2",2:3], 1893:2340))
print(shade_rcm, vp = vplayout(set_pixel["c_sub2",2:3], 2361:2808))

