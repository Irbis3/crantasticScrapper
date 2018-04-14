# My preferred defaults (may be changed in individual chunks)
opts_chunk$set(tidy=FALSE, warning=FALSE, message=FALSE, cache=TRUE, 
               comment=NA, verbose=TRUE, fig.width=6, fig.height=4)
 
# Name the cache path and fig.path based on filename...
opts_chunk$set(fig.path = paste("figure/",
                                gsub(".Rmd", "", knitr:::knit_concord$get('infile')),
                                "-", sep=""),
               cache.path = paste("cache/", 
                                  gsub(".Rmd", "", knitr:::knit_concord$get('infile') ), 
                                "/", sep=""))
 
# Set plotting to bw plot default, but with transparent background elements.  
# Note transparency requires the panel.background, plot.background, and device background all be set!
library(ggplot2) 
theme_set(theme_bw(base_size=12))
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA))
opts_chunk$set(dev.args=list(bg="transparent"))
 
# Set a color-blind friendly pallette
# adapted from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
