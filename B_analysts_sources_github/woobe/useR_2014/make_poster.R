## =============================================================================
## R Script for useR! Poster
## =============================================================================

rm(list=ls())
if (Sys.info()[1] == "Linux") {
  setwd("/media/SUPPORT/Repo/useR_2014/poster")
} else {
  setwd("D:/Repo/useR_2014/poster")
}


## =============================================================================
## Load Packages
## =============================================================================

library(grid)
library(gridExtra)
library(EBImage)
library(ggplot2)
library(rPlotter)
library(extrafont) ## Note: Run font_import() if it has not been done yet

## =============================================================================
## Global Settings
## =============================================================================

set.seed(1234)
new_colours <- extract_colours("http://filmhash.files.wordpress.com/2011/06/reservoir-dogs-051.jpg", 9)
#new_colours2 <- extract_colours("http://www.moviegoods.com/Assets/product_images/1010/477803.1010.A.jpg", 9)

if (FALSE) {
  simulate_colours(new_colours)
  #simulate_colours(new_colours2)
}

## Set Colours
col_header <- new_colours[1]
col_header_line <- new_colours[2]

col_text <- new_colours[1]
col_text2 <- new_colours[2]
col_text3 <- new_colours[9]

col_shade <- new_colours[1] 
col_shade_cm <- new_colours[3]   
col_shade_rcm <- new_colours[7] 

col_link_cm <- new_colours[2]
col_link_rcm <- new_colours[2]

col_file <- new_colours[5]
col_line1 <- new_colours[9]


## =============================================================================
## Global Functions
## =============================================================================

## function for loading image
img2grob <- function(filename = NULL) {
  img <- readImage(filename)
  return(rasterGrob(img, interpolate=TRUE))
}

## blank ggplot2 theme
theme_blank <- create_ggtheme("blank")  ## use rPlotter package


## =============================================================================
## Initiate PDF Creation
## =============================================================================

## Load Extra Fonts
suppressMessages(loadfonts())

## Create blank pdf (the pdf is genearted on Linux Mint 17 (Ubuntu 14.04 LTS) 
## with Ubuntu Fonts - results will vary if different fonts are used
pdf(file = "poster_useR_2014_A0_jofaichow.pdf", height = 33.1, width = 46.8, compress = TRUE,
    family = "Ubuntu", title = "useR! 2014 Poster by Jo-fai Chow") 


## =============================================================================
## Initiate Grid Layout
## =============================================================================

## Set grid (A0 = 1:1.414, use 2000 x 2828 to approximate)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2000, 2828)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

## =============================================================================
## Run each mini scripts
## =============================================================================

source('make_poster_step01_grid.R')
source('make_poster_step02_background.R')
source('make_poster_step03_shade.R')

source('make_poster_step04_header.R')
source('make_poster_step05_sub_header.R')

source('make_poster_step06_t_main.R')
source('make_poster_step07_c_main.R')
source('make_poster_step07b_c_main_annotation.R')

source('make_poster_step08_t_sub1.R')
source('make_poster_step09_p_sub1.R')
source('make_poster_step10_c_sub1.R')

source('make_poster_step11_t_sub2.R')
source('make_poster_step12_p_sub2.R')
source('make_poster_step13_c_sub2.R')

source('make_poster_step14_footer.R')


## =============================================================================
## Print to PDF - Save and Close Device
## =============================================================================

## Save
dev.off()


## =============================================================================
## Embed Font
## =============================================================================

## Embed fonts (very important!)
if (Sys.info()[1] == "Linux") embed_fonts("poster_useR_2014_A0_jofaichow.pdf")

