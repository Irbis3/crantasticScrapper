library("rvest")
library("RSelenium")



# https://stackoverflow.com/questions/29861117/r-rvest-scraping-a-dynamic-ecommerce-page
rD <- rsDriver()
remDr <- rD[["client"]]

link <- "https://www.pexels.com/search/bird/"

# open the webpage
remDr$navigate("https://www.pexels.com/search/bird/")

# scroll down
for(i in 1:20){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"),
                      args = list("dummy"))
  # be nice and wait
  Sys.sleep(3)    
}

# https://www.pexels.com/faq/

page_content <- remDr$getPageSource() 

get_link_from_src <- function(node){
  xml2::xml_attrs(node)["src"] %>%
    as.character() %>%
    stringr::str_replace("\\?h.*", "")
    
}

xtract_pic_links <- function(source) {
  css <- '.photo-item__img'
  read_html(source[[1]]) %>%
  html_nodes(css) %>%
  purrr::map_chr(get_link_from_src)    
}

links <- xtract_pic_links(page_content)

#https://stackoverflow.com/questions/46127818/executescript-from-rselenium-gives-error-with-args
# save
dir.create("birds")
save_pic <- function(url){
  Sys.sleep(1)
  name <- stringr::str_replace(url, ".*\\/", "")
  
  try(magick::image_read(url) %>%
        magick::image_write(paste0("birds/", name)),
      silent = TRUE)
}

purrr::walk(links[1:188], save_pic)

dir.create("formatted_pics")

format_image <- function(path){
  image <- magick::image_read(path)
  info <- magick::image_info(image)
  
  
  
  direction <- ifelse(info$height > info$width,
                      "height", "width")
  scale_number <- as.numeric(info[direction]/500)
  image <- magick::image_scale(image, paste0(info["width"]/scale_number,
                                             "x", 
                                             info["height"]/scale_number))
  newinfo <- magick::image_info(image)
  
  # colours
  colours <- try(rPlotter::extract_colours(path), silent = TRUE)
  
  # one pic at least was problematic but it was bicolor and actually
  # a gif
  if(!is(colours, "try-error")){
    colour <- sample(x = colours, size = 1)
    
    image <- magick::image_border(image, colour, paste0((500-newinfo$width)/2, "x",
                                                        (500-newinfo$height)/2))
    info <- magick::image_info(image)
    # odd numbers out!
    if(info$height/2 != floor(info$height/2)){
      image <- magick::image_crop(image, "0x500+0")
    }
    if(info$width/2 != floor(info$width/2)){
      image <- magick::image_crop(image, "500x0+0")
    }
    magick::image_write(image,
                        stringr::str_replace(path, "birds", "formatted_pics"))
  }
  
  tibble::tibble(path = path,
                 colour = colour)
}

banner_col <- purrr::map_df(dir("birds", full.names = TRUE), format_image)

# we need smaller images
reduce_image <- function(path){
  magick::image_read(path) %>%
    magick::image_scale("71x71!") %>%
    magick::image_write(path)
}

purrr::walk(dir("formatted_pics", full.names = TRUE),
            reduce_image)

# now work on getting the hue and value for all pics
# create a data.frame with path, hue, value 
get_values <- function(path){
  # get main color
  colour <- banner_col$colour[banner_col$path == path]
  
  # translate it
  rgb <- colorspace::hex2RGB(colour)
  values <- grDevices::rgb2hsv(t(rgb@coords))
  
  tibble::tibble(path = path,
                 hue = values[1,1],
                 saturation = values [2,1],
                 value = values[3,1])
}

# all values
pics_col <- purrr::map_df(dir("formatted_pics", full.names = TRUE)[1:147],
                          get_values)

# now we want a location for each
# the dimensions are 18*54. 
no_rows <- 7
no_cols <- 21
pics_col <- dplyr::arrange(pics_col, hue)
pics_col <- dplyr::mutate(pics_col, column = rep(1:no_cols, each = no_rows))
pics_col <- dplyr::group_by(pics_col, column) %>%
  dplyr::arrange(hue) %>%
  dplyr::mutate(row = 1:no_rows) %>%
  dplyr::ungroup()

pics_col <- dplyr::arrange(pics_col, column, row)

# collage time

make_column <- function(i, files, no_rows){
  magick::image_read(files[(i*no_rows+1):((i+1)*no_rows)]) %>%
    magick::image_append(stack = TRUE) %>%
    magick::image_write(paste0("cols/", i, ".jpg"))
}
dir.create("cols")
purrr::walk(0:(no_cols-1), make_column, files = dir("formatted_pics", full.names = TRUE),
            no_rows = no_rows)


magick::image_read(dir("cols/", full.names = TRUE)) %>%
  magick::image_append(stack = FALSE) %>%
  magick::image_write("banner.jpg")