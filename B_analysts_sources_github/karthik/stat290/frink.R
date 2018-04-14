frink <- image_read("https://jeroen.github.io/images/frink.png")
frink
image_border(image_background(frink, "hotpink"), "#000080", "20x10")
image_trim(frink)
image_crop(frink, "100x150+50")
image_scale(frink, "500") # width: 300px
image_rotate(frink, 45)
image_flip(frink)
image_flop(frink)

# Apply filters

image_blur(frink, 10, 5)
image_noise(frink)
image_charcoal(frink)
image_oilpaint(frink)

# Annotate images with text

image_annotate(frink, "I like R!", size = 70, gravity = "southwest", color = "green")
image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
               degrees = 60, location = "+50+100")


# Earth, extract frames

earth <- image_read("https://jeroen.github.io/images/earth.gif") %>%
  image_scale("200x") %>%
  image_quantize(128)

length(earth)
earth

# Work with Gifs

rev(earth) %>% 
  image_flip() %>% 
  image_annotate("meanwhile in Australia", size = 20, color = "white")

# Run with Shiny

library(shiny)
library(magick)
runGitHub("jeroen/shinymagick")