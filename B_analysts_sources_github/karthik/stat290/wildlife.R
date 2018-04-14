files <- list.files("wildlife/",
                    full.names = TRUE)

text <- image_read(files) %>%
  image_convert(type = 'grayscale') %>%
  image_crop("1890x32") %>%
  image_reducenoise(radius = 5) %>%
  image_append(stack = T) %>%
  image_ocr(options = list(tessedit_char_whitelist = "/0123456789:-°ACMP"))

cat(text)