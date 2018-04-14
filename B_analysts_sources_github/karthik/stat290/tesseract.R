# Magick

library(tesseract)
eng <- tesseract("eng")
text <- tesseract::ocr("http://jeroen.github.io/images/testocr.png", engine = eng)
cat(text)


# Extract words and probabilities on their accuracy

results <- tesseract::ocr_data("http://jeroen.github.io/images/testocr.png", engine = eng)
print(results, n = 20)

tesseract_info()
tesseract_download('nld')


input <- image_read("https://i.imgur.com/ISZ5vfG.jpg")
text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
  image_trim(fuzz = 40) %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr() 

cat(text)

text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
  image_trim(fuzz = 40) %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr() 

cat(text)