background <- magick::image_blank(500, 500, col = "blue")
speech <- magick::image_read("speech.svg")
magick::image_composite(background, speech)
# ugly

speech <- magick::image_read(rsvg::rsvg_raw("speech.svg"))
magick::image_composite(background, speech)
# yay

speech <- magick:::image_rsvg("speech.svg")
magick::image_composite(background, speech)
# yay
