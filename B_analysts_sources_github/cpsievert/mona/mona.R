mona <- tempfile(fileext = ".png")
download.file(
  "http://www.clker.com/cliparts/7/a/6/0/12604179921361298927mona_lisa.png",
  mona
)

m <- png::readPNG(mona)
col <- rgb(m[,,1], m[,,2], m[,,3])

d <- dim(m)
x <- rep(seq_len(d[1]), d[2]) 
y <- rep(seq_len(d[2]), each = d[1]) 

library(plotly)
p <- plot_ly(x = x, y = y, marker = list(color = col), type = "scattergl", mode = "markers")
l <- plotly_build(p)

json <- plotly:::to_JSON(l)
writeLines(json, "mona.json")
