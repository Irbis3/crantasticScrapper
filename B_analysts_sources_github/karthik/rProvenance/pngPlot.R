pngPlot =
  #
  # example
  # o = pngPlot({ plot(1:10); abline(h = 5, col = "red")},
  #              meta = list(data = 1:10))
  # z = readPNG(o, meta = TRUE)
  # z$metadata
  # unserialize(base64Decode(z$metadata["data"], "raw"))
  #
function(code, file = character(), ...,
         meta = getSessionProvInfo(substitute(code)))
{
  force(meta)  # ensure random seed is caught before code is evaluated.
  
  toRaw = length(file) == 0 || is.na(file) || file == ""

  if(toRaw)
    file = tempfile()
  
  grDevices::png(file, ...)
  on.exit(dev.off())
  force(code)
  dev.off()
  on.exit()

  data = readPNG(file)

  if(!toRaw)
    meta[["file"]] = as.character(file)
  
  ans = writePNG(data, if(toRaw) raw() else file, meta = meta)
  invisible(if(toRaw)
              structure(ans, meta = meta)
            else
              file)
}

