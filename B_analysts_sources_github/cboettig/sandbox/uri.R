
files <- system("ls *.markdown", intern=TRUE)

require(knitr)
require(gsubfn)


url_pattern <- "https?://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"
chunk_name <- "[-\\s\\da-zA-Z]*"
pattern <- paste("!\\[", chunk_name, "\\]\\((", url_pattern, " ?)\\)", sep="")

makeuri <- function(url){
  download.file(url, destfile="temp.png")
  uri <- image_uri("temp.png")
  system("rm temp.png")
  paste("![](", uri, ")", sep="")
}

lapply(files, 
       function(file){
        content <- readLines(file)
        content <- gsubfn(pattern, makeuri, content)
        writeLines(content, file)
})
      
