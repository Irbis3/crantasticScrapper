
files <- system("ls *.markdown", intern=TRUE)

require(knitr)
require(gsubfn)


makeuri <- function(url){
  download.file(url, destfile="temp.png")
  uri <- image_uri("temp.png")
  system("rm temp.png")
  paste("![](", uri, ")", sep="")
}

lapply(files, 
       function(file){
        content <- readLines(file)
        content <- gsubfn("!\\[.*\\]\\((http:.*[.png|.jpg])\\)", makeuri, content)
        writeLines(content, file)
      })
      
