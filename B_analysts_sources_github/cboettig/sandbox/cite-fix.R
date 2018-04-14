
files <- system("ls *.markdown", intern=TRUE)
library(knitcitations)
library(gsubfn)


         pattern = "\\[cite\\](\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'<>])\\S)+)\\b)\\s?\\[\\/cite\\]"

lapply(files, function(file){
       content <- readLines(file)
       lines <- grep(pattern, content, perl=TRUE)
       if(length(lines > 0)){
         print(file)
         newbib()
         content[lines] <- gsubfn(pattern, citep, backref=-1, content[lines], perl=TRUE)
         bib <- bibliography("markdown")
         if(length(bib)>0){
           content <- c(content, "\n", bib)
           writeLines(content, file)
         }
       }
})



