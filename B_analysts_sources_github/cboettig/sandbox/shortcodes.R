require(Rflickr)

#rssinpage, cetsEmbedRSS, flickr-gallery

# Get a list of all files
files <- system("ls *.markdown", intern=T)


#fix_citations(files)

#files <- "test.markdown"

# Show me a list of tags in use
active_tags <- function(files){
keys <- lapply(files, function(file){
  content <- readLines(file)
  lines <- grep("\\[\\/.*?\\]", content)
  unique(gsub(".*(\\[\\/.*?\\]).*", "\\1", content[lines]))
})
message(unique(unlist(keys)))
}



jekyllformat <- function(files, cite=TRUE){
active_tags(files)
fix_codetags(files)
fix_flickr(files)
fix_equations(files)
add_redirects(files)
if(cite)
  fix_citations(files)
rename_private(files) # changes file names, must run last 
}

# Update syntax highlighted code block tags
fix_codetags <- function(files){
lapply(files, function(file){
  content <- readLines(file)
  content <- gsub("\\[code\\]^(\\()", "\n```\n", content) #code not followed by (, eg. [code](link) will be left alone
  content <- gsub("\\[\\/code\\]", "\n```\n", content)
  content <- gsub("\\[code lang *= *['\"](\\w+)['\"]\\]", "\n```\\1\n", content)
  content <- gsub("\\[sourcecode\\]", "\n```\n", content)
  content <- gsub("\\[\\/sourcecode\\]", "\n```\n", content)
  content <- gsub("\\[sourcecode language *= *['\"](\\w+)['\"]\\]", "\n```\\1\n", content)
  content <- gsub("\\[sourcecode lang *= *['\"](\\w+)['\"]\\]", "\n```\\1\n", content)
  content <- gsub("\\[source\\]", "```\n", content)
  content <- gsub("\\[\\/source\\]", "```\n", content)
  content <- gsub("\\[source language *= *['\"](\\w+)['\"]\\]", "\n```\\1\n", content)
  content <- gsub("\\[source lang *= *['\"](\\w+)['\"]\\]", "\n```\\1\n", content)

  content <- gsub("\\[php\\]", "\n```php\n", content)
  content <- gsub("\\[\\/php\\]", "\n```\n", content)
  content <- gsub("\\[html\\]", "\n```html\n", content)
  content <- gsub("\\[\\/html\\]", "\n```\n", content)
  content <- gsub("\\[bash\\]", "\n```bash\n", content)
  content <- gsub("\\[\\/bash\\]", "\n```\n", content)
  content <- gsub("\\[css\\]", "\n```css\n", content)
  content <- gsub("\\[\\/css\\]", "\n```\n", content)
  content <- gsub("\\[xml\\]", "\n```xml\n", content)
  content <- gsub("\\[\\/xml\\]", "\n```\n", content)
  content <- gsub("<code>", "`", content)
  content <- gsub("<\\/code>", "`", content)
  writeLines(content, file)
})
}


# Update flickr tags
fix_flickr <- function(files){
  require(Rflickr)

  auth=getOption("flickr_tok") 
  api_key=getOption("flickr_api_key") 
  secret=getOption("flickr_secret")


  flickr_url <- function(id){
    sizes_url <- flickr.photos.getSizes(secret=secret, auth_token=auth,
                                        api_key=api_key, photo_id=id)
    n <- length(sizes_url) # get original size
    orig_size_url <- sizes_url[[n-1]][[4]]
    paste("![](", orig_size_url, ")\n" )
  }
  
  lapply(files, function(file){
    content <- readLines(file)
    content <- gsubfn("\\[flickr.*?\\]http://flickr.com\\/photos\\/46456847@N08\\/(\\d+)?\\[\\/flickr\\]", flickr_url, content)
    content <- gsubfn("\\[flickr.*?\\](\\d+)?\\[\\/flickr\\]", flickr_url, content)
    writeLines(content, file)
  })
}


require(gsubfn)
# Update equation tags
fix_equations <- function(files){
  lapply(files, function(file){
    content <- readLines(file)
    display <- "$$\\2$$"
    inline <- "$\\2$"
    content <- gsubfn("^(\\\\\\[)$", "$$", content)
    content <- gsubfn("^(\\\\\\])$", "$$", content)

    content <- gsubfn("(\\\\\\[)(.+)??(\\\\\\])", display, content)
    content <- gsubfn("(\\\\\\( ?)(.+)??( ?\\\\\\))", inline, content)
    content <- gsubfn("(\\[latex\\])(.+)??(\\[\\/latex\\])", display, content)
    content <- gsubfn("(\\$latex)(.+)??(\\$)", inline, content)
    writeLines(content, file)
  })
}
# Consider adding a footnote that post has been converted from a Wordpress site.  


# Add redirects so that old links still work  (tested on test.markdown) 
add_redirects <- function(files){
lapply(files, function(file){
       content <- readLines(file)
       content <- gsub("wordpress_id: (\\d+)", "redirects: [/wordpress/archives/\\1, /archives/\\1]", content)
       writeLines(content, file)
})
}

# Replace citations (tested on test.markdown) 
require(knitcitations)
require(gsubfn)

fix_citations <- function(files){
lapply(files, function(file){
       content <- readLines(file)
       lines <- grep("\\[\\/cite\\]", content)
       if(length(lines>0)){
         cleanbib()
         pattern <- "\\[cite\\]([a-z0-9\\/\\.\\-]+)?\\[\\/cite\\]"
         content <- gsubfn(pattern, citep, content)
         bib <- format(bibliography("html"), "html")
         if(length(bib)>0){
           content <- c(content, "## References\n", bib)
           writeLines(content, file)
         }
       }
})
}

rename_private <- function(files){
lapply(files, function(file){
       content <- readLines(file)
       private <- grep("published: false", content)
       if(length(private) > 0){
         writeLines(content, paste("private", file, sep="_"))
         system(paste("rm", file)) # remove the original file
       }
})
}



fix_iframes <- function(files){
  lapply(files, function(file){
       content <- readLines(file)
       pattern <- "[iframe (http://+? ) (\\d*) (\\d*)]"
       content <- gsubfn(pattern, "<iframe src=\"\\1\" width=\\2 height=\\3>", content)
       writeLines(content, file)
  })
}



migrate_disqus <- function(files){
  require(gsubfn)
  addresses <- 
    sapply(files, function(file){
         file <- paste("originals", file, sep="/")
         new_url <- gsub("\\.markdown$", "", file)
         new_url <- gsubfn("(\\d+)-", "\\1/", new_url)
         new_url <- paste("http://carlboettiger.info/", new_url, ".html", sep="")

         content <- readLines(file)
         lines <- grep("wordpress_id: (\\d+)", content)
         orig_url <- gsub(".*wordpress_id: (\\d+).*", "http://carlboettiger.info/wordpress/archives/\\1", content[lines])
         paste(orig_url, new_url, sep=", ")
  
  })
  write.csv(addresses, "~/Desktop/migrate_disqus.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  invisible(addresses)
}





# Testing / searching for tags in use
pfind <- function(pattern){
  tags <- lapply(files, function(file){
                 content <- readLines(file)
                 lines <- grep(pattern, content)
                 content[lines]
#                 unique(gsub(paste(".*(", pattern, ").*", sep=""), "\\1", content[lines]))
  })
  unique(unlist(tags))
}

#pfind("\\\\\\\\")
#pfind("\\$latex .*?\\$")

