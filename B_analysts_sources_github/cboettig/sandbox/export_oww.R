require(httr)
require(gsubfn)

oww_to_md <- function(pages, user="Carl_Boettiger",  baseurl = "http://openwetware.org"){
  # Store filenames (with dates) and categories for each page, for reference later
  filenames <- get_filenames(pages)
  categories <- get_categories(pages)
  ## Use the API function to extract the content
  parsed <- export_oww(pages, user, baseurl)
  ## Add the baseurl back into all the links
  htmls <- add_baseurl(parsed)
  ## Create YAML header text
  headers <- add_header_txt(parsed, categories)
  ## Use pandoc to convert the html to markdown
  mds <- html_to_md(htmls, filenames) 
  ## Delete the OWW header info we don't want in the markdown
  mds <- clean_md(mds)
  ## Stick headers onto markdown files and name them according to filenames
  write_mds(mds, headers, filenames) 
}

get_filenames <- function(pages){
  lapply(pages, function(p){
    p <- gsub(" ", "_", p)
    filename <- gsub("/", "-", p)
    filename <- gsub("(.*)-(\\d+-\\d+-\\d+)", "\\2-\\1", filename)
  })
}

#' Take a list of page titles to a user's OWW notebooks and export them as Jekyll markdown entries.  
get_categories <- function(pages){
  lapply(pages, function(p){
    p <- gsub("_", " ", p)
    filename <- gsub("/", "-", p)
    category <- gsub("(.*)-(\\d+-\\d+-\\d+)", "\\1", filename)
  })
}


#' Take a list of page titles to a user's OWW notebooks and export them as Jekyll markdown entries.  
export_oww <- function(pages, user="Carl_Boettiger",  baseurl=  "http://openwetware.org"){
  lapply(pages, function(p){
    p <- gsub(" ", "_", p)
    page <- paste("User:", user, "/Notebook/", p, sep="")
    out <- wiki_parse(page, baseurl=baseurl)
  })
}

add_baseurl <- function(parsed,  baseurl = "http://openwetware.org"){
  lapply(parsed, function(out){
    html <- out$parse$text[[1]]
    html <- gsubfn("src=\"/images/", paste("src=\"", 
                   baseurl,"/images/", sep=""), html)
    html <- gsubfn("href=\"/wiki/", paste("src=\"", 
                   baseurl,"/wiki/", 
                    sep=""), html)
  })
}

add_header_txt <- function(parsed, mycategories){
  sapply(1:length(parsed), function(i){
    out <- parsed[[i]]
    category <- mycategories[[i]]
    if(length(out$parse$categories)>0){
      categories <- sapply(out$parse$categories, function(x) x$`*`)
      gsub("_", " ", categories)
      ## Standarize some of my tags, called "categories" on OWW
      tags <- paste("tags: ", "[", 
                          paste0(categories, collapse = ", "),
                          "]", sep="")  
    } else {
      tags <- ""
    }
    
    header <- c("---", 
                "layout: post",
                tags, 
                paste("categories: ", category),
                "---\n\n")
  })
}


html_to_md <- function(htmls, filenames){
  lapply(1:length(htmls), function(html){
    writeLines(htmls[[i]], paste(filenames[i], ".html", sep=""))
    md <- system(paste("pandoc ", filenames[i], ".html -w markdown", 
                       sep=""), intern=TRUE)
  })
}


clean_md <- function(mds){
  lapply(mds, function(md){
    ## Remove OWW header
    md <- gsub("!\\[image\\]\\(http://openwetware.org/images/f/f8/Owwnotebook_icon.png\\)", "", md)
    md <- gsub("!\\[image\\]\\(http://openwetware.org/images/9/94/Report.png\\)",  "", md)
    md <- gsub("Main project", "", md)
    md <- gsub("page\\\\", "", md)
    md <- gsub("!\\[image\\]\\(http://openwetware.org/images/c/c3/Resultset_previous.png\\)",  "", md)
    md <- gsub("Previous$",  "", md)
    md <- gsub("^entry Next$", "", md)
    md <- gsub("!\\[image\\]\\(http://openwetware.org/images/5/5c/Resultset_next.png\\)", "", md)
    md <- gsub("^entry$", "", md)
    md <- gsub("^Stochastic Population Dynamics", "", md)
    md <- gsub("^Comparative Phylogenetics$", "", md)
    md <- gsub("^Teaching$", "", md)
  })
}



write_md <- function(mds, headers, filenames){
  lapply(1:length(mds), function(i){
    writeLines(c(headers[[i]], mds[[i]]), paste(filenames[i], ".markdown", sep="")) 
  })
}
