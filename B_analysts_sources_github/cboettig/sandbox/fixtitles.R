# Get a list of all files
files <- system("ls *OWW*.markdown", intern=T)


# Update syntax highlighted code block tags
lapply(files, function(file){
  content <- readLines(file)
  subtitles <- grep("^-----*$", content)          # Find the title lines
  if(length(subtitles)>0){
    title <- content[subtitles[1]-1]                # Extract the title
    content[(subtitles[1]-1):subtitles[1]] <- ""    # delete the title
    title <- gsub(":", "-", title)                  # colons not permitted
    title <- gsub("/", "-", title)                  # colons not permitted
    yaml <- grep("^---$", content)                  # Find YAML matter ending
    content[yaml[2]] <- paste("title: ", title, "\n---\n", sep="")

    # filename spaces as dashes
    title <-gsub(" ", "-", title) 
    filename <- gsub("(20\\d\\d-\\d\\d-\\d\\d)-(.*).markdown",
                     paste("\\1-", title, ".markdown", sep=""),
                     file)   
    writeLines(content, filename)
  }
})


