wwwDirectory = function(){
  path <- system.file("www",package="D3GB")
  return(path)
}

createHTML <- function(dir, dependencies, data, chromosomes){
  if(file.exists(dir))
    unlink(dir, recursive = TRUE)
  dir.create(dir)
  www <- wwwDirectory()
  html <- scan(file = paste(www, "template.html", sep = "/"), what = character(0), sep = "\n", quiet = TRUE)
  name <- strsplit(dir,"/")[[1]]
  name <- name[length(name)]
  html <- sub("<!--title-->", name, html)
  dep <- "<!--head-->"
  for(i in seq_along(dependencies)){
    if(grepl(".css$",dependencies[i])){
      dep <- paste(dep, paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/",dependencies[i],"\"></link>"), sep = "\n")
      dirName <- "styles"
    }else{
      dep <- paste(dep, paste0("<script type=\"text/javascript\" src=\"scripts/",dependencies[i],"\"></script>"), sep = "\n")
      dirName <- "scripts"
    }
    dir.create(paste(dir, dirName, sep = "/"),FALSE)
    file.copy(paste(www, dependencies[i], sep = "/"), paste(dir, dirName, sep = "/"))
  }
  html <- sub("<!--head-->", dep, html)
  chromosomes <- paste0('<script type="application/json" id="chromosomes">',chromosomes,'</script>')
  html <- sub("<!--body-->",paste("<!--body-->", chromosomes, sep = "\n"),html)
  data <- paste0('<script type="application/json" id="data">',data,'</script>')
  html <- sub("<!--body-->",paste("<!--body-->", data, sep = "\n"),html)
  write(html, paste(dir, "index.html", sep = "/"))
  message(paste0("The graph has been generated in the ",dir," folder."))
}
