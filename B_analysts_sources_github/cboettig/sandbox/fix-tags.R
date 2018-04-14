files <- system("ls *.md", intern=TRUE)
require(gsubfn)
lapply(files, function(file){
  content <- readLines(file)
  yaml <- grep("^---$", content)
  content[yaml[1]:yaml[2]] <- gsubfn("science-communication", "", content[yaml[1]:yaml[2]])
  content[yaml[1]:yaml[2]] <- gsubfn("plugins", "site-configuration", content[yaml[1]:yaml[2]])
  content[yaml[1]:yaml[2]] <- gsubfn("NIMBioS", "pdg-control", content[yaml[1]:yaml[2]])
  content[yaml[1]:yaml[2]] <- gsubfn("model-choice", "pmc", content[yaml[1]:yaml[2]])
  writeLines(content, file)
})



