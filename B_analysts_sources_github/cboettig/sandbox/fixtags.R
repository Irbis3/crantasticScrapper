## need to support other yaml lists too
fix_tags <- function(files){
  require(gsubfn)
  lapply(files, function(file){

       fix <- function(tags){
         tags <- strsplit(tags, ",")
         tags <- gsub("\\[", "", tags)
         tags <- gsub("\\]", "", tags) #drop for the moment if present, add back later
         tags <- gsub("-", " ", tags)
         tags <- gsub("-", " ", tags)
         tags <- gsub("\\b(\\w)", "\\L\\1", tags, perl=TRUE) #lowercase
         paste("tags: \\[", paste0(tags, collapse =","), "\\]", sep="")
       }

       content <- readLines(file)
       content <- gsubfn("tags: (.*)", fix, content)

         writeLines(content, file)
  })
}


files <- system("ls *.markdown *.md", intern=TRUE)
easytags <- function(files){
  require(gsubfn)
  lapply(files, function(file){
    content <- readLines(file)
    yaml <- grep("^---$", content)
    content[yaml[1]:yaml[2]] <- gsubfn("Seminar", "seminar", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Teaching", "teaching", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Stochastic Population Dynamics", "ecology", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Phylogenetics", "evolution", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Computation", "computation", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Logistics", "logistics", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("ecology/evolution,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("progress report,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("site configuration", "site-configuration", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("conferences", "conference", content[yaml[1]:yaml[2]])
    writeLines(content, file)
  })
}


easytags <- function(files){
  require(gsubfn)
  lapply(files, function(file){
    content <- readLines(file)
    yaml <- grep("^---$", content)
    content[yaml[1]:yaml[2]] <- gsubfn("categories: Stochastic Population Dynamics", "categories: [Stochastic Population Dynamics]", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("OpenScience", "open-science", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("open notebook thoughts", "open-science", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Beetles", "tribolium", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Regimes_model,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Paper_Outlines,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("prosecutor-fallacy,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("codepost,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("seminar,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("abstract,*", "", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Model_Choice", "model-choice", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("api", "hpc", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("computing", "hpc", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("ABC", "algorithms", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Adaptive Dynamics", "adaptive-dynamics", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Adaptive_Dynamics", "adaptive-dynamics", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Warning_signals", "warning-signals", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("early-warning", "warning-signals", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Code_tricks", "code-tricks", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("code tricks", "code-tricks", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Web2.0_Tools", "code-tricks", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Science2.0", "open-science", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Open Notebook Thoughts", "open-science", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Carl_meetings", "conferences", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Carl_talks", "conferences", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("Conferences", "conferences", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("conference[,$]+", "conferences", content[yaml[1]:yaml[2]])
    writeLines(content, file)
  })
}


easycategories <- function(files){
  require(gsubfn)
  lapply(files, function(file){
    content <- readLines(file)
    yaml <- grep("^---$", content)
    content[yaml[1]:yaml[2]] <- gsubfn("categories:  Stochastic_Population_Dynamics", "categories: Stochastic Population Dynamics", content[yaml[1]:yaml[2]])
    content[yaml[1]:yaml[2]] <- gsubfn("categories:  Comparative_Phylogenetics", "categories: Phylogenetics", content[yaml[1]:yaml[2]])
    writeLines(content, file)
  })
}



