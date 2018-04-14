
## Dumb idea to write one's own parser
wiki_to_md <- function(file){
  pattern <- "\\b(https?|ftp|file)://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"

  txt <- readLines(file)
  txt <- gsubfn("^={1}\\s(.+)\\s={1} *$", "# \\1", txt)
  txt <- gsubfn("^={2}\\s(.+)\\s={2} *$", "## \\1", txt)
  txt <- gsubfn("^={3}\\s(.+)\\s={3} *$", "### \\1", txt)
  txt <- gsubfn("^={4}\\s(.+)\\s={4} *$", "### \\1", txt)
  txt <- gsubfn("^={5}\\s(.+)\\s={5} *$", "### \\1", txt)
  txt <- gsubfn("^={6}\\s(.+)\\s={6} *$", "### \\1", txt)

  txt <- gsubfn(paste("\\[(", pattern, ") (.+)\\]", sep=""), "[\\2](\\1)", txt)

 categories <- strapply(txt, "\\[\\[Category:(\\w+_*\\w*)\\]\\]")

 categories <- strapply(txt, "\\[\\[Category:(\\w+_*\\w*)( *\\|* (\\w+_*\\w*) *)*\\]\\]")


}
