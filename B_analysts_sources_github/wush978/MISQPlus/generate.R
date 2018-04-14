args <- commandArgs(trailingOnly = TRUE)
rmd_src <- args
if(!file.exists(rmd_src)) {
  quit(save='no',status=1)
}

library(knitr)
library(markdown)
md_dst <- sub('.Rmd','.md',rmd_src)
html_dst <- sub('.Rmd','.html',rmd_src)
knit(rmd_src, md_dst)
markdownToHTML(md_dst, html_dst)