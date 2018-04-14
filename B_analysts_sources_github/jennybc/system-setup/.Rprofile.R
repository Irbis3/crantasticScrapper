if (interactive()) {

  DEVTOOLS.DESC.AUTHOR <-
    paste0("person(\"Jennifer\", \"Bryan\", ",
           "role=c(\"aut\", \"cre\"),\n  email = \"jenny@rstudio.com\")")

  options(
    ## use https repos
    repos = c(CRAN = "https://cran.rstudio.org"),
    devtools.name = "Jennifer Bryan",
    devtools.desc = list(
      `Authors@R`     = DEVTOOLS.DESC.AUTHOR,
      License         = "MIT + file LICENSE",
      Version         = "0.0.0.9000"
    ),
    devtools.revdep.libpath = "~/resources/R/revdep_library",

    ## fancy quotes are annoying and lead to
    ## 'copy + paste' bugs / frustrations
    useFancyQuotes = FALSE,

    max.print = 100,

    ## warnings are errors
    warn = 2
  )
  #      VignetteBuilder = "knitr")

  rm(DEVTOOLS.DESC.AUTHOR)

  ## auto-completion of package names in `require`, `library`
  utils::rc.settings(ipck = TRUE)

  library(devtools)
  library(testthat)
  library(reprex)

  .__Rprofile.env__. <- new.env()

  ## write epically long temp paths to clipboard
  assign("wdclip", function() clipr::write_clip(getwd()),
         envir = .__Rprofile.env__.)

#assign("%>%", magrittr::`%>%`, envir = .__Rprofile.env__.)
assign("%||%", purrr::`%||%`, envir = .__Rprofile.env__.)

  attach(.__Rprofile.env__.)

}

# warn on partial matches
options(warnPartialMatchAttr = TRUE,
        warnPartialMatchDollar = TRUE,
        warnPartialMatchArgs = TRUE)
