#' ---
#' title: "Report from R/Rmd"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#' The iris data is boring, but it won't distract from the Git content.

aggregate(. ~ Species, data = iris, median)
