#' ---
#' title: "Hello"
#' author: "Jenny Bryan"
#' date: "June 15, 2016"
#' output: github_document
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)

#' ## R Markdown

#' You can put formulas. $\overline{\mu_i}$.

#' This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

#' When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

#+ cars
summary(cars)

#'Let's add a new chunk.

mean(cars$speed)

#' ## Including Plots

#' You can also embed plots, for example:

#+ pressure, echo=FALSE
plot(pressure)

#' Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
