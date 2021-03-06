library(shiny)
library(shinyAce)

#' Define UI for application that demonstrates a simple Ace editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  bootstrapPage(
    headerPanel("Shiny Ace knitr Example"),
    div(
      class="container-fluid",
      div(class="row-fluid",
          div(class="col-sm-6",
              h2("Source R-Markdown"),  
              aceEditor("rmd", mode="markdown", value='
### Sample knitr Doc
This is some markdown text. It may also have embedded R code
which will be executed.
```{r}
2*3
rnorm(5)
```
It can even include graphical elements.
```{r}
hist(rnorm(100))
```
'),
              actionButton("eval", "Update")
          ),
          div(class="col-sm-6",
              h2("Knitted Output"),
              htmlOutput("knitDoc")
          )
      )
    )))