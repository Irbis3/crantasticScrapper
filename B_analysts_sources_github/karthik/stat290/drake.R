library(drake)
library(tidyverse)
library(cranlogs)
library(knitr)


package_list <- c(
  "knitr",
  "Rcpp",
  "ggplot2"
)


data_plan <- drake_plan(
  recent = cran_downloads(packages = package_list, when = "last-month"),
  older = cran_downloads(
    packages = package_list,
    from = "2016-11-01",
    to = "2016-12-01"
  ),
  strings_in_dots = "literals"
)


# Character scalar, how to treat quoted character strings in the commands
# specified through .... Set to "filenames" to treat all these strings as
# external file targets/imports (single-quoted), or to "literals" to treat them
# all as literal strings (double-quoted). Unfortunately, because of how R
# deparses code, you cannot simply leave literal quotes alone in the ...
# argument. R will either convert all these quotes to single quotes or double
# quotes. Literal quotes in the list argument are left alone.

data_plan  


output_types <- drake_plan(
  averages = make_my_table(dataset__),
  plot = make_my_plot(dataset__)
)


make_my_table <- function(downloads){
  downloads %>% group_by(package) %>% summarise(m = mean(count))
}

make_my_plot <- function(downloads){
  ggplot(downloads) +
    geom_line(aes(x = date, y = count, group = package, color = package))
}

output_types



output_plan <- plan_analyses(
  plan = output_types,
  datasets = data_plan
)


output_plan

report_plan <- drake_plan(
  knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
)

whole_plan <- rbind(
  data_plan,
  output_plan,
  report_plan
)


config <- drake_config(whole_plan)
vis_drake_graph(config)

make(whole_plan)
readd(averages_recent)
readd(averages_older)
readd(plot_recent)
readd(plot_older)

