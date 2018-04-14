library(fs)
library(here)

if (!dir_exists(here("data"))) {
  dir_create(here("data"))
}

if (!dir_exists(here("figs"))) {
  dir_create(here("figs"))
}

source(here("R/01_write-installed-packages.R"))
source(here("R/02_wrangle-packages.R"))
source(here("R/03_barchart-packages-built.R"))

rmarkdown::render(here("README.Rmd"))
## I'm not really sure why this is being created, tbh
file_delete("README.html")
