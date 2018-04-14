library(fs)
library(purrr)
library(here)

dirs <- c(here("data"), here("figs"))
dirs <- keep(dirs, dir_exists)
files <- map(dirs, dir_ls) %>% flatten_chr()
file_delete(files)
