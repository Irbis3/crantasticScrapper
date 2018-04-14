library(tidyverse)
library(here)
library(fs)

data_file <- dir_ls(here("data"), glob = "*installed-packages.csv")
stopifnot(length(data_file) == 1)

ipt <- read_csv(data_file)

## filter out packages in the default library
## keep Package and Built
apt <- ipt %>%
  filter(LibPath == .libPaths()[1]) %>%
  select(Package, Built)

## View(apt)

write_csv(apt, path = here("data", "add-on-packages.csv"))

apt_freqtable <- apt %>%
  count(Built) %>%
  mutate(prop = n / sum(n))

## apt_freqtable

write_csv(apt_freqtable, path = here("data", "add-on-packages-freqtable.csv"))
