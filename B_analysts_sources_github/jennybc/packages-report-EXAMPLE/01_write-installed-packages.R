library(tidyverse)
library(here)

ipt <- installed.packages() %>%
  as_tibble() %>%
  select(Package, LibPath, Version, Priority, Built) %>%
  write_csv(path = here("data", "installed-packages.csv"))

## IRL I would might incorporate a date, but clunky for today's purpose
## e.g., here("data", paste0(Sys.Date(),"_installed-packages.csv"))

## View(ipt)
