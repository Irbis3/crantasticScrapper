# ws ----------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(jsonlite)

# data --------------------------------------------------------------------
load_data <- function(file = "input/train.json") {
  data <- read_lines(file)
  data <- fromJSON(data)
  # str(data, max.level = 1)
  vars <- setdiff(names(data), c("photos", "features"))
  data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
  data
}

train <- load_data("input/train.json")
test <- load_data("input/test.json.zip")

data <- bind_rows(
  mutate(train, sample = "train"),
  mutate(test, sample = "test")
) %>% 
  mutate(id = seq_len(nrow(.))) %>% 
  select(id, sample, interest_level, everything())

glimpse(data)
count(data, sample)

# export ------------------------------------------------------------------
saveRDS(data, "input/data_load.rds")

