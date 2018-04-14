dir.create("raw", showWarnings = FALSE)

# ==============================================================================
# PACKAGES
# ==============================================================================

library(dplyr)
library(readr)
library(rvest)
library(stringr)

library(ggplot2) # 02
library(scales)  # 02
library(tidyr)   # 02

# ==============================================================================
# DATASETS SPECS
# ==============================================================================

f_list <- "ala_list.csv"
s_list <- cols(
  page = col_character(),
  title = col_character(),
  url = col_character(),
  date = col_datetime(format = ""),
  au_url = col_character(),
  au_id = col_character()
)

f_info <- "ala_info.csv"
s_info <- cols(
  url = col_character(),
  description = col_character(),
  tags = col_character()
)

f_tags <- "ala_tags.csv"
s_tags <- cols(
  parent = col_character(),
  tag = col_character()
)

f_data <- "ala_data.csv"
s_data <- cols(
  date = col_datetime(format = ""),
  url = col_character(),
  tags = col_character(),
  au_url = col_character(),
  au_id = col_character(),
  title = col_character(),
  description = col_character()
)

f_refs <- "ala_refs.csv"
s_refs <- cols(
  i = col_character(),
  j = col_character()
)
