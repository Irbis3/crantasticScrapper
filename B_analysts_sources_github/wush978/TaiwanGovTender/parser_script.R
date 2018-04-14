source("bparser.R")
basicConfig()
max_column_names <- roaming_dir(start_dir= "./tenders", use_minimum_parse = TRUE, dont_save_each_csv = TRUE)
file_add_header(start_dir= "./", max_column_names)