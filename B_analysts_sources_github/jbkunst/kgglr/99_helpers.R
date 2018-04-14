is_out <- function(x = rt(20, 1)) {
  x %in% boxplot.stats(x)$out
}

get_rank <- function(x = sample(1:10)) {
  ecdf(x)(x)
}

median <- function(x) {
  quantile(x, 0.5, na.rm = TRUE)
}

str_std <- function(string = c("prewar", "elevator", "storage", "Laundry in Building", "dishwasher", 
                          "Dogs Allowed", "Cats Allowed", "LOWRISE", "EXPOSED BRICK", "LAUNDRY", 
                          "SIMPLEX", "HARDWOOD", "No Fee", "other/other", "now?!")) {
  
  string %>% 
    str_to_lower() %>% 
    str_replace_all("[:punct:]", "") %>% 
    str_replace_all(" |/|-", "_")
  
}

str_get_counts <- function(string, pattern) {
  message(pattern)
  string %>%
    map(str_extract_all, pattern) %>%
    map(unlist) %>%
    map_int(length)
}
