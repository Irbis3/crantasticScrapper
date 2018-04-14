source("scripts/entropy.R")
library(tidyverse)

my_matrix <- test_matrix(8)
mm <- matrix(c(1,0,0,0,1,0,0,1,0,0,0,1,0,1,1,1,0,0,0,0,0,1,0,1,1), nrow = 5, byrow = TRUE)
life <- matrix(c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5, byrow = TRUE)
chess <- matrix(rep(c(rep(c(1, 0), 4), rep(c(0,1), 4)), 4), nrow = 8)
m_highest <- low_matrix(8)
m_lowest <- low_matrix(8, x = 0)

plot_ks(test_matrix(5))
plot_ks(life)

plot_ks(chess)
plot_ks(test_matrix(8))

plot_ks(m_lowest)
plot_ks(m_highest)

mm_big <- test_matrix(100)
m_big_lowest <- low_matrix(100)
plot_ks(mm_big)
plot_ks(m_big_lowest)


## order matrices

matrices <- map(1:10, ~ test_matrix(8))

matrices <- c(list(chess, low_matrix(8)), matrices)
matrices %>% 
  entropy_order %>% 
  map(plot_ks) %>% 
  reduce(bind_rows)


## Make a lot of matrices
matrices4 <- map(1:1000, ~ test_matrix(4)) %>% 
  dedupe_matrices %>%
  entropy_order

matrices4 %>% 
  as_entropy_df

matrices4 %>% 
  map(~ plot_ks(.,k = "2"))
