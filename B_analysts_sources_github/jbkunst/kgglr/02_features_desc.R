# ws ----------------------------------------------------------------------
rm(list = ls())
source("code/99_helpers.R")
library(tidyverse)
library(stringr)
library(tidytext)
library(scales)
# devtools::install_github("hrbrmstr/pluralize")
library(pluralize)

# data --------------------------------------------------------------------
data <- readRDS("input/data_load.rds")
glimpse(data)

# description -------------------------------------------------------------
data_desc <- select(data, id, desc_ori = description)
rm(data)

glimpse(data_desc)

data_desc <- data_desc %>% 
  mutate(
    desc = desc_ori,
    desc = str_to_lower(desc),
    desc = stringi::stri_trans_general(desc, "Latin-ASCII"),
    desc = str_trim(desc),
    desc = str_replace_all(desc, "<.*?>", " "),
    desc = str_replace_all(desc, "[:punct:]", " "),
    desc = str_replace_all(desc, "\\!|\\?|:|-|_|\\*|/", " "),
    desc = str_replace_all(desc, "<a website redacted", " "),
    desc = str_replace_all(desc, "\\s+", " ")
  )

sample(data_desc$desc, size = 10)
glimpse(data_desc)

str_replace("<a website redacted", "<a website redacted", "")
"no" %in% stop_words$word


# descriptive description -------------------------------------------------
# data_desc <- data_desc %>% 
#   mutate(
#     desc_n_words = desc %>% str_split(" ") %>% map(unlist) %>% map_int(length),
#     desc_n_excl  = str_get_counts(desc, "!"),
#     desc_n_punct = str_get_counts(desc, "[:punct:]"),
#     desc_n_email = str_get_counts(desc, "<[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}")
#   )

# glimpse(data_desc)

# singularize -------------------------------------------------------------
data_words <- data_desc %>% 
  select(id, desc) %>% 
  unnest_tokens(word, desc)

glimpse(data_words)

data_words_summ <- data_words %>%
  count(word, sort = TRUE) %>%
  mutate(id_word = 1:nrow(.))
data_words_summ

ggplot(data_words_summ) +
  geom_line(aes(id_word, n)) + 
  scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma) 

lowcountwordsl <- filter(data_words_summ, n <= 3)$word
data_words_summ <- filter(data_words_summ, ! word %in% lowcountwordsl)
data_words_summ %>% filter(n < 5) %>% sample_n(10)

filter(data_words_summ, !is.na(str_extract(word, "^\\d+$")))
data_words_summ <- filter(data_words_summ, is.na(str_extract(word, "^\\d+$")))
  
word_sing <- map_chr(data_words_summ$word, function(w){
  print(w)
  try({ return(singularize(w)) })
  NA
  })

data_words_summ <- mutate(data_words_summ, word_s = word_sing)

filter(data_words_summ, is.na(word_s))

count(data_words_summ, word)
count(data_words_summ, word_s)

# data_words <- select(data_words, -word_s)
data_words <- data_words %>% 
  left_join(select(data_words_summ, word, word_s), by = "word") %>% 
  rename(word = word_s, word_ori = word) %>% 
  mutate(word = ifelse(is.na(word), word_ori, word)) %>% 
  select(id, word)
  
glimpse(data_words)  

# Term frequency and inverse document frequency ---------------------------
data_words_summ <- data_words %>% 
  # sample_frac(.2) %>%
  # filter(!word %in% lowcountwordsl) %>% 
  anti_join(stop_words) %>% 
  count(id, word) %>% 
  ungroup() %>% 
  # find the words most distinctive to each description
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf)) %>% 
  # calculating tf_idf rank by id/desc
  group_by(id) %>% 
  arrange(id, desc(tf_idf)) %>% 
  ungroup() %>% 
  group_by(id) %>% 
  mutate(
    tf_idf_rank = row_number(-tf_idf)
    ) %>% 
  ungroup() %>% 
  group_by(word) %>% 
  summarise(
    total_app_desc = n(),
    total_app_desc_sum = sum(n),
    tf_idf_median = median(tf_idf),
    tf_idf_rank_median = median(tf_idf_rank)
    )

P <- 0.5
N <- 50
data_words_summ_final <- data_words_summ %>% 
  mutate(app_prop = total_app_desc/nrow(data_desc)) %>% 
  filter( P/100 <= app_prop, app_prop <= (100 - P)/100) %>% 
  mutate(factor = get_rank(total_app_desc) * get_rank(tf_idf_median)) %>% 
  arrange(desc(factor)) %>% 
  head(N)

data_words_summ_final

  
# sent affin --------------------------------------------------------------
data_sent_affin <- inner_join(data_words, get_sentiments(lexicon = "afinn"))
data_sent_affin

data_sent_affin_summ <- data_sent_affin %>% 
  group_by(id) %>% 
  do(tidy(summary(.$score))) %>% 
  ungroup()

names(data_sent_affin_summ) <- paste0("desc_sent_affin_sum_",
                                      names(data_sent_affin_summ))

data_sent_affin_summ <- rename(data_sent_affin_summ, 
                               id = desc_sent_affin_sum_id)

data_desc <- left_join(data_desc, data_sent_affin_summ, by = "id")

rm(data_sent_affin, data_sent_affin_summ)

# sent bing ---------------------------------------------------------------
data_sent_bing <- inner_join(data_words, get_sentiments(lexicon = "bing"))
data_sent_bing

data_sent_bing_summ <- data_sent_bing %>% 
  count(id, sentiment) %>% 
  ungroup() %>% 
  spread(sentiment, n)

names(data_sent_bing_summ) <- paste0("desc_sent_bing_sum_",
                                      names(data_sent_bing_summ))

data_sent_bing_summ <- rename(data_sent_bing_summ, 
                              id = desc_sent_bing_sum_id)

data_desc <- left_join(data_desc, data_sent_bing_summ, by = "id")

rm(data_sent_bing, data_sent_bing_summ)


# sent nrc ----------------------------------------------------------------
data_sent_nrc <- inner_join(data_words, get_sentiments(lexicon = "nrc"))

data_sent_nrc_summ <- data_sent_nrc %>% 
  count(id, sentiment) %>% 
  ungroup() %>% 
  spread(sentiment, n)

names(data_sent_nrc_summ) <- paste0("desc_sent_nrc_sum_",
                                     names(data_sent_nrc_summ))

data_sent_nrc_summ <- rename(data_sent_nrc_summ, 
                              id = desc_sent_nrc_sum_id)

data_desc <- left_join(data_desc, data_sent_nrc_summ, by = "id")

rm(data_sent_nrc, data_sent_nrc_summ)

# ngrams  -----------------------------------------------------------------
data_ngram <- data_words %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(id) %>% 
  summarize(desc = paste(word, collapse = " ")) %>%
  unnest_tokens(ngram, desc, token = "ngrams", n = 3)

data_ngram_summ <- count(data_ngram, ngram, sort = TRUE)
data_ngram_summ

# ngrams 3 ----------------------------------------------------------------

# export ------------------------------------------------------------------
data_desc <- dmap_if(data_desc, is.numeric, function(x) ifelse(is.na(x), 0, x))
glimpse(data_desc)
saveRDS(data_desc, "input/data_feat_description.rds")


