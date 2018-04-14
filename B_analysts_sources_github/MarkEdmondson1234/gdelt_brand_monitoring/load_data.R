# devtools::install_github("abresler/gdeltr2")
library(gdeltr2)

## run examples
test_terms <-
  c('"BeoPlay"', '"Fullrate"', '"Visit Dubai"', '"Ikea"')

term_data <- 
  get_data_ft_api_terms(terms = test_terms, max_rows = 10000, only_english = F, domain = NA)

test_domains <- 
  c('realdeal.com', 'nytimes.com', 'curbed.com', 'pehub.com', 'wsj.com')

domain_data <- 
  get_data_ft_api_domains(term = NA, domains = test_domains)

domain_wordcloud <- 
  get_data_wordcloud_ft_api_domains(domains = test_domains, term = NA, tone_more_than = 1)

term_wordcloud <- 
  get_data_wordcloud_ft_api_terms(terms = test_terms, domain = NA)

term_sentiment <-
  get_data_sentiment_ft_api_terms(terms = test_terms, domain = NA )

domain_sentiment <-
  get_data_sentiment_ft_api_domains(domains = test_domains)

events <- get_data_gdelt_periods_event(periods = c(1983, 1989),
                                       file_directory = "temp_gdelt_data", remove_files = T,
                                       empty_trash = T, return_message = T)