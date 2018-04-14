# ws ----------------------------------------------------------------------
rm(list = ls())
source("code/99_helpers.R")
library(tidyverse)
library(stringr)

# data --------------------------------------------------------------------
data <- readRDS("input/data_load.rds")
glimpse(data)

PERC_FEATS <- 1/100
  
# features ----------------------------------------------------------------
data$features <- map(data$features, str_std)

feats <- unlist(data$features)
feats <- count(data_frame(feats = feats), feats, sort = TRUE)
ids <- seq(from = 1, to = nrow(feats))
feats <- mutate(feats, id = ids, p = n/nrow(data))

ggplot(feats) + 
  geom_line(aes(id, p)) + 
  geom_hline(aes(yintercept = PERC_FEATS)) + 
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent) 

feats_to_consider <- filter(feats, p >= PERC_FEATS)$feats

data_feats <- map_df(data$features, function(x){
  message(x)
  if(length(x) == 0) {
    return(data_frame(none = 1))
  } 
  ifelse(x %in% feats_to_consider, x, "other") %>%  
    { data_frame(feat = .) }%>% 
    count(feat) %>% 
    spread(feat, n)
})

names(data_feats) <- paste0("ft_", names(data_feats))
try({data_feats <- select(data_feats, -ft_total)})

data_feats <- dmap(data_feats, function(x) ifelse(is.na(x), 0, x)) 

data_feats <- data_feats %>% 
  mutate(
    ft_total = rowSums(data_feats),
    id = seq(1, nrow(data_feats))
    ) %>% 
  select(id, ft_total, ft_other, everything())

glimpse(data_feats)

# export ------------------------------------------------------------------
saveRDS(data_feats, "input/data_feat_misc.rds")


