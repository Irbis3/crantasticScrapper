# ws ----------------------------------------------------------------------
rm(list = ls())
source("code/99_helpers.R")
library(tidyverse)
library(stringr)
# devtools::install_github("hansthompson/rusps")
library(rusps)
library(XML)

# data --------------------------------------------------------------------
data <- readRDS("input/data_load.rds")
glimpse(data)

USERNAME <- "389198504250"

# features ----------------------------------------------------------------
dstreet <- data %>% 
  select(id, street = street_address) %>% 
  mutate(street_address = street)
rm(data)

dstreet <- dstreet %>% 
  mutate(
    street = str_to_lower(street),
    street = str_replace_all(street, "[:punct:]", ""),
    street = str_replace_all(street, "\r", ""),
    street = str_replace_all(street, "\"", " "),
    street = str_replace_all(street, "streetstreet", " st st "),
    street = str_replace_all(street, "street", " st "),
    street = str_replace_all(street, "ave |ave$|ave\\.", "avenue"),
    street = str_replace_all(street, "eastreet", " e "),
    street = str_replace_all(street, "westreet", " w "),
    street = str_replace_all(street, "\\s+", " "),
    street = str_trim(street)
  )

str_split(dstreet$street, " ") %>% 
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(20) %>% 
  names()

dstreetres <- dstreet %>% 
  count(street, sort = TRUE) %>% 
  mutate(p = 100 * n / sum(n),
         acump = cumsum(p))
dstreetres

ss <- dstreetres$street

ss_list <- split(ss, ceiling(seq_along(ss)/500))

length(ss_list)

map(sample(seq_along(ss_list)), function(chunk){ # chunk <- 200
  
  file <- sprintf("data/street_usps_%s.rds", chunk)
  
  if(!file.exists(file)) {
    
    dfchunk <- map_df(ss_list[[chunk]],  function(x){ # x <- sample(dstreetres$street, size = 1)
      message(x)
      try({
        # x <- "396 east 25th st"
        # gm <- ggmap::geocode(paste(x, "NY USA"), output = "all")
        # cty <- gm$results[[1]]$address_components[[1]]$long_name
        # if(is.null(cty)) cty <- "New York"
        y <- validate_address_usps(x, "New York", "ny", USERNAME) %>% 
          select(Address2) %>% 
          .[1,1] %>% 
          unlist() %>% 
          as.vector()
        dfout <- data_frame(street = x, street_usps = y)
        print(dfout)
        return(dfout)
      })
      return(data_frame(street = x, street_usps = NA))
    })
    
    saveRDS(dfchunk, file)
    
  }
  
})

# validate_address_usps("266 washington ave Brooklyn", "New York", "ny", USERNAME)
# validate_address_usps("396 east 25th st", "Brooklyn", "ny", USERNAME)
# validate_address_usps("4310 crescent st", "", "ny", USERNAME)

dusps <- dir("data/", pattern = "usps", full.names = TRUE) %>%
  map_df(readRDS)

data_streets <- dstreetres %>% 
  select(street) %>% 
  left_join(dusps) %>% 
  left_join(dstreet, .)
  
left_join(dstreetres, dusps) %>%
  # group_by(is.na(street_usps)) %>% summarise(sum(p))
  # count(is.na(street_usps))
  # filter(is.na(street_usps)) %>% 
  count(street_usps)

# export ------------------------------------------------------------------
saveRDS(data_streets, "input/data_feat_street.rds")


