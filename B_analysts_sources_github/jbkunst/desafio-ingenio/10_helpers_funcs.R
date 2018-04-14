# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stringr)

# data --------------------------------------------------------------------
scores <- readRDS("data/scores.rds")
prefrc <- readRDS("data/prefrc.rds")
carrrs <- readRDS("data/carrrs.rds")

# helpers functions -------------------------------------------------------
assigment_rndm <- function() {
  data_frame(
    alumno = as.character(1:100),
    assigment = sample(rep(carrrs$carrera, carrrs$cupos), size = 100)
  )
}

assigments <- assigment_rndm()
  
assigment_temp <- function(assigments) {
  
  left_join(assigments, prefrc, by = "alumno") %>% 
    filter(assigment == carrera) %>% 
    mutate(weight = preferencia - 1) %>% 
    arrange(desc(weight)) %>% 
    count(weight) %>% 
    summarise(sum(weight*n)) %>% 
    .[[1]]

}

assigment_cnts <- function(assigments) {
  
  assigments %>% 
    count(assigment) %>% 
    left_join(carrrs, by = c("assigment" = "carrera")) %>% 
    mutate(check = n <= cupos)
}

assigment_vald <- function(assigments){
  
  counts <- assigment_cnts(assigments)
  
  bigcheck <- all(counts$check)
  
  if(!bigcheck){
    
    counts %>% 
      filter(!check) %>%
      select(-check) %>% 
      dmap_if(is.character, str_pad, width = 10) %>% 
      dmap_if(is.numeric, str_pad, width = 2) %>% 
      by_row(paste, collapse = " ", .collate = "rows") %>% 
      .$.out %>% 
      paste0(collapse = "\n") %>% 
      message()
      
  }
  
  bigcheck
}

assigment_rndm()
assigment_temp(assigment_rndm())
assigment_cnts(assigment_rndm())
assigment_vald(assigment_rndm())

# export ------------------------------------------------------------------
save(scores, prefrc, carrrs,
     assigment_temp, assigment_vald, assigment_rndm, assigment_cnts,
     file = "data/initial_data.RData")


