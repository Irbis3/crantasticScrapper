# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(stringi)


# helpers -----------------------------------------------------------------
clean_data <- function(d){
  d %>% 
    dmap_if(is.character, tolower) %>% 
    dmap_if(is.character, str_trim) %>% 
    dmap_if(is.character, stri_trans_general, id = "Latin-ASCII")
  
}


# data --------------------------------------------------------------------
scores <- read_excel("data/alumnos_puntajes.xlsx") %>% 
  clean_names()

prefrc <- read_excel("data/alumnos_preferencias.xlsx") %>% 
  clean_names()

carrrs <- read_excel("data/carrera_cupos.xlsx") %>% 
  clean_names() %>% 
  clean_data()

scoresl <- scores %>% 
  gather(alumno, puntaje, -carrera) %>% 
  mutate(alumno = str_extract(alumno, "\\d+")) %>% 
  clean_data()

prefrcl <- prefrc %>% 
  gather(carrera, preferencia, -alumno) %>% 
  mutate(alumno = str_extract(alumno, "\\d+")) %>% 
  clean_data()

# export ------------------------------------------------------------------
saveRDS(scoresl, "data/scores.rds")
saveRDS(prefrcl, "data/prefrc.rds")
saveRDS(carrrs, "data/carrrs.rds")



