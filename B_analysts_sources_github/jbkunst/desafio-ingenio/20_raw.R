# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stringr)

# data --------------------------------------------------------------------
load("data/initial_data.RData")

# heuristic ---------------------------------------------------------------
assigments <- setNames(rlist::list.parse(carrrs), NULL)
names(assigments) <- carrrs$carrera

for(c in carrrs$carrera){ # c <- sample(carrrs$carrera, size = 1)
  
  message(c)
  
  students <- prefrc %>%
    filter(carrera == c, preferencia == 1) %>% 
    left_join(scores, by = c("alumno", "carrera")) %>% 
    arrange(desc(puntaje))

  assigments[[c]]$alumnos <- students$alumno
  
}

assigment_raw <- assigments %>% 
  map_df(function(x){ # x <- assigments[[1]]
    if(length(x$alumnos) > 0) {
      return(data_frame(alumno = x$alumnos, assigment = x$carrera))
    }
  })

assigment_temp(assigment_raw)
assigment_cnts(assigment_raw)
assigment_vald(assigment_raw)




