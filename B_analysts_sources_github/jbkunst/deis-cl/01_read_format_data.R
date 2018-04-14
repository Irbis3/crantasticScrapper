# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(rio)
library(lubridate)
library(stringr)

message("Info in http://www.deis.cl/bases-de-datos-egresos-hospitalarios/")

# helpers -----------------------------------------------------------------
import_egreso <- function(f) { # f <- "data/egresos/egr_2012.csv"
  message("Importing: ", f)
  
  x <- tbl_df(rio::import(f))
  names(x) <- tolower(names(x))
  
  # remove factors
  x <- mutate_if(x, is.factor, as.character)

  # glimpse(x)

  if(has_name(x, "intervq"))    x <- rename(x, interv_q = intervq)
  if(has_name(x, "int_q"))      x <- rename(x, interv_q = int_q)
  if(has_name(x, "iq"))         x <- rename(x, interv_q = iq)

  if(has_name(x, "fecha_egre")) x <- rename(x, fecha_egr = fecha_egre)
  if(has_name(x, "fech_egre"))  x <- rename(x, fecha_egr = fech_egre)
  if(has_name(x, "fecha_eg"))   x <- rename(x, fecha_egr = fecha_eg)
  if(has_name(x, "fec_egr"))    x <- rename(x, fecha_egr = fec_egr)
  if(has_name(x, "egreso"))     x <- rename(x, fecha_egr = egreso)

  if(has_name(x, "ser_res"))    x <- rename(x, serv_res = ser_res)
  if(has_name(x, "ss_res"))     x <- rename(x, serv_res = ss_res)
  
  if(has_name(x, "serviciosalud")) x <- rename(x, ser_salud = serviciosalud)
  
  if(has_name(x, "serc_egr"))   x <- rename(x, sercle = serc_egr)

  if(has_name(x, "est"))        x <- rename(x, estab = est)

  if(has_name(x, "ds_estad"))   x <- rename(x, dias_estad = ds_estad)
  if(has_name(x, "dias_est"))   x <- rename(x, dias_estad = dias_est)

  if(has_name(x, "reg_res"))    x <- rename(x, region = reg_res)

  
  x <- mutate_at(x, vars(sercle, estab), as.character)
  
  x <- mutate_at(x,
                 vars(serv_res, sexo, edad, interv_q, benef, previ,
                      comuna, region, dias_estad, cond_egr, ser_salud),
                 as.numeric)

  if(has_name(x, "mod"))
    x <- mutate(x, mod = as.numeric(mod))

  if(!is.Date(pull(x, "fecha_egr"))) {

    pos <- pull(x, "fecha_egr") %>%
      sample(100) %>%
      str_locate("\\d{4}") %>%
      as_data_frame() %>%
      count(start, sort = TRUE) %>%
      head(1) %>%
      first()

    fun <- ifelse(pos == 1, ymd, dmy)

    x <- mutate(x, fecha_egr = fun(fecha_egr))

  }
  
  glimpse(x)
  x
  
}

fs <- dir(path = "data/egresos/", pattern = "csv$|dbf$", full.names = TRUE)
fs <- fs[order(str_extract(fs, "\\d{4}"))]
fs <- tail(fs, 10)

data <- map(fs, import_egreso) %>% 
  reduce(bind_rows)

glimpse(data)

# SER_SALUD = Servicio de Salud: Campo numérico, corresponde al servicio de Salud de ocurrencia 
# ESTAB = Establecimiento: Campo alfanumérico, código de establecimiento.
# Sexo: campo numérico, los valores aceptados son 1 = Hombre y 2 = Mujer
# Edad: Campo numérico.
# PREVI = Previsión: Campo numérico,  códigos válidos: 1 = Ley 18.469, 2 = Ley 16.744 (Accidente de trabajo),  3= Ley 16.744 (Accidente escolar), 4= Ley 18.490 (Accidente de transporte), 5 = ISAPRE,  6 = particular, 7 = Otro,  9 = Ignorado.
# BENEF = Tipo de Beneficiario: Campo numérico, si Previsión es 1 este no debe ser 0 ni mayor que 6. Códigos Validados: 1= Beneficiario A, 2= Beneficiario B, 3 = Beneficiario C, 4 = Beneficiario D, 5 = Libre Elección,  6 = Auge.
# Comuna: Comuna de Residencia. Campo numérico, este campo es validado con la tabla de comunas.
# FECHA_EGR = Fecha de Egreso: Campo de Fecha.
# SERC_EGR = Servicio Clínico de Egreso: Campo alfanumérico, validado con tabla de Servicios Clínicos
# DIAS_ESTAD = Días de Estada: Campo numérico.
# DIAG1 = Diagnóstico Principal: Campo alfanumérico, validado con tabla de CIE-10
# DIAG2 = Causa externa: Campo alfanumérico, validado con tabla de CIE-10                      
# COND_EGR = Condición al Egreso: Campo numérico, códigos válidos 1 = Mejorado y 2  = Muerto.
# INTERV_Q = Intervención Quirúrgica: Campo numérico, valores validados 1 =  Si, 2= No, 9= Ignorado.
# Región: Campo numérico, creado a partir del campo Comuna.
# SERV_RES = Servicio de Salud de Residencia: Campo numérico, creado a partir del campo de Comuna.
# 
# datas %>% 
#   map(names) %>% 
#   reduce(intersect)
# 
# datas %>% 
#   map(names) %>%
#   unlist() %>% 
#   as.vector() %>% 
#   table() %>% 
#   sort()
#   reduce(intersect)
# 
# map(datas, ~ try(count(.x, ser_salud)))
#   
# 
# data <- reduce(datas, bind_rows)
# 
# d <- datas %>% 
#   map(head, 2) %>% 
#   map(select, ser_salud) %>% 
#   reduce(bind_rows)
# 
# datas %>% 
#   map(~ map(.x, class)) %>% 
#   map(~ set_names(names(.x), unlist(.x))) %>% 
#   map(sort) %>% 
#   map(setdiff, c("mod")) %>% 
#   map_dbl(length)
# 
# reduce(datas, bind_rows)
# reduce(datas)
# 
# map_lgl(datas, has_name, "ser_salud")
# map(datas, ncol)
# 
# 
# n <- 5
# bind_rows(
#   datas[[1]] %>%
#     head(n) %>% 
#     rename(interv_q = intervq, fecha_egr = fecha_egre) %>% 
#     mutate(serv_res = as.numeric(serv_res), estab = as.character(estab)),
#   datas[[2]] %>%
#     head(n) %>%
#     rename(fecha_egr = egreso, serv_res = ser_res, interv_q = intervq) %>%
#     mutate(fecha_egr = dmy(fecha_egr), estab = as.character(estab)),
#   datas[[3]] %>%
#     head(n) %>% 
#     rename(serv_res = ser_res, fecha_egr = fech_egre) %>% 
#     mutate(interv_q = as.numeric(interv_q), estab = as.character(estab)),
#   datas[[4]] %>%
#     head(n) %>% 
#     rename(sercle = serc_egr, fecha_egr = fecha_eg) %>%  # sercle = serc_egr?
#     mutate(fecha_egr = dmy(fecha_egr), estab = as.character(estab)),
#   datas[[5]] %>% 
#     head(n) %>% 
#     rename(serv_res = ser_res, sercle = serc_egr, estab = est) %>% # mod = estab?
#     mutate(benef = as.numeric(benef), fecha_egr = ymd(fecha_egr),
#            sercle = as.character(sercle), mod = as.numeric(mod)),
#   datas[[6]] %>% 
#     head(n) %>% 
#     rename(sercle = serc_egr) %>% 
#     mutate(estab = as.character(estab), fecha_egr = dmy(fecha_egr)),
#   datas[[7]] %>% 
#     head(n) %>% 
#     rename(fecha_egr = fec_egr, dias_estad = ds_estad, region = reg_res,
#            serv_res = ss_res) %>% 
#     mutate(mod = as.numeric(mod), fecha_egr = dmy(fecha_egr), estab = as.character(estab)),
#   datas[[8]] %>% 
#     head(n) %>% 
#     mutate(estab = as.character(estab)),
#   datas[[9]] %>% 
#     head(n) %>% 
#     rename(interv_q = iq) %>% 
#     mutate(interv_q = as.numeric(interv_q), estab = as.character(estab)),
#   datas[[10]] %>% 
#     head(n) %>% 
#     mutate(previ = as.numeric(previ), serc_egr = as.character(serc_egr),
#            comuna = as.numeric(comuna), region = as.numeric(region)),
#   datas[[11]] %>% 
#     head(n) %>% 
#     # rename(dias_estad = dias_est, interv_q = int_q) %>% 
#     mutate(previ = as.numeric(previ), serc_egr = as.character(serc_egr)),
#   datas[[12]] %>% 
#     head(n) %>% 
#     # rename(dias_estad = dias_est, interv_q = int_q) %>% 
#     mutate(previ = as.numeric(previ), serc_egr = as.character(serc_egr),
#            ser_salud = as.numeric(ser_salud), sexo = as.numeric(sexo),
#            edad = as.numeric(edad), benef = as.numeric(benef), mod = as.numeric(benef)),
#   datas[[13]] %>% 
#     head(n) %>% 
#     # rename(dias_estad = dias_est, interv_q = int_q) %>% 
#     mutate(previ = as.numeric(previ), serc_egr = as.character(serc_egr)),
#   datas[[14]] %>% 
#     head(n) %>% 
#     # rename(dias_estad = dias_est, interv_q = int_q) %>% 
#     mutate(previ = as.numeric(previ), serc_egr = as.character(serc_egr)),
#   datas[[15]] %>% 
#     head(n) %>% 
#     # rename(dias_estad = dias_est, interv_q = int_q) %>% 
#     mutate(previ = as.numeric(previ), serc_egr = as.character(serc_egr))
# )
