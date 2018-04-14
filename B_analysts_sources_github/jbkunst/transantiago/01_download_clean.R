# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stringr)
library(jsonlite)
library(rvest)
library(lubridate)
library(data.table)
library(scales)

dir.create("data")
dir.create("data/raw-rds")
dir.create("data/tbl-rds")
dir.create("data/day-rds")

# example -----------------------------------------------------------------
# url <- "http://mtt-scl.data.pedalean.com/pedalean/mtt-gz/2017/04/20/00/20170420000001.json.gz"
# data <- fromJSON(url)
# names(data)
# str(data, max.level = 2)

BASE_URL <- "http://mtt-scl.data.pedalean.com/pedalean"

# functions ---------------------------------------------------------------
"http://mtt-scl.data.pedalean.com/pedalean/?prefix=mtt-gz/2017/05/12/23/&max-keys=10000000"

get_urls_date <- function(date = ymd("20170507")) {
  
  urls <- 0:24 %>% 
    str_pad(2, pad = "0") %>% 
    file.path(
      "http://mtt-scl.data.pedalean.com/pedalean/?prefix=mtt-gz",
      format(date, "%Y"), 
      format(date, "%m"),
      format(date, "%d"),
      ., 
      "&max-keys=10000000") %>% 
    map(read_html) %>% 
    map(html_nodes, "key") %>% 
    map(html_text) %>% 
    reduce(c)
  
  urls
  
}

download_url <- function(url) { # url <- sample(urls, size = 1)

  message(url)
  
  file <- url %>% 
    basename() %>% 
    str_replace(".json.gz", ".rds") %>%
    file.path("data/raw-rds", .)
  
  if(file.exists(file)) {
    message("skipping")
    return(TRUE)
  }
  
  message("downloading")
  
  url %>% 
    file.path(BASE_URL, .) %>% 
    fromJSON() %>% 
    saveRDS(file)
  
}

raw_to_df <- function(f) { # f <- sample(files, size = 1)
  
  message(f)

  file <- f %>% 
    basename() %>% 
    file.path("data/tbl-rds", .) 
  
  if(file.exists(file)) {
    message("skipping")
    return(TRUE)
  }
  
  message("processing raw data")
  
  data <- readRDS(f)
  
  dpos <- data$posiciones %>% 
    map(str_split, ";") %>%
    map(unlist) %>% 
    map(t) %>% 
    map(as.matrix) %>% 
    reduce(rbind)
  
  dpos2 <- map_df(1:4, function(x){ # x <- 2
    dpos[, 1:12 + 12*(x - 1)] %>% 
      as_data_frame()
  })
  
  message("cleaning raw data")
  
  datac <- clean_df(dpos2)
  
  message("writing ", comma(nrow(datac)), " rows from ", basename(f), " to ", file)
  
  saveRDS(datac, file)
    
}

clean_df <- function(data) { # data <- dpos2
 
  names(data) <- c("fecha_hora", "patente", "lat", "lon", "velocidad",
                   "direccion", "op", "nombre", "sentido", "consola_ruta",
                   "sinoptico_ruta", "fecha_hora_insercion")
  
  # glimpse(data)
  
  # count(data, fecha_hora, patente)
  
  datac <- data %>% 
    select(fecha_hora, patente, lat, lon, velocidad, direccion, nombre, sentido) %>% 
    mutate_at(vars(lat, lon, velocidad, direccion), as.numeric) %>% 
    # separate(fecha_hora, c("fecha", "hora"), sep = " ") %>% 
    # separate(hora, c("hora", "minuto", "segundo"), sep = ":") %>% 
    # mutate(fecha = dmy(fecha), hora = hms(hora)) %>% 
    mutate(fecha_hora = as.POSIXct(fecha_hora, format = "%d-%m-%Y %H:%M:%S")) %>% 
    # filter(!is.na(fecha)) %>%
    filter(!is.na(fecha_hora)) 
  
  # arrange(data, patente, fecha_hora)
  # arrange(datac, patente, fecha_hora)
  # 
  # distinct(data)
  # count(data, fecha_hora, patente)
  # count(datac, fecha, hora, minuto, segundo, patente)
  # 
  # data
  # 
  # data %>%  filter(patente == "CJRS-65")
  # datac %>%  filter(patente == "CJRS-65")
  
  datac
  
}

process_day_data <- function(date = ymd("20170513")) {
 
  urls <- get_urls_date(date)
  
  map(sample(urls), download_url)
  
  files <- dir("data/raw-rds/", pattern = paste0("^", format(date, "%Y%m%d")), full.names = TRUE)
  
  map(sample(files), raw_to_df)
  
}

collect_day_data <- function(date = ymd("20170513")) {
  
  dy <- format(date, "%Y%m%d")
  
  message(dy)
  
  file <- file.path("data/day-rds", paste0(dy, ".rds"))
  
  if(file.exists(file)) {
    message("skipping")
    return(TRUE)
  }
  
  message("Reserving name")
  
  saveRDS(data_frame(), file)
  
  files <- dir("data/tbl-rds/", pattern = paste0("^", format(date, "%Y%m%d")), full.names = TRUE)
  
  message("Reading ", comma(length(files)), " files")

  data <- map_df(files, readRDS) # 38MM
  
  message("Files contains ", comma(nrow(data)), " rows")
  
  data <- distinct(data, fecha_hora, patente, .keep_all = TRUE) # 8MM
  
  data <- filter(data, date(fecha_hora) == date)
  
  data <- arrange(data, patente, fecha_hora)
  
  message("Files contains ", comma(nrow(data)), " distinct rows")
  
  saveRDS(data, file)
  
  rm(data)
  
  gc()
  
  
}

# download and process ----------------------------------------------------
d <- ymd("20170507") + 0:7

map(d, process_day_data)

map(sample(d), collect_day_data)


# validation --------------------------------------------------------------
fs1 <- dir("data/raw-rds/")
fs2 <- dir("data/tbl-rds/")

fs1[duplicated(fs1)]
fs2[duplicated(fs2)]

length(unique(fs1))
length(unique(fs2))

length(intersect(fs1, fs2))

fs1[1]
fs2[1]

if(length(fs1) > length(fs2)) {
  
  setdiff(fs1, fs2) %>% length()

} else {
  
  setdiff(fs2, fs1) %>% length()
  
}
