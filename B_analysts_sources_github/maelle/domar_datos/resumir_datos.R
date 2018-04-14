library("dplyr")
load("data/meas100ail.RData")
glimpse(meas100ail)

# media para cada parametro
meas100ail %>% 
  group_by(parameter) %>% 
  summarize(mean = mean(value))

# o mas que la media
meas100ail %>% 
  group_by(parameter) %>% 
  summarize(mean = mean(value),
            min = min(value),
            max = max(value))

# es posible calcular la media para cada parametro i para cada año
library("lubridate")
meas100ail %>% 
  mutate(year = year(dateLocal)) %>%
  group_by(parameter, year) %>% 
  summarize(mean = mean(value),
            min = min(value),
            max = max(value))

# si queremos también tener el numero de observaciones
meas100ail %>% 
  group_by(parameter) %>% 
  summarize(mean = mean(value),
            min = min(value),
            max = max(value),
            n = n())