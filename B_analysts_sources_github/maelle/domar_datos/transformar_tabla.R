library(dplyr)
library("tidyr")
load("data/meas100ail.RData")
# hay lineas repetidas
meas100ail <- unique(meas100ail)
# Qué hay aqui dientro?

glimpse(meas100ail)

# de long a wide
widemeas <- meas100ail %>% spread(parameter, value)
# si hago un error
meas100ail %>% spread(value, parameter)


# de wide a long
widemeas %>% gather(parameter, value, co:so2)
# o si solo quiero lineas con value
widemeas %>% gather(parameter, value, co:so2, na.rm = TRUE)

# date i time separados
meas100ail <- meas100ail %>% separate(dateLocal, c("date", "time"), sep = " ")
# de nuevo quiero que sea date i time
library("lubridate")
meas100ail <- meas100ail %>% mutate(date = as_date(ymd(date)),
                                    time = hms(time))

# value y unit juntos
meas100ail %>% unite(niceValue, c(value, unit), sep = " ")
