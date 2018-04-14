# aim: download data used in the challenge

# install.packages("googlesheets")

library(googlesheets)
library(magrittr)

# setup
gs_gap() %>%
  gs_copy(to = "Gapminder")
#
# gs_ls()

gs1 <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1UnR-6bTRYz_QTZhpSl5_QgbCdrpMkuEui2YZgkCaFrg/edit#gid=1854940019")
df <- googlesheets::gs_read(gs1)
head(df)
dir.create("data")
# write.csv(df, "data/df.csv")

pred <- gs_read(ss = gs1, ws = 3)
head(pred)


