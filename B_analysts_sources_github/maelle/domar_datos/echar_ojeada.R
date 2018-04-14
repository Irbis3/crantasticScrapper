library("dplyr")
load("data/latestData.RData")

# tbl_df es como un data.frame pero por ejemplo es mas facil mirar-lo.
# es una tabla de datos.
class(latestData)

# per ver lo que hay
print(latestData)
latestData
View(latestData)
glimpse(latestData)
summary(latestData)