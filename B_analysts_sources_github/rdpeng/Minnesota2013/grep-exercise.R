homicides <- readLines("homicides.txt")


## Counting causes of death
## Method 1
reg <- regexec("<dd>Cause: +(.*?)</dd>", homicides)
mat <- regmatches(homicides, reg)
cause <- sapply(mat, function(x) x[2])
table(cause)  ## Double counts cause of death if capitalized name
cause <- tolower(cause)
table(cause)

## Method 2
reg <- regexec("icon_*[Hh]omicide_*(.*?),", homicides)
mat <- regmatches(homicides, reg)
cause <- sapply(mat, function(x) x[2])
table(cause)  ## Double counts cause of death if capitalized name
cause <- tolower(cause)
table(cause)


## Age distribution
reg <- regexec("([0-9]+) (years|year) old</dd>", homicides)
mat <- regmatches(homicides, reg)
age <- sapply(mat, function(x) ifelse(length(x) > 0, x[2], NA))
age <- as.numeric(age)
hist(age)


## Map
reg <- regexec("^([0-9]+\\.[0-9]+), (-[0-9]+\\.[0-9]+),", homicides)
mat <- regmatches(homicides, reg)
lat <- as.numeric(sapply(mat, function(x) x[2]))
long <- as.numeric(sapply(mat, function(x) x[3]))
library(maps)
par(mar = rep(0.2, 4))
map("county", "maryland,baltimore city", asp = 1)
points(long, lat, pch = 20, col = rgb(0, 0, 0, 0.3))

