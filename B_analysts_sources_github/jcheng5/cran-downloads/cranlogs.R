library(dplyr, warn.conflicts=FALSE)
library(ggplot2)

daily <- readRDS("daily.rds")
daily

# Take the data from 2014
perioddata <- daily %>% filter(date >= as.Date("2014-01-01"))
perioddata

# Take a look only at packages we're interested in
pkgdata <- perioddata %>% filter(package %in% c("BRugs", "rbugs", "R2WinBUGS"))
pkgdata

# Plot
ggplot(pkgdata, aes(x = date, y = count, color = package)) +
  expand_limits(y = 0) +
  geom_line()

# Seems a little rough. Let's smooth it out
ggplot(pkgdata, aes(x = date, y = count, color = package)) +
  expand_limits(y = 0) +
  geom_smooth(se = FALSE, method = "loess")

# Maybe too smooth... try less
ggplot(pkgdata, aes(x = date, y = count, color = package)) +
  expand_limits(y = 0) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4)
