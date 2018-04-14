# test plots
plot(df$lonWGS84, df$latWGS84)
# source("R/load-gs.R")
dir.create("figures")
png("figures/timeplot1.png")
plot(df$time, df$precip_mm)
dev.off()
