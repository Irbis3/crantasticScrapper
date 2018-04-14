# experiments with animation

pkgs <- c(
  "animation",

  "ggmap",       # package for map visualisation
  "lubridate",   # package for converting time data
  "readr",
  "dplyr"
)
lapply(pkgs, library, character.only = TRUE)

mibig <- read_csv("~/Desktop/Eastings_Northings_MINAP_RL.csv")
sel <- is.na(mibig$easting) | is.na(mibig$northing)
mibig <- mibig[!sel,]
head(mibig)
mibig$yrmnth <- paste(mibig$year, mibig$arrival_month)
mibig$yrmnth <- paste(mibig$yrmnth, "01")
head(mibig)
mibig$Time <- lubridate::ymd(mibig$yrmnth)
mibig$time_month <- lubridate::round_date(mibig$Time, unit = "month")
head(mibig[c("Time", "time_month")])
mi <- mibig[sample(nrow(mibig), 1000),]


mi <- mibig[sample(nrow(mibig), 50000),]

u <- sort(unique(mi$Time))

mi$Time[mi$Time == min(mi$Time)] <- u[2]

s <- sample(nrow(mi), size = 150)
mi$Time[s] <-
  # create days in Jan 2003
  rep(u[1] + ddays(0:30), 5)

mi$Time[s]

u <- unique(mi$Time)
u <- sort(u)
length(u)

plot(mi$easting, mi$northing, col = "white")
oopt <- ani.options(interval = 0.08)
for(i in 1:ani.options("nmax")) {
  sel <- mi$Time == u[i]
  points(mi$easting[sel], mi$northing[sel])
  ## draw your plots here, then pause for a while with
  ani.pause()
}

ani.options(interval = 0.1, nmax = length(u))

saveGIF(expr = {
  for(i in 1:ani.options("nmax")) {
    plot(mi$easting, mi$northing, col = "white")
    sel <- mi$Time == u[i]
    points(mi$easting[sel], mi$northing[sel])
    ## draw your plots here, then pause for a while with
    ani.pause()}}, movie.name = "a.gif", img.name = "i.png"
  )


saveGIF(expr = {
  for(i in seq_len(ani.options("nmax"))){
    sel <- mi$Time < u[i]
    dev.hold()
    p <- ggplot(mi[sel,]) +
      geom_point(aes(easting, northing), alpha = 1, size = 1) +
      xlim(c(1470, 6540)) +
      ylim(c(310, 8450)) +
      geom_text(aes(label = year(u[i]), x = 5900, y = 8000)) +
      theme_nothing()
    print(p)
    #     plot(mi$easting, mi$northing, col = "white")
    #     points(mi$easting[sel], mi$northing[sel])
    ani.pause()
  }},
  movie.name = "a.gif", img.name = "i.png"
)


