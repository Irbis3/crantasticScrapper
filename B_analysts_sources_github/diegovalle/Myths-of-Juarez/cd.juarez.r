

jm <- read.csv("data/juarez-month.csv")
jm <- subset(jm, Month != "No Especificado")

jm$Year <- rep(1985:2008, each = 12)
jm <- subset(jm, Year < 2008)
jm$Month2 <- rep(1:12)
jm$date <- as.Date(paste(jm$Year, jm$Month2,"15", sep = "-"))

jm <- merge(jm,
            data.frame(date = seq(as.Date("1985-01-15"),
                              as.Date("2007-12-15"), by = "month")),
            by = "date"
            )
jm[is.na(jm)] <- 0

guajardo <- as.Date("1993-03-12")

ggplot(jm, aes(date, Total)) +
    geom_line() +
    geom_vline(aes(xintercept = guajardo)) +
    scale_x_date() +
    annotate("text", x = guajardo-1000, y = 30,
             label = "Death of Aguilar Guajardo")
#dev.print(png, "charts/cd.juarez-historic-num.png", width = width, height = height)

jy <- read.csv("data/juarez.csv")
jy$prop <- jy$Female.Murders / jy$Male.Murders
p <- ggplot(jy, aes(Year, prop)) +
    geom_line() +
    scale_y_continuous(limits = c(0,.2))+
    opts(title = "Proportion of Homicide Victims who were Female") +
    scale_y_continuous(formatter = "percent")
savePlot(p, "charts/fem-proportion.png")

jm$prop <- jm$Female.Murders / (jm$Male.Murders + jm$Female.Murders)
jm$prop[is.nan(jm$prop)] <- 0
jm$prop[jm$prop == Inf] <- 1
ggplot(jm, aes(date, prop)) +
    geom_line() +
    scale_x_date()


#jm <- subset(jm, Year >= 1990)

pop.juarez <- c(833344, 863054, 896108, 931698, 968983, 1007227,
1045655, 1083380, 1119753, 1154393, 1186347,
1215085, 1240956, 1264647, 1287322, 1310302,
1334864, 1359787)#, 1384102)
pop.juarez <- data.frame(pop.juarez,
                         date = seq(as.Date("1990-06-15"),
                              as.Date("2007-06-15"), by = "year")
                         )
jm <- merge(jm, pop.juarez, by = "date", all.x = TRUE)
#544,496 pop 1980 http://es.wikipedia.org/wiki/Ciudad_Ju%C3%A1rez


jm$monthly <- na.spline(c(544496, rep(NA,59), jm$pop.juarez))[61:336]
jm$rate <- jm$Total / jm$monthly * 100000 * 12

p <- ggplot(jm, aes(date, rate)) +
    geom_line(color = "darkred", size = .3) +
    geom_vline(aes(xintercept = guajardo)) +
    scale_x_date() +
    ylab("annualized monthly homicide rate") +
    opts(title = "Monthly Homicide Rate in Ciudad Juarez (1985-2007)") +
    annotate("text", x = guajardo - 1350, y = 33,
             label = "Death of Aguilar Guajardo")

savePlot(p, "charts/cd.juarez-historic.png", AA = TRUE)
