
width = 640;height=480;
border.muni <- c("Tijuana", "Cd. Juarez", "Nuevo Laredo", "Matamoros")

ct <- read.csv("data/cities.csv")
ct[is.na(ct)] <- 0
ct$rate.men <- ct$Masc / ct$Pop.Masc * 100000
ct$rate.women <- ct$Fem / ct$Pop.Fem * 100000
ct$rate <- ct$Total / ct$Grand.Total * 100000
m.ct <- melt(ct[,c(1:2,10:12)], id = c("Year","Municipality"))
m.ct <- subset(m.ct, Year <= 2007)


hwomen <- subMuni(m.ct, c("Miguel Hidalgo", "Cd. Juarez", "Toluca"))
hwomen.border <- subMuni(m.ct, border.muni)
hrates <- subMuni(m.ct, c("Toluca", "Cd. Juarez", "Badiraguato",
                          "Miguel Hidalgo"))
border <- subMuni(m.ct, border.muni)


########################################################
#Plot Murder rates comparing Ciudad Juarez
########################################################
dfs <- list(hwomen, hrates, border, hwomen.border)
subs <- list("rate.women", "rate", "rate", "rate.women")
titles <- list("Homicide rate for women (1990-2007)",
               "Homicide rates (1990-2007)",
               "Homicide rates in border cities (1990-2007)",
               "Homicide rates for women in border cities (1990-2007)")
filenames <- list("charts/women.png", "charts/rates.png",
                  "charts/border.png", "charts/women-border.png")

mapply(plotM, dfs, subs, titles, filenames)

jt <- read.csv("data/juarez-toluca.csv")
jt$rate <- jt$Hfemenino / jt$Femenino * 10^5
p <- ggplot(jt, aes(Year, rate, group = MA, color = MA)) +
  geom_line() +
  opts(title = "Homicide rates in the metropolitan areas of Ciudad Juárez and Toluca (1990 - 2007)") +
  xlim(1990, 2009)
savePlot(direct.label(p, "last.points"), "charts/juarez-toluca")
