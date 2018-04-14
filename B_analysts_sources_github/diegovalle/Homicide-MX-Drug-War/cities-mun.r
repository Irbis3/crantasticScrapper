########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Wed May 05 18:21:40 2010
########################################################
#This program does this and that

allmun <- data.frame()
for(i in c(1995,2000,2005:2008)) {
    f <- read.csv(paste("choropleths/output/map", i, ".csv", sep = ""))
    allmun <- rbind(allmun,f)
}

border <- list("Nuevo Laredo" = " 28 027 ",
               "Tijuana" =  "02 004",
               "Reynosa" = " 28 032 ",
               "Nogales" = " 26 043 ",
               "Matamoros" = " 28 022 ",
               "Juárez" = " 08 037 ",
               "Piedras Negras" = " 05 025 ",
               "Acuña" = " 05 002 ",
               "Mexicali" = " 02 002 ")

north <- list("Culiacán" = " 25 006 ",
           "Chihuahua" = " 08 019 ",
           "Durango" = " 10 005 ",
           "Mazatlán" = " 25 012 ",
           "Ensenada" = " 02 001 ",
           "Torreón" = " 05 035 ",
           "Saltillo" = " 05 030 ",
           "Monterrey" = " 19 039 ")

south.center <- list("Lázaro Cárdenas" = " 16 052 ",
                  "Toluca" = " 15 106 ",
                  "Acapulco de Juárez" = " 12 001 ",
                  "Cuernavaca" = " 17 007 ",
                  "Chilpancingo de los Bravo" = " 12 029 ",
                  "Texcoco" =  " 15 099 ",
                  "Uruapan" = " 16 102 ",
                  "Morelia" = " 16 053 ",
                  "Oaxaca de Juárez"= "20067",
                  "Tuxtla Gutiérrez" = " 07 101 ")

vacation <- list("Cabos, Los" = " 03 008 ",
                 "Ensenada" = " 02 001 ",
                 "Puerto Peñasco" = " 26 048 ",
                 "Benito Juárez" = " 23 005 ", #Cancún
                 "Acapulco de Juárez" = " 12 001 ",
                 "José Azueta" = " 12 038 ", #Zihuatanejo
                 "Paz, La" = " 03 003 ",
                 "Mazatlán" = " 25 012 ",
                 "Manzanillo" = " 06 007 ",
                 "Ciudad Madero" = " 28 009 ",
                 "Playas de Rosarito" = " 02 005 ")

plotCities <- function(cities, title, df) {
  mun.int <- subset(df, Code %in% sapply(cities, "[[", 1))
  mun.int$County.x <- iconv(mun.int$County.x, "windows-1252",
                            "utf-8")
  mun.int$County.x <- factor(mun.int$County.x)
  p <- ggplot(mun.int, aes(Year.of.Murder, rate, group = County.x,
                     color = County.x)) +
      geom_line(size = 1.5) +
      geom_point(size = 5) +
      coord_cartesian(xlim = c(1991.5,2009)) +
      opts(title = title) +
      theme_bw()
  print(direct.label(p, first.points))
  filename <- paste("most-violent-counties/output/municipalities-",
                    title, ".png", sep = "")
  dev.print(png, filename, width = 960, height = 600)
}
mapply(plotCities,
       list(south.center, north, border, vacation),
       list("Cities in Southern and Central Mexico",
            "Cities in Northen Mexico (excluding border cities)",
            "Mexican Cities Bordering the US",
            "Vacation Spots in Mexico"),
       MoreArgs = list(df = allmun))


mun08 <- subset(allmun, Population >= 100000 &
                Year.of.Murder == 2008)
mun08 <- mun08[order(-mun08$rate),][1:20,]
mun08$County.y <- iconv(mun08$County.y, "windows-1252",
                            "utf-8")
mun08$County.y <- factor(mun08$County.y)
mun08$County.y <- reorder(mun08$County.y, mun08$rate)
p <- ggplot(mun08, aes(County.y, rate)) +
    geom_point() +
    geom_segment(aes(xend = County.y, yend=0)) +
    coord_flip() +
    opts(title = "Most violent municipalities in 2008 (with more than 100,000 people)") +
    ylab("Homicide rate") + xlab("")
print(p)
dev.print(png, "most-violent-counties/output/most-violent-2008.png",
          width = 500, height = 600)
