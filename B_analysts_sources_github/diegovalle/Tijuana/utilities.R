clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  variable.name <- gsub('(^[0-9]+$)', 'X\\1', variable.name, perl = TRUE)
  return(variable.name)
}

clean.variable.name("2005")

#Save plots to png
saveAAPlot <- function(p, filename, filename.pdf, width = 700, height = 500) {
    Cairo(file = filename, width = width, height = height)
    print(p)
    dev.off()
    ggsave(filename.pdf, w = 8, h = 4)
}

savePlot <- function(p, filename, width = 800, height = 600) {
    png(file = filename, width = width, height = height)
    print(p)
    dev.off()
}

#Generate all the plots and save them as a list
generateCharts <- function(hom, year, name) {
 ll <- list()
 titles <- c("Daily Number of Homicides in",
                "Homicidios Semanales en",
                "Homicidios Mensuales en",
                "Kernel Density of Age at Death for all Homicides in",
                "Kernel Density of Age at Death for all Homicides by Year in",
                "Kernel Density of Age at Death for all Homicides by Sex in",
                "Homicides by Sex as a Percentage of All Homicides in",
                "Time of Death for all Homicide Victims in",
                "Homicides by Age Group as a Percentage of all Homicides in",
                "Number of Homicides by Age Group in",
                "Homicides by Firearm as a Percentage of all Homicides in",
                "Total Number of Homicides in",
                "Place where the Homicide Victims were Found in",
                "School Level of the Homicide Victims in",
                "Occupation of the Homicide Victims in",
                "Marital Status of the Homicide Victims in",
                "Homicides by Day of Week",
                "Los Homicidios fueron Certificados por",
                "Homicidios con o sin Necropsia en")

 titles <- str_c(titles, " ", name)
 titles[4] <- str_c(titles[4], " (", as.character(year), ")")
 titles[8] <- str_c(titles[8], " (", as.character(year), ")")
 titles[17] <- str_c(titles[17], " (", as.character(year), ")")


 hom.count <- formatDaily(hom)
 hom08 <- subset(hom, ANIODEF == year)

 ll$daily <- daily(hom.count, titles[1])
 ll$weekly <- weekly(hom.count, titles[2])
 ll$monthly <- monthly(hom.count, titles[3])
 

 ll$age.den <- ageDensity(hom, titles[4])
 ll$age.year <- ageDensityYear(hom, titles[5])
 ll$age.sex <- ageDensitySex(hom, titles[6])

 ll$sex.per <- bumpChart(subset(hom, SEXO != 0), "SEXOtxt",
                         scale = "Sex", title = titles[7])

 ll$time.of.day <- plotHours(hom, year, fix = TRUE, title = titles[8])


 age.groups <- c(0,15,20,25,30,35,40,45,50,55,60,65,Inf)

 ll$age.bump <- plotAgeBump(hom, age.groups, titles[9])
 ll$age.dot <- plotAgeDot(hom, age.groups, 2008, titles[10])

 ll$firearm <- plotFirearmPer(hom, titles[11])
 ll$total.hom <- totalHomicides(hom, titles[12])

 ########################################################
 #Places where people are most likely to die
 ########################################################
 ll$place.bump <- bumpChart(hom, "LUGLEStxt",
                            directlabel = FALSE,
                            title = titles[13],
                            scale = "Location")
 ll$place.dot <- dotPlot(hom08, "LUGLEStxt")


 ########################################################
 #Schooling
 ########################################################
 ll$school.bump <- bumpChart(hom, "ESCOLtxt", last.points,
                             title = titles[14],
                             scale = "Education Level")
 ll$school.dot <- dotPlot(hom08, "ESCOLtxt")


 ########################################################
 #Ocupation
 ########################################################
 #debug("bumpChart")
 ll$ocu.bump <- bumpChart(hom, "OCUPACIONtxt",
                         directlabel = FALSE,
                          title = titles[15],
                          scale = "Occupation")
 ll$ocu.dot <- dotPlot(hom08, "OCUPACIONtxt")


 ########################################################
 #Marital Status
 ########################################################
 ll$marital.bump <- bumpChart(hom, "EDOCIVILtxt",
                             directlabel = FALSE,
                              title = titles[16],
                              scale = "Marital\nStatus")
 ll$marital.dot <- dotPlot(hom08, "EDOCIVILtxt")

 ll$dayofDeath <- dayOfDeath(hom.count, year, titles[17])

 ll$certified <- doctorPlot(hom, "CERTIFtxt", titles[18],
          "Certificado por:")
 ll$autopsy <- doctorPlot(hom, "NECROPCIAtxt", titles[19],
          "Necropsia")
 
 ll
}


saveCharts <- function(ll, location) {
  filenames <- c("daily", "weekly", "monthly",
                 "age-den", "age-year", "age-sex", "sex-per",
                 "time-of-day", "age-bump", "age-dot",
                 "firearm", "total-homicides",
                 "place-bump", "place-dot",
                 "school-bump", "school-dot",
                 "occupation-bump", "occupation-dot",
                 "marital-bump", "marital-dot",
                 "dayofdeath", "certifies",
                 "autopsy")

  filenames.pdf <- str_c("pdfs/", location, "-", filenames,
                     ".pdf")
  filenames <- str_c("graphs/", location, "-", filenames,
                     ".png")
  

  i <- 1
  for(plot in ll) {
    message(str_c("Saving ", filenames[i], "\n"))
    saveAAPlot(plot, filenames[i], filenames.pdf[i])
    i <- i + 1
  }
}

chartRegion <- function(df, ents, muns, year,
                        chart.name, func = NULL) {
  file.prefix <- tolower(chart.name)
  hom.sub <- subset(df, ENTOCU %in% ents &
                          MUNOCU %in% muns)
  ll.charts <- generateCharts(hom.sub, year, chart.name)
  if(!is.null(func)) {
    llcharts$weekly <- llcharts$weekly + func
  } 
  saveCharts(ll.charts, file.prefix)
}
