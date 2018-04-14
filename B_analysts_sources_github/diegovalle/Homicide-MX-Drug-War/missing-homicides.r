########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Fri Feb 05 20:34:20 2010
########################################################
#1. Time series of the different agencies that collect
#homicide data in Mexico
#2. Scatter plot of the INEGI homicide data vs the ICESI.
#Chihuahua is a big outlier
#3. Bar plot of the differences
#4. Scatter plot against the proportions

savePlotAA <- function(p, filename, width = 960, height = 600){
    Cairo(file = filename, width=width, height=height)
    print(p)
    dev.off()
}

########################################################
# Line plot of PAHO, UN, INEGI and ICESI homicide rates
########################################################
homts <- read.csv("missing-homicides/data/PAHO-UN-INEGI-ICESI.csv")
homts <- subset(homts, Year >= 1994)
names(homts)[4] <- "SNSP"
p <- ggplot(melt(homts, id="Year"), aes(Year, value, group = variable,
                                   color = variable)) +
       geom_line(alpha = .7, size = 1.2) +
       ylab("Homicide rate") +
      opts(title = "Different estimates of the \'homicide\' rate")
savePlotAA(p, file = "missing-homicides/output/PAHO-UN-INEGI-SNSP.png", width = 640, height = 480)

#FADE IN:
#LOWLY GOVERNMENT OFFICIAL Fernando is on the phone with a POWERFUL
#MEXICAN POLITICIAN named Felipe

#                    POWERFUL MEXICAN POLITICIAN

#(into phone)
#What the heck are you doing!



#                   FERNANDO

#(into phone)
#I'm deleting over 1,100 hundred murders from the police database boss



#                  POWERFUL MEXICAN POLITICIAN

#(into phone)
#Amigo: 2010 will be the 100th anniversary of the Mexican Revolution and the 200th anniversary of the Independence War, perhaps we should mark the occasion in a special way


#                  FERNANDO

#(into phone)
#Yes Jefe, in 2010 I will delete 2,100 murders from the database
#to mark the occasion


#                  POWERFUL MEXICAN POLITICIAN

#(into phone)
#Ja, ja, ja, pinche bola de pendejos, nadie se va a dar cuenta de
#lo que hicimos.
#Oye, ya nos dijo a que estado le vamos a dar en la madre a la
#siguiente.
#(English Subtitles)
#Please invest in Mexico, it is a safe an honest country

#FADE TO:

#EXT. INSIDE THE LOBBY OF A GOVERNMENT BUILDING - DAY

#FERNANDO breaks into song backed up by ten "edacanes"

#I am the very model of a modern Mexican politician
#With many cheerful facts about the murder rate.
#I'm quitting because of my party's political calculus;
#I know the president of my party is an animalculous:
#In short, in lying, cheating, and stealing
#I am the very model of a modern Mexican politician

#                                                           FADE OUT

#                  THE END


########################################################
# Scatter Plot
########################################################

ivsi <- read.csv("missing-homicides/data/INEGIvsICESI.csv")
#Get rid of the full name of the states (eg: Veracruz de
#Ignacio de la Llave changes to Veracruz
cleanNames <- function(df, varname = "County"){
  df[[varname]] <- gsub("* de .*","", df[[varname]])
  df[[varname]]
}

ivsi$State <- cleanNames(ivsi, "State")
ivsi$State <- iconv(ivsi$State, "windows-1252", "utf-8")
ivsi$Abbrv <- iconv(ivsi$Abbrv, "windows-1252", "utf-8")
print(ggplot(ivsi, aes(INEGI, ICESI,
                 label = paste(State," (",
                       as.character(INEGI-ICESI), ")", sep = ""))) +
       geom_text(aes(size = sqrt(abs(INEGI-ICESI))), hjust=-.1) +
       geom_point() +
       geom_abline(slope=1, linetype=2, color="blue") +
       opts(title = "Differences in \'homicide\' reporting rates (INEGI - SNSP)") + ylab("SNSP") +
       scale_x_continuous(limits = c(0, 4000)) +
       opts(legend.position = "none") +
       annotate("text", 1400, 1400, label = "missing-homicides/data ara equal ->",
                color ="blue", hjust = 1))
dev.print(png, file = "missing-homicides/output/scatter-inegi-snsp.png", width = 600, height = 480)




########################################################
# Bar Plot
########################################################
drawBars <- function(df) {
  values <- cast(df)
  labels <- data.frame(State = values$State,
                       variable = c("INEGI"),
                       value = ifelse(values$INEGI > ivsi[ ,3],
                                      values$INEGI,
                                      values[,3]),
                       missing = values[,2] - values[,3])
  labels$missing <- ifelse(labels$missing < 0,
                         as.character(labels$missing),
                         as.character(paste("+",
                                            labels$missing, sep="")))
  df$State <- with(df, reorder(factor(State), value))
  ggplot(df, aes(x=State, y=value, group = variable,
                    fill = variable)) +
         opts(title = "Differences in reported \'homicides\' (2008)") +
         geom_bar(stat = "identity", position = "identity",
                  alpha = .5) +
         ylab("Number of Homicides") +
         geom_text(data = labels, aes(label = missing), hjust=-.1,
                   color = "gray40") +
         scale_y_continuous(limits = c(0, 3000)) +
         coord_flip()
}
#ICESI data
names(ivsi)[3] <- "SNSP"
print(drawBars(melt(ivsi[ , c(1:2,3)], id="State")))
dev.print(png, file = "missing-homicides/output/INEGIvsSNSP.png", width = 480, height = 600)

#data from the statistical yearbooks
print(drawBars(melt(ivsi[ , c(1:2,6)], id="State")))
dev.print(png, file = "missing-homicides/output/INEGIvsYearbook.png", width = 480, height = 600)


########################################################
# Percentage Difference
########################################################
print(ggplot(ivsi, aes(INEGI, abs(INEGI - Stat.Yrbks) /
                 (Stat.Yrbks + INEGI) / 2, label = Abbrv)) +
    geom_text(alpha = .6)+
    #Not significant
    geom_smooth(method=lm, se = FALSE, color ="red") +
    scale_y_continuous(formatter = "percent") +
    xlab("Number of Homicides according to INEGI") +
    ylab("Percentage difference") +
    opts(title = "Statistical Yearbook and INEGI"))
dev.print(png, file = "missing-homicides/output/INEGIvsStatYrbksprop.png", width = 480, height = 480)

with(ivsi, {
     dep <- abs((INEGI - Stat.Yrbks) /
                 ((Stat.Yrbks + INEGI) / 2)  )
     summary(lm(INEGI ~ dep, data = ivsi))
 })

#How much did the number of homicide change. Compare the data from
#the SNSP witht that of the statistical yearbooks
print(ggplot(ivsi, aes(SNSP,
                 Stat.Yrbks / SNSP, label = Abbrv)) +
    geom_text(alpha = .6)+
    scale_y_continuous(formatter = "percent") +
    xlab("Number of Homicides according to ICESI") +
    ylab("Percentage extra according to the Statistical Yearbooks") +
    opts(title="SNSP and Statistical Yearbooks"))


########################################################
#Dot plot
########################################################

mivsi <- melt(ivsi[ , c(1:3,5)], id="State")
mivsi <- na.omit(mivsi)
mivsi$State <- with(mivsi, reorder(factor(State), value))
print(ggplot(mivsi, aes(value, State, group = variable,
                  fill = variable)) +
       opts(title = "Differences in reported \'homicides\'") +
       xlab("Number of Homicides") +
       geom_point(aes(color = variable, shape = variable),
                  size = 3, alpha = .5))
dev.print(png, file = "missing-homicides/output/INEGIvsSNSPvsYear.png", width = 480, height = 600)
