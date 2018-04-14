########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Fri Sep  7 18:04:50 2012
## Email: diegovalle at gmail.com
## Purpose: Time series of homicides in CA 
## Copyright (c) Diego Valle-Jones. All rights reserved


p <- ggplot(hom.ca, aes(Year, Rate, group = Country.or.Area,
                   color = Country.or.Area)) +
  geom_line(size = 1.1) +
  xlim(1995, 2018) +
  ggtitle("Homicide Rates in Central America")
direct.label(p, "last.bumpup")
ggsave("graphs/hom-ca.png", dpi = 100,
       width = 9, height = 5)

hom.ca.per <- subset(hom.ca, Year >= 2000)
hom.ca.per <- ddply(hom.ca.per, .(Country.or.Area), transform,
      per = Rate / Rate[length(Rate)])

p <- ggplot(hom.ca.per, aes(Year, per, group = Country.or.Area,
                        color = Country.or.Area)) +
                          geom_line(size = 1.1) +
                          scale_y_continuous(labels = percent) +
                          xlim(2000, 2015) +
                          ylab("homicide rate as a percentage of that in 2000") +
                          ggtitle("Homicide Rates in Central America as a Percentage of those in 2000")
direct.label(p, "last.bumpup")
ggsave("graphs/hom-ca-percentage.png", dpi = 100,
       width = 9, height = 5)



hom.ca.per7 <- subset(hom.ca, Year >= 2006)
hom.ca.per7 <- ddply(hom.ca.per7, .(Country.or.Area), transform,
      per = Rate / Rate[length(Rate)])

p <- ggplot(hom.ca.per7, aes(Year, per, group = Country.or.Area,
                        color = Country.or.Area)) +
                          geom_line(size = 1.1) +
                          scale_y_continuous(labels = percent) +
                          xlim(2006, 2013) +
                          ylab("homicide rate as a percentage of that in 2006") +
                          ggtitle("Homicide Rates in Central America as a Percentage of those in 2006")
direct.label(p, "last.bumpup")
ggsave("graphs/hom-ca-percentage07.png", dpi = 100,
       width = 9, height = 5)
