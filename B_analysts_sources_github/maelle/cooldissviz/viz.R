library("ggplot2")
library("dplyr")
library("emojifont")
library("gganimate")
list.emojifonts()
library("lubridate")
date1 <- ymd("2012-10-08")
date2 <- ymd("2016-03-07")
difftime(date2, date1, units = "days")

load.emojifont('OpenSansEmoji.ttf')

data <- readr::read_csv2("gestation.csv") %>%
  arrange(gestation) %>%
  mutate(animal = factor(animal,
                         levels = animal[order(gestation, 
                                               decreasing = TRUE)],
                         ordered = TRUE))

p <- ggplot(data) +
  geom_bar(aes(x = animal,
               y = gestation,
               frame = gestation,
               cumulative = TRUE,
               fill = color),
           stat = "identity") +
  scale_fill_manual(values = c("grey30",
                               "darkgoldenrod1")) +
  geom_text(aes(x = animal, 
                y = gestation + 45,
                frame = gestation,
                cumulative = TRUE,
                label = emoji(label)),
            family="OpenSansEmoji", size=8) +
  theme(axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        text = element_text(size=20),
        legend.position="none")+
  coord_flip() +
  xlab("Animal") +
  ylab("Gestation in days")
  
gg_animate(p, "gestation.gif",
           interval = c(rep(1,11), 4))