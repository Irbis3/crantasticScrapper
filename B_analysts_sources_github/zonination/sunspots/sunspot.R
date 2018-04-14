# Set working directory, load libraries, load data
# setwd("c:/path/to/file") # needs to be uncommented to work
# Direct Download: http://www.sidc.be/silso/INFO/sndtotcsv.php
library(readr)
library(ggplot2)
spots <- read_delim("SN_d_tot_V2.0.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# Names of columns can be noted on this page: http://www.sidc.be/silso/infosndtot
names(spots)<-c("y","m","d","y.frac","ssn","sd","n.obs","def")

# Convert "-1" to "NA" values, as noted in webpage.
spots$ssn<-ifelse(spots$ssn==-1,NA,spots$ssn)

ggplot(spots,aes(y.frac,ssn))+
  geom_point(alpha=.1, size=.01, color="orangered")+
  scale_x_continuous(breaks=seq(1800, 2100, 50), minor_breaks = seq(1800, 2100, 10))+
  scale_y_continuous(breaks=seq(0, 600, 100), minor_breaks = seq(0, 600, 20))+
  labs(title="Daily Sunspot Number",
       x="Date",y="Sunspot Number",
       subtitle="The number of sunspots present on the surface of the sun",
       caption="created by /u/zonination and /u/thoughtso")+
  theme_bw()
ggsave("sunspots.png",height=5,width=9,dpi=120,type="cairo-png")