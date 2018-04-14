# Load Source and Libraries
# setwd("Path/To/File)
abc<-read.csv("abc.csv")
abc<-abc[,1:6]
library(ggplot2)
library(ggrepel)
library(scales)

# Plot Relative Frequency against Scrabble Frequency
ggplot(abc, aes(freq, scrab.amt/sum(abc$scrab.amt)))+
  geom_point(pch=21, fill="steelblue1", color="black", size=4)+
  geom_label_repel(aes(label=letter), size=2, box.padding = unit(.75, 'lines'))+
  # geom_label(aes(label=letter), size=2)+
  scale_x_continuous(limits=c(0, .15), labels=scales::percent)+
  scale_y_continuous(limits=c(0, .15), labels=scales::percent)+
  geom_abline(slope=1, intercept=0, alpha=.5, linetype=3)+
  labs(title="Scrabble Frequency",
       x="Relative Frequency of Letter",
       y="Number of Tiles in Scrabble Game")+
  theme_bw()
ggsave("scrabbefreq.png", height=5, width=9, dpi=120, type="cairo-png")

# Plot Relative Frequency against Scrabble Values
ggplot(abc, aes(freq, scrab.pts))+
  geom_point(pch=21, fill="steelblue1", color="black", size=4)+
  geom_label_repel(aes(label=letter), size=2, box.padding = unit(.75, 'lines'))+
  scale_x_log10(limits=c(0.0005, .15), breaks=10^(-4:-1),
                minor_breaks=c(seq(.0001, .001,.0001),
                               seq(.001 ,  .01, .001),
                               seq(.01  ,   .1,  .01),
                               seq(.1   ,    1,   .1)),
                               labels=scales::percent)+
  scale_y_continuous(limits=c(0,10), breaks=c(0:5)*2)+
  geom_smooth(type="lm", se=F, size=.33, color="black", linetype=4)+
  labs(title="Scrabble Frequency",
       x="Relative Frequency of Letter",
       y="Letter Points Score")+
  theme_bw()
ggsave("scrabbepoints.png", height=5, width=9, dpi=120, type="cairo-png")