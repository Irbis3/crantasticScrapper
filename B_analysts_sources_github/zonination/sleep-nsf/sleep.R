# Load library, set working directory, load data
# setwd("C:/path/to/file") # Uncomment to activate
sleep <- read.csv("sleep.csv")
# Source: https://sleepfoundation.org/media-center/press-release/national-sleep-foundation-recommends-new-sleep-times
library(ggplot2)
library(reshape2)

ggplot(sleep)+
  geom_ribbon(aes(x=age,ymin=min,ymax=max,fill=Recommendation),alpha=.7,color="black")+
  scale_fill_manual(values=c("#bdd7e7","#3182bd"))+
  scale_y_continuous(limits=c(0,20))+
  labs(title="Sleep Duration Recommendations",
       subtitle="From the National Sleep Foundation",
       x="Subject Age (Years)",
       y="Hours of Sleep",
       caption="created by /u/zonination")+
  theme_bw()
ggsave("Sleep.png",height=6, width=10, type="cairo-png", dpi=120)