# Set song
songname<-"Miley Cyrus - Wrecking Ball"

# Load libraries, load FretMap, and working directory
# setwd("C:/path/to/folder") # Switch all "\" to "/"
library(tidyverse)
source("fretmap.R")

# Initialize DataFrame
df<-data.frame(matrix(nrow=0, ncol=2))
names(df)<-c("String", "Fret")

# Import the tabs for the song
songtabs<-scan(paste("songs/", songname, ".txt", sep=""),
               what="string")

# Import tabs from our list and append to our DataFrame
for(n in 1:length(songtabs)){
  eval(parse(text=paste("df<-rbind(df, as.data.frame(tabs$",
                        songtabs[n],
                        "))",
                        sep="")))
}; rm(n)

# Summarize our data frame
map<-summarise(group_by(df, String, Fret), heat=length(Fret))

# Plot the data
ggplot(map, aes(
          x=factor(String, levels=c(NA, 1:6),
                  labels=c("E","A","D","G","B","e")),
          y=Fret,
          size=heat))+
  geom_point()+
  scale_y_reverse(limits=c(6.5, 0.5), breaks=1:6, minor_breaks=seq(.5, 6.5, 1))+
  labs(title="Heatmap of Guitar Tabs",
       subtitle=paste("for", songname),
       x="String", y="Fret",
       size="",
       caption="created by /u/zonination")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.major.y=element_blank())