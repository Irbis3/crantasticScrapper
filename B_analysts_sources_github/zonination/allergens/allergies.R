# Load libraries
library(tidyverse)
library(lubridate)
library(ggjoy)

# List of allergens:
list<-c("birch", "cedar", "cottonwood", "elm", "grass", "hay",
        "maple", "oak", "ragweed", "nettle")

# Inidial data frame
df<-data.frame("week"=NA, "index"=NA, "allergen")
df<-subset(df, !is.na(index))

# Select and condition data
for(n in list){
  df1<-read_csv(paste(n, "5.csv", sep=""), skip=2)
  df1$allergen<-n
  names(df1)<-c("week", "index", "allergen")
  df<-rbind(df, df1)
}; rm(list); rm(n); rm(df1)

# Group by weeks
df$week<-week(df$week)
df<-group_by(df, week, allergen)
graph<-summarise(df, mean(index))
names(graph)<-c("week", "allergen", "index")
graph$allergen<-factor(graph$allergen,
                levels=rev(unique(graph$allergen)))

# Plot data
ggplot(graph, aes(x=ymd("2000-01-01")+weeks(week-1), y=allergen))+
  geom_joy(stat="identity", aes(height=index, fill=allergen))+
  scale_x_date(date_breaks="1 month", labels=scales::date_format("%b"))+
  guides(fill=F)+
  labs(title="U.S. Allergy Calendar",
       subtitle='based on Google Search Index for "___ allergy"',
       x="", y="",
       caption="created by /u/zonination")+
  theme_classic()+
  theme(axis.line = element_line(color="#888888"))+
  theme(panel.grid.major.y = element_line(color="#CCCCCC"))
ggsave("allergy.png", height=10, width=10, dpi=120, type="cairo-png")
