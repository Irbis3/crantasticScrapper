setwd("C:/Users/Cait/Dropbox/Research/Social Opportunity/june2016_6rem/csvs")
setwd("C:/Users/caitl/Dropbox/Research/Social Opportunity/june2016_6rem/csvs")
beh <- read.csv("beh.csv")
head(beh)

#### splitting nonrising and rising

risers <- beh[beh$status == "riser", ]
nonrisers <- beh[beh$status == "nonriser", ]

### looking at day of removal wins and losses 

### comparing ## of wins 
wilcox.test(risers$winsafter, nonrisers$winsafter) ### W = 138, p-value = 0.0001482

### comparing ## of losses 
wilcox.test(risers$lossesafter, nonrisers$lossesafter) ### W = 56.5, p-value = 0.3006
### no signif difference in # of losses. 


### comparing day before removal to day of removal wins and losses 

## risers wins before v. after 
wilcox.test(risers$winsbefore, risers$winsafter, paired = TRUE) ## V = 1, p = 0.003
## risers losses before v. after 
wilcox.test(risers$lossesbefore, risers$lossesafter, paired = TRUE) ## V = 48, p = 0.037

## nonrisers wins before v. after 
wilcox.test(nonrisers$winsbefore, nonrisers$winsafter, paired = TRUE) ## V = 21, p = 0.54
## nonrisers losses before v. after 
wilcox.test(nonrisers$lossesbefore, nonrisers$lossesafter, paired = TRUE) ## V = 16, p = 0.832



### figures

library(ggplot2)
library(viridis)

ggplot(beh, aes(status, wins, mouse, group = status, color = status, fill = status))+   
  geom_boxplot(size = .8, alpha = .1)+
  ylab("Number of Wins") +
  xlab("Mouse Status")+
  scale_color_viridis(option = "viridis", discrete = TRUE)+
  scale_fill_viridis(option = "viridis", discrete = TRUE)+
  scale_x_discrete(labels=c("beta \n alpha remained", "beta \n alpha removed"))+
  newggtheme

ggplot(beh, aes(status, wins, mouse, group = status, color = status, fill = status))+   
  geom_boxplot(size = .8, alpha = .1)+
  ylab("Number of Wins") +
  xlab("Mouse Status")+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  scale_x_discrete(labels=c("beta \n alpha remained", "beta \n alpha removed"))+
  newggtheme



ggplot(beh, aes(status, losses, mouse, group = status, color = status, fill = status))+   
  geom_boxplot(size = .8, alpha = .1)+
  ylab("Number of Losses") +
  xlab("Mouse Status")+
  scale_color_manual(values = c("purple4", "orange"))+
  scale_fill_manual(values = c("purple4", "orange"))+
  scale_x_discrete(labels=c("beta \n alpha remained", "beta \n alpha removed"))+
  newggtheme


