##########
# Notice #
##########

# This work is licensed by zonination under a Creative Commons Attribution-ShareAlike 4.0 International License.
# You may view, modify, and redistribute this code as long as:
#   1. You share under the same license as the original.
#   2. You give me credit, and indicate if changes were made.
# More information: http://creativecommons.org/licenses/by-sa/4.0/

#############
# Prep Work #
#############

# Let's open up our Zombie file and fire up the libraries.
setwd("~/Dropbox/R/Zombies")
zombies <- read.csv("zombies.csv", stringsAsFactors = F)
library(ggplot2)
library(gridExtra)

#We're going to apply a common visual theme to our graphs, inspired by minimaxir and FiveThirtyEight
#store as a function to use later:
z_theme <- function() {
  theme_bw(base_size=9) +
    #Background and Grid formatting
    theme(panel.background=element_rect(fill="#FFFFFF", color="#FFFFFF")) +
    theme(plot.background=element_rect(fill="#FFFFFF", color="#FFFFFF")) +
    theme(panel.border=element_rect(color="#FFFFFF")) +
    theme(panel.grid.major=element_line(color="#969696",size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    #Legend formatting
    theme(legend.background = element_rect(fill="#FFFFFF")) +
    theme(legend.text = element_text(size=14,color="#525252")) +
    theme(legend.title= element_blank())+
    #Axis & Title Formatting
    theme(plot.title=element_text(color="#525252", size=20, vjust=1.25)) +
    theme(axis.ticks=element_blank()) +
    theme(axis.text.x=element_text(size=14,color="#737373")) +
    theme(axis.text.y=element_text(size=14,color="#737373")) +
    theme(axis.title.x=element_text(size=16,color="#737373", vjust=0)) +
    theme(axis.title.y=element_text(size=16,color="#737373", vjust=1.25))
}

##########################
# Data Reshaping: Part 1 #
##########################

# Here's where the real guts and grit of this come into play. We're going to divide
# all 13 media into their own data frames, then divide those into speed,
# strength, and intelligence, and plot all 13 individually. Then, finally,
# we'll calculate the averages and put them back into one frame to do a final plot.

# 0. Let's convert 1:Least and 7:Greatest into actual integers.
zombies[zombies=="1: Least"]<-1
zombies[zombies=="7: Greatest"]<-7
zombies<-as.matrix(sapply(zombies, as.numeric))

# 1. The Walking Dead
walkingdead<-data.frame(zombies[,2:4])              # Grab our columns
names(walkingdead)<-c("intel","speed","strength")   # Rename the headers
walkingdead$what<-"The Walking Dead"                # Tell us what this is, bro.
walkingdead$media <- "TV Show"                      # TV Show, Video Game, or Movie?
walkingdead$kind <- "Infected"                      # What kind of zombies are these?

# 2. World War Z
worldwarz<-data.frame(zombies[,5:7])
names(worldwarz)<-c("intel","speed","strength")
worldwarz$what<-"World War Z"
worldwarz$media <- "Movie"
worldwarz$kind <- "Infected"

# 3. Shaun of the Dead
shaun<-data.frame(zombies[,8:10])
names(shaun)<-c("intel","speed","strength")
shaun$what<-"Shaun of the Dead"
shaun$media <- "Movie"
shaun$kind <- "Unclear"

# 4. Dawn of the Dead
dawn<-data.frame(zombies[,11:13])
names(dawn)<-c("intel","speed","strength")
dawn$what<-"Dawn of the Dead"
dawn$media <- "Movie"
dawn$kind <- "Unclear"

# 5. Game of Thrones
gameofthrones<-data.frame(zombies[,14:16])
names(gameofthrones)<-c("intel","speed","strength")
gameofthrones$what<-"Game of Thrones"
gameofthrones$media <- "TV Show"
gameofthrones$kind <- "Possessed"

# 6. Zombieland
zombieland<-data.frame(zombies[,17:19])
names(zombieland)<-c("intel","speed","strength")
zombieland$what<-"Zombieland"
zombieland$media <- "Movie"
zombieland$kind <- "Infected"

# 7. Firefly
firefly<-data.frame(zombies[,20:22])
names(firefly)<-c("intel","speed","strength")
firefly$what<-"Firefly"
firefly$media <- "TV Show"
firefly$kind <- "Psychotic"

# 8. Left4dead 2
left4dead<-data.frame(zombies[,23:25])
names(left4dead)<-c("intel","speed","strength")
left4dead$what<-"Left4Dead"
left4dead$media <- "Video Game"
left4dead$kind <- "Infected"

# 9. Army of Darkness
armyofdarkness<-data.frame(zombies[,26:28])
names(armyofdarkness)<-c("intel","speed","strength")
armyofdarkness$what<-"Army of Darkness"
armyofdarkness$media <- "Movie"
armyofdarkness$kind <- "Possessed"

# 10. Resident Evil
residentevil<-data.frame(zombies[,29:31])
names(residentevil)<-c("intel","speed","strength")
residentevil$what<-"Resident Evil"
residentevil$media <- "Video Game"
residentevil$kind <- "Infected"

# 11. Call of Duty
callofduty<-data.frame(zombies[,32:34])
names(callofduty)<-c("intel","speed","strength")
callofduty$what<-"Call of Duty"
callofduty$media <- "Video Game"
callofduty$kind <- "Unclear"

# 12. I am Legend
iamlegend<-data.frame(zombies[,35:37])
names(iamlegend)<-c("intel","speed","strength")
iamlegend$what<-"I am Legend"
iamlegend$media <- "Movie"
iamlegend$kind <- "Infected"

# 13. 28 Days Later
days28<-data.frame(zombies[,38:40])
names(days28)<-c("intel","speed","strength")
days28$what<-"28 Days Later"
days28$media <- "Movie"
days28$kind <- "Infected"

# Just for kicks, let's see how all the data sums up.
alldata<-rbind(walkingdead,worldwarz,shaun,dawn,gameofthrones,zombieland,firefly,left4dead,armyofdarkness,
               residentevil,callofduty,iamlegend,days28)

####################
# Plotting, Part 1 #
####################

# First thing we're going to do is plot Speed + Strength vs. Intelligence for all groups
# and then calculate out the barycenters. All plots will go into one faceted
# map, then we'll plot all the barycenters in a single plot.
# Unfortunately, I couldn't figure out how to split averages across facets, so
# I had to plot all 13 manually. A simple paperwork exercise of copy and paste, but
# be warned that the next section is a little verbose.

# 0.1. Showing all data; this is useless, but it shows how much work is left.
png(file=paste("zombies02.png",sep=""), width=900, height=600)
ggplot(alldata,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  ggtitle("All Data")+
  ylab("Intelligence")+
  xlab("Speed and Strength")+
  stat_smooth(method=lm,color="red",se=F)+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()
dev.off()

# 0.2. Showing all data with variables separated. Ditto from above.
png(file=paste("zombies03.png",sep=""), width=900, height=600)
ggplot(alldata,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,aes(color=what))+
  ggtitle("All Data")+
  ylab("Intelligence")+
  xlab("Speed and Strength")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()
dev.off()

# 1. The Walking Dead
plot1<-ggplot(walkingdead,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("The Walking Dead")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

#2. World War Z
plot2<-ggplot(worldwarz,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("World War Z")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

#3. Shaun of the Dead
plot3<-ggplot(shaun,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Shaun of the Dead")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 4. Dawn of the Dead
plot4<-ggplot(dawn,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Dawn of the Dead")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 5. Game of Thrones
plot5<-ggplot(gameofthrones,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Game of Thrones")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 6. Zombieland
plot6<-ggplot(zombieland,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Zombieland")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 7. Firefly
plot7<-ggplot(firefly,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Firefly")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 8. Left4Dead 2
plot8<-ggplot(left4dead,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Left 4 Dead 2")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 9. Army of Darkness
plot9<-ggplot(armyofdarkness,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Army of Darkness")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 10. Resident Evil
plot10<-ggplot(residentevil,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Resident Evil")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 11. Call of Duty
plot11<-ggplot(callofduty,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("Call of Duty")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 12. I am Legend
plot12<-ggplot(iamlegend,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("I am Legend")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# 13. 28 Days Later
plot13<-ggplot(days28,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  geom_point(aes(mean(speed+strength,na.rm=T),mean(intel,na.rm=T)),
             color="red",size=5,pch=3)+
  geom_density2d(alpha=.2)+
  ggtitle("28 Days Later")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  z_theme()

# Bring all plots together and party.
png(file=paste("zombies04.png",sep=""), width=1366, height=768)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             plot7, plot8, plot9, plot10, plot11, plot12, plot13,
             nrow=3, ncol=5)
dev.off()

#Let's see how it looks when controlling for TYPE of zombie
png(file=paste("zombies05.png",sep=""), width=900, height=600)
ggplot(alldata,aes(strength+speed,intel))+
  geom_jitter(size=4,alpha=.7,color="steelblue")+
  ggtitle("Controlling for Type")+
  ylab("Intelligence")+
  xlab("Speed and Strength")+
  geom_density2d(alpha=.5)+
  scale_x_continuous(limits=c(2,14))+
  scale_y_continuous(limits=c(1,7))+
  facet_wrap(~kind)+
  z_theme()
dev.off()
  
  #Let's see how it looks when controlling for MEDIA of zombie
png(file=paste("zombies06.png",sep=""), width=900, height=600)
  ggplot(alldata,aes(strength+speed,intel))+
    geom_jitter(size=4,alpha=.7,color="steelblue")+
    ggtitle("Controlling for Medium")+
    ylab("Intelligence")+
    xlab("Speed and Strength")+
    geom_density2d(alpha=.5)+
    scale_x_continuous(limits=c(2,14))+
    scale_y_continuous(limits=c(1,7))+
    facet_wrap(~media)+
  z_theme()
  dev.off()
  
#######################################
# Data Reshaping 2: Electric Boogaloo #
#######################################
  
# Time to compile our averages into a frame, so we can do our final graph.
avgframe<-data.frame(
  c(mean(armyofdarkness$speed,na.rm=T),mean(callofduty$speed,na.rm=T),mean(dawn$speed,na.rm=T),mean(days28$speed,na.rm=T),mean(firefly$speed,na.rm=T),mean(gameofthrones$speed,na.rm=T),mean(iamlegend$speed,na.rm=T),mean(left4dead$speed,na.rm=T),mean(residentevil$speed,na.rm=T),mean(shaun$speed,na.rm=T),mean(walkingdead$speed,na.rm=T),mean(worldwarz$speed,na.rm=T),mean(zombieland$speed,na.rm=T)),
  c(mean(armyofdarkness$strength,na.rm=T),mean(callofduty$strength,na.rm=T),mean(dawn$strength,na.rm=T),mean(days28$strength,na.rm=T),mean(firefly$strength,na.rm=T),mean(gameofthrones$strength,na.rm=T),mean(iamlegend$strength,na.rm=T),mean(left4dead$strength,na.rm=T),mean(residentevil$strength,na.rm=T),mean(shaun$strength,na.rm=T),mean(walkingdead$strength,na.rm=T),mean(worldwarz$strength,na.rm=T),mean(zombieland$strength,na.rm=T)),
  c(mean(armyofdarkness$intel,na.rm=T),mean(callofduty$intel,na.rm=T),mean(dawn$intel,na.rm=T),mean(days28$intel,na.rm=T),mean(firefly$intel,na.rm=T),mean(gameofthrones$intel,na.rm=T),mean(iamlegend$intel,na.rm=T),mean(left4dead$intel,na.rm=T),mean(residentevil$intel,na.rm=T),mean(shaun$intel,na.rm=T),mean(walkingdead$intel,na.rm=T),mean(worldwarz$intel,na.rm=T),mean(zombieland$intel,na.rm=T)),
  c(armyofdarkness$what[1],callofduty$what[1],dawn$what[1],days28$what[1],firefly$what[1],gameofthrones$what[1],iamlegend$what[1],left4dead$what[1],residentevil$what[1],shaun$what[1],walkingdead$what[1],worldwarz$what[1],zombieland$what[1]),
  c(armyofdarkness$media[1],callofduty$media[1],dawn$media[1],days28$media[1],firefly$media[1],gameofthrones$media[1],iamlegend$media[1],left4dead$media[1],residentevil$media[1],shaun$media[1],walkingdead$media[1],worldwarz$media[1],zombieland$media[1]),
  c(armyofdarkness$kind[1],callofduty$kind[1],dawn$kind[1],days28$kind[1],firefly$kind[1],gameofthrones$kind[1],iamlegend$kind[1],left4dead$kind[1],residentevil$kind[1],shaun$kind[1],walkingdead$kind[1],worldwarz$kind[1],zombieland$kind[1])
)
names(avgframe)<-c("speed","strength","intel","what","Medium","Type")

# I'm going to make my final visual theme be a little darker, to get the spookiness out here.
z_theme2 <- function() {
  theme_bw(base_size=9) +
    #Background and Grid formatting
    theme(panel.background=element_rect(fill="#000000", color="#000000")) +
    theme(plot.background=element_rect(fill="#000000", color="#000000")) +
    theme(panel.border=element_rect(color="#000000")) +
    theme(panel.grid.major=element_line(color="darkred",size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    #Legend formatting
    theme(legend.background = element_rect(fill="#000000")) +
    theme(legend.text = element_blank()) +
    theme(legend.title= element_blank())+
    theme(legend.position="none")+
    #Axis & Title Formatting
    theme(plot.title=element_text(color="darkred", size=20, vjust=1.25)) +
    theme(axis.ticks=element_blank()) +
    theme(axis.text.x=element_text(size=14,color="darkred")) +
    theme(axis.text.y=element_text(size=14,color="darkred")) +
    theme(axis.title.x=element_text(size=16,color="darkred", vjust=0)) +
    theme(axis.title.y=element_text(size=16,color="darkred", vjust=1.25))
}

# And now for the final plot. Thank you all, you've been a wonderful crowd!
png(file=paste("zombies01.png",sep=""), width=1366, height=768)
ggplot(avgframe,aes(speed+strength,intel))+
  geom_point(size=7,color="red",alpha=.7)+
  scale_x_continuous(breaks=c(6,8,10), labels=c("Weak","Strong or Fast","Destructive"))+
  scale_y_continuous(breaks=c(3,4,5), labels=c("Stupid","Intelligent","Brilliant"))+
  ggtitle("Zombie Danger")+
  ylab("")+
  xlab("")+
  geom_text(aes(label=what),alpha=.7,vjust=-1,color="red")+
  z_theme2()
dev.off()