########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Mon Jan 25 21:46:30 2010
########################################################
#This program calculates the the probability that a marriage
#in Mexico ends in divorce

library(ggplot2)
library(Hmisc)
library(directlabels)
library(nlme)

source("load.r")

predictAll <- function(df, filename){
    probs <- cast(df)
    pred <- apply(probs[2:ncol(probs)], 2,
                  function(x) coef(lm(x ~ probs$Year.of.Marriage)))
    years <- 2006:2010
    pred08.10 <-
        data.frame(apply(pred, 2, function(x) x[1] + years * x[2]))
    names(pred08.10) <- names(probs)[2:ncol(probs)]
    pred08.10$Year.of.Marriage <- years
    allprobs <- rbind(probs[1:(nrow(probs) - 2),], pred08.10)
    allprobs[14:15,2] <- probs[14:15,2]
    allprobs[14,3] <- probs[14,3]
    write.csv(allprobs, filename)
    NULL
}



#Marriage rate and divorce rate per 1000
div <- read.csv("data/marriage-rate.csv", header=T)
div.rate <- (div$Divorce / div$Population) * 1000
mar.rate <- (div$Marriage / div$Population) * 1000
rates <- data.frame(mar.rate, div.rate)
mrates <- melt(rates)
ggplot(mrates, aes(x = 1993:2007, y = value, group=variable,
                   color=variable)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", space = "free")  +
  scale_colour_hue(name="Rates",
                   labels=c("Marriage Rate", "Divorce Rate"),
                   breaks=c("mar.rate", "div.rate")) +
  opts(title="Marriages and Divorces per Thousand People, Mexico 1993-2007") +
  xlab("Year")+ylab("Rate")    +
  theme_bw() +
  scale_y_continuous(breaks = c(.4,.55,.7,6,6.5,7,7.5))
dev.print(png, file="output/Marriages and Divorces per Thousand People, Mexico 1993-2007.png", width=640, height=480)

#According to Helping Couples Change
#http://books.google.com.mx/books?id=-YwsDr3fW1cC&pg=PA3&lpg=PA3&dq=Helping+Couples+Change+This+figure+touched+1.0+per+1000+in+1912;&source=bl&ots=pNoGsMu3bS&sig=feehHlHDKMJ46GOP4431hkQXqvA&hl=en&ei=cFU-S5K_GILqsQPYkLjHAQ&sa=X&oi=book_result&ct=result&resnum=1&ved=0CAgQ6AEwAA#v=onepage&q=&f=false
#This figure touched 1.0 per 1000 adults in 1912;
#in the US. So the divorce rate in
#Mexico is roughly what it was in the US in 1912!

#Marriage rate per 1000 adults 18+ 2005 (just an example)
595713/65061653 *1000
70184 /65061653 *1000

#Marriage rate per 1000 adults 15+  for the years 2001 and 2005-2007
#the population data is from the CONAPO
#divide the number of marriages by the sum of the number of people
#between 15-64 and those older than 65
marriages <- c( (665434 / (63190000 + 4950000)) * 1000,
                (595713 / (66001495 + 5404652 ) ) * 1000,
                (586978 / (67134774 + 5588666)) * 1000,
                (595209 / (68269297 + 5782286)) *1000)
divorces <- c( (57370 / (63190000 + 4950000)) * 1000,
               (70184 / (66001495 + 5404652 ) ) * 1000,
               (72396 / (67134774+5588666)) * 1000,
               (77255 / (68269297 + 5782286)) * 1000)
years <- c(2001,2005:2007)

mdf <- melt(data.frame(years,marriages,divorces),id="years")
ggplot(mdf, aes(years, value, group = variable, color = variable)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  opts(title="Marriages and Divorces per Thousand Adults (2001 and 2005-2007)") +
  ylab("Rate") +
  facet_grid(variable ~ .,scales = "free", space = "free")   +
  theme_bw() +
  scale_y_continuous(breaks = c(.85, 1, 8.5, 9, 9.5))
dev.print(png, file="output/Marriages and Divorces per 1000 Adults, 2001,2005-2007.png", width=640, height=480)




##################################################
#This is the interesting part. What are cumulative probabilities that
#a marriage ends in divorce after a given number of years
#################################################
divp <- read.csv("output/divorce-probs.csv")
names(divp)[1] <- "Year.of.Marriage"
mdivp <- melt(divp[c(1,5,9,13), ], id = c("Year.of.Marriage"))
mdivp$variable <- rep(0:14, each = 4)
ggplot(data=mdivp,aes(x = variable, y = value,
                      group = Year.of.Marriage,
                      linetype = factor(Year.of.Marriage))) +
  geom_line() +
  scale_y_continuous(formatter = "percent") +
  #scale_colour_hue(name="Year of\nmarriage",labels=c("1993","1997","2001","2005" )) +
  xlab("Years since wedding") +
  ylab("Proportion of marriages ending in divorce") +
  opts(title = "All Marriages Ending in Divorce, by Year of Marriage")  +
  theme_bw() +
  geom_text(data = mdivp[c(57,42,27,12), ], aes(x = c(13,9,5,1),
            label = c("1993","1997","2001","2005" )),
            hjust = 0, vjust = 0) +
  opts (legend.position = "none")
dev.print(png, file="output/Marriages Ending in Divorce, by Year of Marriage.png", width=600, height=400)

#Now some regressions to predict the probability of
#divorce in the future for each cohort
divp <- read.csv("output/divorce-probs.csv", header = T)
names(divp)[1] <- "Year.of.Marriage"
mdivp <- melt(divp, id = c("Year.of.Marriage"))
mdivp$variable <- rep(0:14, each = 15)

########################################################
#Multilevel Model
########################################################
mdivp2 <- na.omit(mdivp)
new <- mdivp[is.na(mdivp$value),]

mdivp2 <- subset(mdivp2, variable != 0)
mdivp2 <- mdivp2[order(mdivp2$Year.of.Marriage),]
reg <- lme(fixed = value ~ variable * Year.of.Marriage,
            random = ~ 1 | Year.of.Marriage,
            data = mdivp2)

mdivp2$pre <- predict(reg)
pre <- data.frame(predict(reg, new))
new$value <- unlist(pre)
new <- rbind(subset(mdivp, value > 0), new)
#
ggplot(new, aes(variable, value, group = Year.of.Marriage,
                color = factor(Year.of.Marriage))) +
       geom_line(data = mdivp, aes(variable, value,
              group = Year.of.Marriage), linetype = 1) +
       geom_line(linetype = 2) +
       scale_y_continuous(formatter = "percent") +
       xlab("Years since wedding") +
       ylab("Proportion of marriages ending in divorce") +
       scale_colour_grey("Year", start = .8, end = 0) +
       theme_bw() +
       opts(legend.position="none",
            title = "Projection of Divorce Probabilities" )
direct.label(last_plot(), "last.points")
dev.print(png, file="output/Multilevel-Marriages Ending in Divorce, by Year of Marriage.png", width=800, height=600)


predictAll(new, "output/divorce-probability-lme.csv")


########################################################
#Plain old regression
########################################################
lm_df <- function(df) {
  lm(value ~ variable, data = df)
}
mdivp <- melt(divp, id = c("Year.of.Marriage"))
mdivp$variable <- rep(0:14, each = 15)
dmodels <- dlply(subset(mdivp, variable > 0 & Year.of.Marriage < 2007),
                 .(Year.of.Marriage), lm_df)
dcoefs <- ldply(dmodels, function(x) c(coef(x)))
names(dcoefs)[2:3] <- c("intercept", "slope")
p <- subset(mdivp, is.na(value))
p <- merge(p, dcoefs, by = "Year.of.Marriage")
p$value <- p$intercept + p$variable * p$slope
p <- with(p, data.frame(Year.of.Marriage, variable, value))
p <- rbind(subset(mdivp, value > 0), p)
ggplot(subset(p, Year.of.Marriage < 2005), aes(variable, value,
                               group = Year.of.Marriage,
                               color = factor(Year.of.Marriage))) +
       geom_line(linetype = 2)+
       geom_line(data = subset(mdivp, Year.of.Marriage < 2004),
                 aes(x = variable, y = value,
                     group = Year.of.Marriage,
                     color = factor(Year.of.Marriage))) +
       scale_y_continuous(formatter = "percent") +
       xlab("Years since wedding") +
       ylab("Proportion of marriages ending in divorce") +
       scale_colour_grey("Year", start = .8, end = 0) +
       theme_bw() +
       #geom_text(data=mdivp[mdivp$variable == 14, ],
        #         aes(label = Year.of.Marriage), hjust = -.1,
         #            vjust = .2, size = 3, color = "black")+
       opts(legend.position="none",
            title = "Projection of Divorce Probabilities" )
direct.label(last_plot(), "last.points")
dev.print(png, file="output/Projection-Marriages Ending in Divorce, by Year of Marriage.png", width=800, height=600)


predictAll(p, "output/divorce-probability.csv")

#is there indeed a 7 year itch
#calculate probabilities by year (not cumulative)
#hahaha, it's more like a 4-6 year itch
divp <- read.csv("output/divorce-probs-byyear.csv")
names(divp)[1] <- "Year.of.Marriage"
mdivp <- melt(divp, id = c("Year.of.Marriage"))
mdivp$variable <- rep(0:14, each = 15)
mdivp <- na.omit(mdivp)
mdivp <- subset(mdivp, Year.of.Marriage < 2006)
itch <- ggplot(mdivp, aes(x = variable, y = value,
                          group = Year.of.Marriage,
                 color = factor(Year.of.Marriage))) +
       geom_line() +
       scale_y_continuous(formatter = "percent") +
       xlab("Years since wedding") +
       ylab("Proportion of a marriages ending in divorce each year") +
       opts(title = "All Marriages Ending in Divorce, by Year of Marriage")  +
       theme_bw() +
       scale_colour_grey("Year", start = .8, end = 0)
last.points <- dl.indep(data.frame(d[which.max(d$x),],hjust=0,vjust=0.5))
direct.label(itch, last.points)
dev.print(png, file="output/Prob. Marriages Ending in Divorce, by Year of Marriage.png",
  width=800, height=600)

#Average length of marriages that end in divorce
divp <- read.csv("output/marriages-length.csv")
mdivp <- melt(divp, id = "Year")
mdivp[is.na(mdivp)] <- 0
mdivp$variable <- as.numeric(substring(mdivp$variable, 2))
mdivp$duration <- mdivp$variable - mdivp$Year
mdivp <- subset(mdivp, duration >= 0)
means <- ddply(mdivp, .(variable),
               function(df) wtd.mean(df$duration, df$value))
qplot(1993:2007, means$V1, geom = "line")+
       xlab("Year") +
       ylab("Average lenght of marriages") +
       opts(title = "Average length of all marriages that ended in divorce in Mexico")  +
       theme_bw() +
       scale_y_continuous(limits = c(0, 14))
dev.print(png, file = "output/Marriage Length.png", width = 600, height = 400)

# a density function of marriages that ended in divorce
ggplot(data = mdivp, aes(x = duration, weight = value/sum(value),
                         group = variable, color = variable)) +
       geom_density()
