###################################
### FIGURE 1
###################################

library(ggplot2)
library(scales)

data <- structure(list(month = structure(c(13330, 13361, 13392, 13422, 
13453, 13483, 13514, 13545, 13573, 13604, 13634, 13665, 13695, 
13726, 13757, 13787, 13818, 13848, 13879, 13910, 13939, 13970, 
14000, 14031, 14061, 14092, 14123, 14153, 14184, 14214, 14245, 
14276, 14304, 14335, 14365, 14396, 14426, 14457, 14488, 14518, 
14549, 14579, 14610, 14641, 14669, 14700, 14730, 14761, 14791, 
14822, 14853, 14883, 14914, 14944, 14975, 15006, 15034, 15065, 
15095, 15126, 15156, 15187, 15218, 15248, 15279, 15309), class = "Date"), 
    counts = c(204L, 168L, 226L, 361L, 315L, 363L, 207L, 240L, 
    196L, 192L, 494L, 210L, 218L, 110L, 118L, 138L, 211L, 140L, 
    123L, 115L, 132L, 178L, 213L, 198L, 150L, 114L, 103L, 143L, 
    167L, 124L, 162L, 526L, 378L, 190L, 253L, 271L, 234L, 231L, 
    223L, 439L, 356L, 302L, 232L, 222L, 319L, 480L, 372L, 267L, 
    342L, 249L, 299L, 304L, 288L, 206L, 290L, 258L, 328L, 371L, 
    406L, 321L, 235L, 111L, 172L, 209L, 261L, 214L)), .Names = c("month", 
"counts"), row.names = c(NA, 66L), class = "data.frame")

plot <- ggplot(data, aes(x=month, y=counts))
plot <- plot + geom_line() + scale_x_date() + 
	scale_y_continuous("Number of Articles, per Month") +
	theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x=element_blank()) + theme_bw()

plot

ggsave(plot, file="figure1a.pdf", width=7, height=3.5)

data <- structure(list(months = structure(c(15614, 15584, 15522, 15492, 
15461, 15431, 15400, 15371, 15340, 15309, 15279, 15248, 15218, 
15156, 15126, 15095, 15065, 15034, 15006, 14975, 14944, 14914, 
14883, 14853, 14791, 14761, 14730, 14700, 14669, 14641, 14610, 
14579, 14549, 14518, 14488, 14426, 14396, 14365, 14335, 14304, 
14276, 14245, 14214, 14184, 14153, 14123, 14061, 14031, 14000, 
13970, 13939, 13910, 13879, 13848, 13818, 13787, 13757, 13695, 
13665, 13634, 13604, 13573, 13545, 13514, 13483, 13453, 13422, 
13392, 13330, 13300, 13269, 13239, 13208, 13180, 13149, 13118, 
13088, 13057, 13027, 12965, 12935, 12904, 12874, 12843, 12815, 
12784, 12753, 12723, 12692, 12662, 12600, 12570, 12539, 12509, 
12478, 12449, 12418, 12387, 12357, 12326, 12296, 12234, 12204, 
12173, 12143, 12112, 12084, 12053, 12022, 11992, 11961, 11931, 
11869, 11839, 11808, 11778, 11747, 11719), class = "Date"), data = c(9.2, 
8.5, 12.2, 12.4, 9.3, 8.7, 12.2, 8.6, 12.3, 6, 5.4, 5.5, 4.4, 
7.4, 6.9, 5, 5.1, 4.6, 2.9, 2.4, 2.7, 3.3, 4.4, 3.1, 2.7, 4.5, 
4.4, 9.4, 3.2, 2.8, 2.9, 3.9, 10.4, 5.2, 1.4, 1.6, 1.9, 1.9, 
1.3, 2.2, 1.2, 0.4, 0.7, 0.8, 1, 0.4, 0.8, 1.3, 0.5, 0.4, 0.2, 
0.7, 0.7, 0.8, 0.5, 0.6, 0.7, 2.3, 1.6, 2.8, 1.7, 1.2, 1.5, 1.9, 
2.9, 3.1, 2, 1, 1, 1.4, 1.6, 2.3, 0.9, 0.4, 0.6, 0.6, 0.4, 0.4, 
0.4, 0.4, 0.3, 0.4, 0.7, 0.6, 0.5, 0.3, 0.7, 0.4, 0.6, 0.4, 0.7, 
0.8, 0.7, 0.4, 0.4, 1.2, 0.6, 0.6, 0.7, 0.9, 1.3, 2.3, 2.7, 0.1, 
0.3, 0.3, 0.3, 0.6, 0.8, 0.6, 0.8, 0.4, 0.8, 1, 0.7, 2.4, 1.4, 
0.9)), .Names = c("months", "data"), row.names = 20:137, class = "data.frame")

plot <- ggplot(data, aes(months, data))
plot <- plot + geom_line() + 
	scale_x_date(name="CIS Public Opinion Barometers, by date", 
		breaks=date_breaks("year"), labels=date_format("%Y")) +
	scale_y_continuous("% worried about corruption and fraud") +
	theme(axis.line = element_line(colour = "black"),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank()) + theme_bw()

plot

ggsave(plot, file="figure1b.pdf", width=7, height=3.5)



###################################
### FIGURE 2
###################################

library(ggplot2)
library(scales)

## data from "margins" command in Stata

data <- structure(list(lb = c(-0.0328489, -0.014859, -0.0680634), 
	mean = c(-0.0182562, 0.0188659, -0.041803), 
	ub = c(-0.0036635, 0.0525908, -0.0155426
), simulation = c("All types of corruption", "Welfare-enhancing", 
"Welfare-decreasing")), .Names = c("lb", "mean", "ub", "simulation"
), class = "data.frame", row.names = c(NA, -3L))

## simulating data for difference
x <- rnorm(1000, data$mean[2], (data$ub[2]-data$mean[2])/1.96)
y <- rnorm(1000, data$mean[3], (data$ub[3]-data$mean[3])/1.96)
z <- x - y
data[4,] <- c(quantile(z, .05), mean(z), quantile(z, .95), "Difference between\nwelfare-enhancing and\nwelfare-decreasing")

data$simulation <- factor(data$simulation, 
    levels=c("Difference between\nwelfare-enhancing and\nwelfare-decreasing", "Welfare-enhancing",
        "Welfare-decreasing", "All types of corruption"))
data$lb <- as.numeric(data$lb)
data$mean <- as.numeric(data$mean)
data$ub <- as.numeric(data$ub)

plot <- ggplot(data, aes(x=simulation, y=mean)) +
		geom_errorbar(width=.1, aes(ymin=lb, ymax=ub))+ 
		geom_point() + 
		scale_x_discrete("") +
		scale_y_continuous(name="90% Confidence Intervals for Marginal Effect", labels = percent_format(),
				limits=c(-0.08, 0.11)) + geom_hline(xintercept=0, color="grey40") + theme_bw() + 
				geom_vline(xintercept=1.5, color="grey20", linetype=3) + 
                geom_vline(xintercept=3.5, color="grey20", linetype=3) +
				coord_flip()
plot

ggsave(plot, file="figure2.pdf", width=6, height=3.2)


###################################
### FIGURE 3
###################################

library(ggplot2)
library(reshape)
library(scales)
library(foreign)

## simulated data generated by 'replication.do'
data <- read.csv("media-data.csv", stringsAsFactors=F)
data_melted <- melt(data, id=c("references", "simulation"))

## reading data set for rug plot
d <- read.dta("dataset.dta")
d <- d[!is.na(d$references),]
d$variable <- ifelse(d$w_enhance==1, "Welfare-Enhancing Corruption", "Welfare-Decreasing Corruption")
d$y <- 0
d <- d[,c("references", "variable", "y")]
d$simulation <- factor(d$variable, 
	levels=c("Welfare-Enhancing Corruption", "Welfare-Decreasing Corruption"))


p <- ggplot(data_melted, aes(references, value, group=variable))
pq <- p + theme_bw() + geom_line(aes(linetype=variable), show_guide=FALSE) + 
		geom_rug(data=d, aes(x=references, y=y, group=simulation), sides="b") + 
		facet_grid(. ~ simulation) +
		scale_linetype_manual(values = c(6,1,6), guide="none") +
		scale_x_log10("Number of References to Corruption Scandal in Newspapers") +
		scale_y_continuous("Marginal Effect on Vote Share", labels = percent_format()) +
		geom_hline(aes(yintercept=0), colour="grey40", linetype=3) + theme_bw()
		
pq

ggsave("figure3.pdf", plot=pq, width=7, height=3)









