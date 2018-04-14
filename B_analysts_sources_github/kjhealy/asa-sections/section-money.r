
###--------------------------------------------------
### ASA Section revenues and membership
###--------------------------------------------------

### Revenue data:
### http://www.asanet.org/documents/ASA/pdfs/ASA_2014_Audit.pdf,
### Schedule 3

### Membership data:
### http://www.asanet.org/sections/CountsLastFiveYears.cfm

###----------------------------------------
### Setup
###--------------------------------------------------

library(ggplot2)
library(scales)
library(MASS)
library(stringr)
library(splines)
library(quantreg)


my.colors <- function (palette = "cb") {
    cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    rcb.palette <- rev(cb.palette)
    bly.palette <- c("#E69F00", "#0072B2", "#999999", "#56B4E9",
                     "#009E73", "#F0E442", "#D55E00", "#CC79A7")
    if (palette == "cb")
        return(cb.palette)
    else if (palette == "rcb")
        return(rcb.palette)
    else if (palette == "bly")
        return(bly.palette)
    else stop("Choose cb, rcb, or bly ony.")
}

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

###--------------------------------------------------


###--------------------------------------------------
### Data
###--------------------------------------------------

data <- read.csv("data/asa-section-membership.csv", header=TRUE)
## data$Sname <- str_replace(data$Sname, "Comm/Urban", "Comm\\\nUrban")
## data$Sname <- str_replace(data$Sname, "Mental Health", "Mental\\\nHealth")
## data$Section <- str_replace(data$Section, "\\(.*\\)", "")
data$Sname <- str_trim(data$Sname)


###--------------------------------------------------
### Figures
###--------------------------------------------------

pdf(file="figures/membership-v-revenues.pdf", height=5, width=7)
p <- ggplot(data, aes(x=X2014, y=Revenues, label=Sname))

p + geom_smooth(method = "rlm", se = FALSE,
                color = "gray80") +
    geom_point(aes(color = Journal)) +
    geom_text(data=subset(data, Revenues > 7000),
              size = 2,
              aes(x=X2014+10,
                  hjust = 0,
                  lineheight = 0.7)) +
    scale_y_continuous(labels = dollar) +
    scale_color_manual(values = my.colors("bly")) +
    labs(x="Membership",
         y="Revenues",
         color = "Section has own Journal") +
    theme(legend.position = "bottom") +
    ggtitle("ASA Sections, Membership vs Revenues")
dev.off()



pdf(file="figures/membership-v-balance.pdf", height=5, width=7)
p <- ggplot(data, aes(x=X2014, y=Ending, label=Sname))
p + geom_smooth(method="rlm", se=FALSE, color = "gray80") +
    geom_point(aes(color = Journal)) +
    geom_text(data=subset(data, Ending > 6000),
              size = 2,
              aes(x=X2014+10,
                  hjust = 0,
                  lineheight = 0.7)) +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma) +
    scale_color_manual(values = my.colors("bly")) +
    labs(x="Membership",
         y="End of Year Balance",
         color = "Section has own Journal") +
        theme(legend.position = "bottom") +
    ggtitle("ASA Sections 2014")
dev.off()


pdf(file="figures/revenues-v-expenses.pdf", height=8, width=7, pointsize = 14)
p <- ggplot(data, aes(x=Revenues, y=Expenses, label = Sname))
p + geom_smooth(method="lm", se=FALSE, color = "gray80") +
    geom_point(aes(color = Journal,
                   size = X2014),
               pch = 21) +
    geom_text(data=subset(data, Expenses/Revenues > 1.3 | Expenses/Revenues < 0.7 | Revenues > 10000),
              size = 2.2,
              aes(x = Revenues + 35,
                  hjust = 0,
                  lineheight = 0.7)) +
    scale_color_manual(values = my.colors("bly")) +
    scale_x_continuous(trans = log2_trans(), breaks = c(500, 1000, 5000, 20000) ,
                       labels = dollar) +
    scale_y_continuous(trans = log2_trans(), breaks = c(1000, 5000, 20000),
                       labels = dollar) +
    labs(x="Logged Dollar Revenues",
         y="Logged Dollar Expenses",
         color = "Section has own Journal",
         size = "Membership") +
    theme(legend.position = "bottom") +
    ggtitle("ASA Sections, Revenues vs Expenses (2014)")
dev.off()


pdf(file="figures/membership-v-balance-no-j.pdf", height=8.5, width=10)
p <- ggplot(subset(data, Journal == "No"), aes(x=X2014, y=Ending, label=Sname))
p + geom_smooth(method="lm", se=FALSE) +
    geom_point() +
    geom_text(data=subset(data, Journal == "No" & Ending > 7000),
              size = 2,
              aes(hjust = -0.15)) +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma) +
    labs(x="Membership",
         y="Ending Balance") +
    ggtitle("ASA Sections without Own Journal,\nMembership vs EOY Balance")
dev.off()


pdf(file="figures/eoy-balance.pdf", height=12, width = 10)
p <- ggplot(data, aes(x=Ending, y=reorder(Section, Ending, ordered = TRUE)))

p + geom_vline(color = "gray70") +
    geom_point() +
    scale_x_continuous(labels = dollar) +
    labs(x = "End of Year Balance",
         y = "") + ggtitle("End of Year Balance")
dev.off()


###--------------------------------------------------


###--------------------------------------------------
### Membership Trends
###--------------------------------------------------


library(tidyr)
library(dplyr)

yrs <- colnames(data) %in% paste("X", 2005:2015, sep="")

data.m <- subset(data, select = c("Sname", colnames(data)[yrs]))

data.m <- gather(data.m, Year, Members, X2005:X2015)

## data.m$Year <- as.Date(strptime(str_replace(data.m$Year, "X", ""),
## format="%Y"))

data.m$Year <- as.integer(str_replace(data.m$Year, "X", ""))

trend.tab <- data.m %>% group_by(Year) %>%
    mutate(yr.tot = sum(Members, na.rm=TRUE)) %>%
    group_by(Sname) %>%
    na.omit() %>%
    mutate(Ave = mean(Members, na.rm=TRUE),
           Dif = Members - Ave,
           Pct.All = round((Members/yr.tot*100), 2),
           Age = length(Members)) %>%
    group_by(Sname) %>%
    mutate(Index = (Members / first(Members, order_by = Year))*100,
           AveInd = mean(Index))


index.labs <- trend.tab %>%
    filter(Year == 2015) %>%
    ungroup() %>%
    filter(min_rank(desc(Index)) < 12 | min_rank(desc(Index)) > 44)


index.low <- trend.tab %>%
    filter(Year == 2015) %>%
    ungroup() %>%
    filter(min_rank(Index) < 12)


index.high <- trend.tab %>%
    filter(Year == 2015) %>%
    ungroup() %>%
    filter(min_rank(desc(Index)) < 12)




ind.all <- trend.tab$Sname %in% index.labs$Sname
ind.low <- trend.tab$Sname %in% index.low$Sname
ind.high <- trend.tab$Sname %in% index.high$Sname


trend.tab$Track.all <- ind.all
trend.tab$Track.low <- ind.low
trend.tab$Track.high <- ind.high



pdf(file="figures/sections-estd-index-decline.pdf", height=7, width=10)

p <- ggplot(subset(trend.tab, Age==11 & AveInd < 105),
            aes(x=Year, y=Index, group=Sname, color = Track.low))

p + geom_smooth(method = "rqss", formula = y ~ qss(x), se = FALSE) +
    geom_hline(yintercept = 100) +
    geom_text(data=subset(index.low, Age==11 & AveInd < 105),
              aes(x=Year+0.2, y=Index+rnorm(1, sd=0.8),
                  label=Sname,
                  lineheight=0.8),
              hjust = 0,
              color = "black",
              size = 2.9) +
    expand_limits(x = c(2005:2016)) +
    scale_color_manual(values = my.colors("bly")[c(3, 1)]) +
    scale_x_continuous(breaks = c(seq(2005, 2015, 3))) +
    guides(color = FALSE) +
    ggtitle("Declining Sections. 2005 = 100")

credit("Smoothed estimator. Excludes sections founded since 2005.")

dev.off()



pdf(file="figures/sections-estd-index-rise.pdf", height=7, width=10)

p <- ggplot(subset(trend.tab, Age==11 & AveInd > 104),
            aes(x=Year, y=Index, group=Sname, color = Track.high))

p + geom_smooth(method = "rqss", formula = y ~ qss(x), se = FALSE) +
    geom_hline(yintercept = 100) +
    geom_text(data=subset(index.high, Age==11 & AveInd > 104),
              aes(x=Year+0.2, y=Index,
                  label=Sname,
                  lineheight=0.8),
              hjust = 0,
              color = "black",
              size = 2.9) +
    expand_limits(x = c(2005:2016)) +
    scale_color_manual(values = my.colors("bly")[c(3, 1)]) +
    scale_x_continuous(breaks = c(seq(2005, 2015, 3))) +
    guides(color = FALSE) +
    ggtitle("Rising Sections. 2005 = 100")

credit("Smoothed estimator. Excludes sections founded since 2005.")

dev.off()



index.sub <- trend.tab %>%
    filter(Year == 2015 & Age < 10)



pdf(file="figures/sections-new-index-growth.pdf", height=6, width=8)
p <- ggplot(subset(trend.tab, Age<10), aes(x=Year, y=Members, group=Sname))
p + geom_line(color = my.colors("bly")[2]) +
    geom_text(data=subset(index.sub),
              aes(x=Year, y=Members,
                  label=Sname,
                  lineheight=0.6),
              hjust = 0,
              color = "black",
              size = 3) +
    expand_limits(x = c(2008:2016)) +
    scale_x_continuous(breaks = c(seq(2008, 2015, 3))) +
    guides(color = FALSE) +
    ggtitle("ASA Sections Founded Since 2005")
dev.off()

pdf(file = "figures/sections-faceted.pdf", height = 12, width = 10)
ind <- trend.tab %>%
    group_by(Sname) %>%
    summarize(Ave = mean(Members, na.rm =TRUE)) %>%
    mutate(Rank = order(Ave, decreasing = TRUE))

trend.tab$Sname2 <- factor(trend.tab$Sname,
                           levels = ind$Sname[ind$Rank],
                           ordered = TRUE)


p <- ggplot(trend.tab, aes(x=Year, y=Members, group=Sname2))
p + geom_line(color=my.colors("bly")[2]) +
    scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
    facet_wrap(~ Sname2, ncol = 7)
credit("Excludes sections founded since 2005.")

dev.off()

### Example of a convienience function to plot a particular section
plot.section <- function(section="Culture",
                         x = "Year",
                         y = "Members",
                         data = trend.tab,
                         smooth=FALSE){

    ## Note use of aes_string() rather than aes() to pass arguments to
    ## ggplot inside a function
    p <- ggplot(subset(data, Sname==section),
            aes_string(x=x, y=y))

    if(smooth == TRUE) {
        p0 <- p + geom_smooth(color = my.colors("bly")[2],
                              size = 1.2,
                              method = "lm",
                              formula = y ~ ns(x, 3)) +
            scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
            ggtitle(section)
    } else {

    p0 <- p + geom_line(color=my.colors("bly")[2], size=1.2) +
        scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
        ggtitle(section)

    }

    print(p0)

}

plot.section("Rationality")

plot.section("OOW", smooth = TRUE)

plot.section("Crim")


###--------------------------------------------------
### Work on a function to make it a bit more general
### e.g. passing through arguments.
### Plenty more could be done, e.g. calculating breaks
###--------------------------------------------------

plot.section2 <- function(section="Culture", x = "Year",
                         y = "Members", data = trend.tab,
                         smooth=FALSE, ...){
    require(ggplot2)
    require(splines)
    ## Note use of aes_string() rather than aes()
    p <- ggplot(subset(data, Sname==section),
            aes_string(x=x, y=y))

    if(smooth == TRUE) {
        p0 <- p + geom_smooth(color = my.colors("bly")[2],
                              size = 1.2, ...) +
            scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
            ggtitle(section)
        } else {
    p0 <- p + geom_line(color=my.colors("bly")[2], size=1.2) +
        scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
        ggtitle(section)
    }

    print(p0)
}

plot.section2("Comm/Urban", smooth = TRUE, method = "loess")

plot.section2("Children", smooth = TRUE, method = "lm", formula = y ~ ns(x, 2))

###--------------------------------------------------
### Quasi shingle-plot
###--------------------------------------------------

### See http://learnr.files.wordpress.com/2009/08/latbook_time1.pdf, p31

make.shingles <- function(data = trend.tab$Members, vname = "Members", number = 4, ...) {
    intrv <- as.data.frame(co.intervals(data, number, ...))
    intrv <<- round(intrv, 0) ## Create object outside fn
    Members <- sort(unique(data))
    intervals <- plyr::ldply(Members, function(x) {
      t(as.numeric(x < intrv$V2 & x > intrv$V1))
    })
    tmp <- reshape2::melt(cbind(Members, intervals), id.var = 1)
    out <- tmp[tmp$value > 0, 1:2]
    colnames(out)[1] <- vname
    return(out)
}

trend.ordered <- merge(trend.tab, make.shingles())

intrv <- with(intrv, paste(V1, V2, sep = "-"))
trend.ordered <- plyr::rename(trend.ordered, c(variable = "Range"))

trend.ordered$Range <- factor(trend.ordered$Range, labels = intrv)


p <- ggplot(trend.ordered,
             aes(x = Year,
                 y = Members,
                 group = Sname2,
                 label = FALSE))

p + geom_line(color = "gray70") +
    facet_grid(~ Range) +
    labs(x = "Year", y = "Members") +
    scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
    theme(legend.position = "none")


rev.shingle <- merge(data, make.shingles(data=data$X2014, vname = "X2014"))
intrv <- with(intrv, paste(V1, V2, sep = "-"))
rev.shingle <- plyr::rename(rev.shingle, c(variable = "Range"))
rev.shingle$Range <- factor(rev.shingle$Range, labels = intrv)

pdf(file="figures/quasi-shingle-plot.pdf", height = 5, width = 15)

p <- ggplot(rev.shingle, aes(x=Revenues, y=Expenses, label = Sname))
p + geom_smooth(method="lm", se=FALSE, color = "gray80") +
    geom_point(aes(color = Journal,
                   size = X2014),
               pch = 21) +
    geom_text(data=subset(rev.shingle, Expenses/Revenues > 1.3 | Expenses/Revenues < 0.7 | Revenues > 10000),
              size = 2.2,
              aes(x = Revenues + 35,
                  hjust = 0,
                  lineheight = 0.7)) +
    facet_grid(~ Range, scales = "free") +
    scale_color_manual(values = my.colors("bly")) +
    scale_x_continuous(trans = log2_trans(), breaks = c(500, 1000, 5000, 20000) ,
                       labels = dollar) +
    scale_y_continuous(trans = log2_trans(), breaks = c(1000, 5000, 20000),
                       labels = dollar) +
    labs(x="Logged Dollar Revenues",
         y="Logged Dollar Expenses",
         color = "Section has own Journal",
         size = "Membership") +
    theme(legend.position = "bottom") +
    ggtitle("ASA Sections, Revenues vs Expenses (2014)")

dev.off()

pdf(file="figures/quasi-shingle-plot-nojournal.pdf", height = 6, width = 15)

p <- ggplot(subset(rev.shingle, Journal == "No"), aes(x=Revenues, y=Expenses, label = Sname))
p + geom_smooth(method="lm", se=FALSE, color = "gray80") +
    geom_point(aes(size = X2014),
               pch = 21) +
    geom_text(data=subset(rev.shingle, Journal == "No" &
                                       Expenses/Revenues > 1.3 |
                                       Expenses/Revenues < 0.7 |
                                       Revenues > 10000),
              size = 2.2,
              aes(x = Revenues + 35,
                  hjust = 0,
                  lineheight = 0.7)) +
    facet_grid(~ Range, scales = "free") +
    scale_color_manual(values = my.colors("bly")) +
    scale_x_continuous(trans = log2_trans(), breaks = c(500, 1000, 5000, 20000) ,
                       labels = dollar) +
    scale_y_continuous(trans = log2_trans(), breaks = c(1000, 5000, 20000),
                       labels = dollar) +
    labs(x="Logged Dollar Revenues",
         y="Logged Dollar Expenses",
         size = "Membership") +
    theme(legend.position = "bottom") +
    ggtitle("ASA Sections, Revenues vs Expenses (2014)")

dev.off()

###--------------------------------------------------
### Lattice version of the shingle/coplot
###--------------------------------------------------
library(lattice)

data$Range <- equal.count(data$X2015, 4)

p <- xyplot(Expenses ~ X2015 | Range,
            data = data,
            fill.color = data$Journal,
            cex = 1.1,
            panel = function(x, y, fill.color, ..., subscripts) {
                fill <- fill.color[subscripts]
                panel.grid(h = -1, v = -1)
                panel.lmline(x, y, col = "gray60", lwd=1.5)
                panel.xyplot(x, y, pch = 21, fill = fill, ...)
            },
            xlab = "Members"
            )


print(p)

p <- xyplot(Revenues ~ X2015 | Range,
            data = subset(data, Journal == "No"),
            cex = 1.1,
            panel = function(x, y) {
                panel.grid(h = -1, v = -1)
                panel.lmline(x, y, lwd=1.5, col = "gray60")
                panel.xyplot(x, y, pch = 21)
            },
            xlab = "Members")

print(p)
