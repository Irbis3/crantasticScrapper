###--------------------------------------------------
### CA Vaccines
### Kieran Healy
### 1/29/15
### http://www.cdph.ca.gov/programs/immunize/pages/immunizationlevels.aspx
### Specifically,
### http://www.cdph.ca.gov/programs/immunize/Documents/2014-15%20CA%20Kindergarten%20Data.xlsx
###--------------------------------------------------


library(ggplot2)
library(dplyr)
library(stringr)
library(kjhutils)
library(pander)
cb.palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                    "#0072B2", "#D55E00", "#CC79A7")


## Clean/colhead version.
## There's a Los Angeles typo in the raw data
data <- read.csv("data/elementary-schools-1415-CA.csv", header=TRUE)

## Missing enrollment data
no.counts <- is.na(data$enrollment)

## Recalculate the exemption rate
data$Exempt <- round((data$PBE.n/data$enrollment)*100,2)
data$Med.Exempt <- round((data$PME.n/data$enrollment)*100,2)
data$Rel.Exempt <- round((data$RelPBE.n/data$enrollment)*100,2)


## Working data
data.sub <- subset(data, subset=!no.counts, select=c("code", "county", "name", "Type", "district", "city", "enrollment", "PBE.pct", "Exempt", "Med.Exempt", "Rel.Exempt"))

## Look
arrange(data.sub, desc(Exempt))[1:200,c("name", "county", "city","enrollment", "Exempt")]
state.rate <- mean(data.sub$Exempt, na.rm=TRUE)
summarize(data.sub, Mean.PBE = mean(Exempt, na.rm = TRUE))


###--------------------------------------------------
### Some new Variables
###--------------------------------------------------

## Inspecting School names
library(tm)
sc <- VCorpus(VectorSource(data.sub$name))
sc <- tm_map(sc, removeWords, c("SCHOOL", "SCHOOLS", "ELEMENTARY", "THE", "OF", "AND", "FOR", "LOS", "DEL"))

scm <- DocumentTermMatrix(sc)
scfreq <- colSums(as.matrix(scm))
o <- order(scfreq, decreasing = TRUE)
nlist <- toupper(names(scfreq[o[1:250]])) ## Most common words in School names, excluding


detach(package:tm)
detach(package:NLP)

library(tidyr)
## These won't isolate christian schools properly, vis "Christopher
## Dena Elementary", etc. Lutheran schools can have Saints' names
xtian.name <- str_detect(data.sub$name, "CHRISTIAN|FAITH|LUTHERAN|CHRIST |BAPTIST")
catholic <- str_detect(data.sub$name, "CATHOLIC|SAINT |ST\\. |PARISH|PAROCHIAL|DIVINE ")
other.relig.name <- str_detect(data.sub$name, "JEWISH|ISLAM")
mont <- str_detect(data.sub$name, "MONTESSORI")
waldorf <- str_detect(data.sub$name, "WALDORF")
charter <- str_detect(data.sub$name, "CHARTER")

data.sub$Type2 <- as.character(data.sub$Type)
data.sub$Type2[charter] <- "CHARTER"
data.sub$Type2 <- as.factor(data.sub$Type2)
data.sub$Kind <- data.sub$Type2
data.sub$Catholic <- catholic
data.sub$Christian <- xtian.name
data.sub$OtherRel <- other.relig.name
data.sub$Montessori <- mont
data.sub$Waldorf <- waldorf
data.sub <- unite(data.sub, "MWC", c(Catholic, Christian, OtherRel, Montessori, Waldorf, Type2))

library(car)

## Public schools are always public even if "Christ" appears in their
## name; e.g. "Christa McAuliffe Elementary School". Jewish and
## Islamic Schools are always Private. There are no Charter Waldorf
## schools. Lutheran schools can have Saints's names
##
data.sub$MWC <- car::recode(data.sub$MWC, "'FALSE_FALSE_FALSE_FALSE_FALSE_CHARTER'='Charter';
'FALSE_FALSE_FALSE_FALSE_FALSE_PRIVATE'='Private Non-Specific';
'FALSE_FALSE_FALSE_FALSE_FALSE_PUBLIC'='Public';
'FALSE_FALSE_FALSE_FALSE_TRUE_PRIVATE'='Private Waldorf';
'FALSE_FALSE_FALSE_FALSE_TRUE_PUBLIC'='Public';
'FALSE_FALSE_FALSE_TRUE_FALSE_CHARTER'='Charter Montessori';
'FALSE_FALSE_FALSE_TRUE_FALSE_PRIVATE'='Private Montessori';
'FALSE_FALSE_FALSE_TRUE_FALSE_PUBLIC'='Public Montessori';
'FALSE_FALSE_TRUE_FALSE_FALSE_PRIVATE'='Private Jewish/Islamic';
'FALSE_TRUE_FALSE_FALSE_FALSE_PRIVATE'='Private Christian';
'FALSE_TRUE_FALSE_FALSE_FALSE_PUBLIC'='Public';
'FALSE_TRUE_FALSE_TRUE_FALSE_PRIVATE'='Private Christian Montessori';
'TRUE_FALSE_FALSE_FALSE_FALSE_PRIVATE'='Private Catholic';
'TRUE_FALSE_FALSE_FALSE_FALSE_PUBLIC'='Public';
'TRUE_FALSE_FALSE_TRUE_FALSE_PRIVATE'='Private Montessori';
'TRUE_TRUE_FALSE_FALSE_FALSE_PRIVATE'='Private Catholic'", as.factor.result=TRUE, levels=c("Public", "Charter", "Private Non-Specific", "Private Christian", "Private Catholic", "Private Montessori", "Private Waldorf", "Charter Montessori", "Public Montessori", "Private Christian Montessori", "Private Jewish/Islamic"))

lutheran <- str_detect(data.sub$name, "LUTHERAN")
data.sub[lutheran, "MWC"] <- "Private Christian"

## This misses one Catholic montessori

detach(package:car)


###--------------------------------------------------
### Summary tables
###--------------------------------------------------

### By Pub/Private
by.type <- data.sub %>% group_by(Type) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm = TRUE))


### by specific type
by.mwc <- data.sub %>%  group_by(MWC) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Median.PBE = round(median(Exempt, na.rm=TRUE), 2), Max.PBE = round(max(Exempt, na.rm=TRUE), 2), Min.PBE = round(min(Exempt, na.rm=TRUE), 2), Schools=n(), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By County
by.county <- data.sub %>% group_by(county) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within County
school.by.county <- data.sub %>% group_by(county, name) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By City
by.city <- data.sub %>% group_by(city) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within City
school.by.city <- data.sub %>% group_by(city, code) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By District
by.district <- data.sub %>% group_by(district) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within District
school.by.district <-  data.sub %>% group_by(district, code) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within Type
school.by.type <- data.sub %>% group_by(MWC, name) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

filter(school.by.type, MWC=="Private Catholic") # Top Catholic PBE is
                                        # a homeschool service

###--------------------------------------------------
### Plots
###--------------------------------------------------

### County Level
p <- ggplot(by.county, aes(x=log(Students), y=Mean.PBE))
p1 <- p + geom_point(alpha=0.6) + theme_bw() +
    ylab("Percent of Students with a Personal Belief Exemption") +
        xlab("log N Students in County \n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, County Level") + scale_color_manual(values=cb.paltte)

### Pick out some outliers
ind <- with(by.county, (Mean.PBE>2*IQR(Mean.PBE)))
data.out <- droplevels(by.county[ind,])

pdf(file="figures/pbe-by-county.pdf", height=5, width=8)
p2 <- p1 + geom_text(data = data.out, aes(x=log(Students),
                         y=Mean.PBE, label=county), hjust=-0.1, size=2)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-county.png",
    p2,
    width=8,
    height=5,
    dpi=300
    )


## District Level
p <- ggplot(by.district, aes(x=log(Students), y=Mean.PBE, label=district))
p + geom_point(alpha=0.6) + theme_bw() +
    ylab("Percent of Students with a Personal Belief Exemption") +
        xlab("log N Students in District") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, District Level (Public Schools Only)") + ylim(0,65) + scale_color_manual(values=cb.palette)


p <- ggplot(by.district, aes(x=log(Students), y=Mean.PBE))
p1 <- p + geom_point(alpha=0.6) + theme_bw() +
    ylab("Percent of Students with a Personal Belief Exemption") +
        xlab("log N Students in District\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, District Level (Public Schools Only)") + ylim(0,65) + scale_color_manual(values=cb.palette)

### Pick out some outliers
ind <- with(by.district, (Mean.PBE>6*IQR(Mean.PBE)))
data.out <- droplevels(by.district[ind,])

p2 <- p1 + geom_text(data = data.out, aes(x=log(Students),
                   y=Mean.PBE, label=district), hjust=-0.05, size=2)


pdf(file="figures/pbe-by-district.pdf", width=10, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-district.png",
    p2,
    width=10,
    height=8,
    dpi=300
    )



## School level

## ID outliers
ind <- with(data.sub, (enrollment>200 & Exempt > 10))
data.out <- data.sub[ind,]


p <- ggplot(data.sub, aes(x=log(enrollment), y=Exempt, color=Type))
p1 <- p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Personal Belief Exemption") +
        xlab("log N Kindergarten Students\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + ylim(0,100) + scale_color_manual(values=cb.palette[c(2,6)]) + theme(legend.position="top")

p2 <- p1 + geom_text(data=data.out, aes(x=log(enrollment), y=Exempt, label=name), hjust=0.8, vjust=-1.2, size=2, alpha=1)


pdf(file="figures/pbe-by-school.pdf", width=8, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school.png",
    p2,
    width=8,
    height=8,
    dpi=300
    )


## School level, unlogged
ind <- with(data.sub, (enrollment>200 & Exempt > 10))
data.out <- data.sub[ind,]

p <- ggplot(data.sub, aes(x=enrollment, y=Exempt, color=Type))
p1 <- p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Personal Belief Exemption") +
        xlab("Number of Kindergarten Students\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + ylim(0,100) + scale_color_manual(values=cb.palette[c(2,6)]) + theme(legend.position="top")

p2 <- p1 + geom_text(data=data.out, aes(x=enrollment, y=Exempt, label=name), hjust=0.8, vjust=-1.2, size=2, alpha=1)



pdf(file="figures/pbe-by-school-unlogged.pdf", width=10, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school-unlogged.png",
    p2,
    width=10,
    height=8,
    dpi=300
    )



###--------------------------------------------------
### Alt type variable
###--------------------------------------------------

### Get the top schools of each type
ind <- with(data.sub, (enrollment>200 & Exempt > 10))
data.out <- data.sub[ind,]


p <- ggplot(data.out, aes(x=log(enrollment), y=Exempt, color=MWC))
p1 <- p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Personal Belief Exemption") +
        xlab("log N Kindergarten Students\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + ylim(0,100) + scale_color_manual(values=cb.palette) + theme(legend.position="top")

p2 <- p1 + geom_text(data=data.out, aes(x=log(enrollment), y=Exempt, label=name), hjust=0.8, vjust=-1.2, size=2, alpha=1) + labs(color="School Type")


pdf(file="figures/pbe-by-school-mwc.pdf", width=8, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school-mwc.png",
    p2,
    width=8,
    height=8,
    dpi=300
    )



###--------------------------------------------------
### MWC jittered boxplot
###--------------------------------------------------
library(RColorBrewer)

aux.info <- data.sub %>%  group_by(MWC) %>% summarize(Schools=n(), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit()
aux.info$Summary <- paste(aux.info$Schools, " Schools enrolling\n", aux.info$Students, " Kindergarteners", sep="")

make.jit.plot <- function(dat=data.sub,
                          pw=0.3, ph=0.25, palpha=0.4,
                          title="Vaccination Exemption Rates in California Kindergartens, by Type of School"){
    theme <- theme_set(theme_minimal())
    theme <- theme_update(panel.grid.major.x=element_blank())
    jit <- position_jitter(width=pw, height=ph)

    colorCount <- length(levels(dat$MWC))
    getPalette <- colorRampPalette(brewer.pal(8, "Set2"))

    p <- ggplot(dat, aes(y=PBE.pct, x=MWC, size=enrollment, fill=MWC))
    p1 <- p + geom_jitter(shape=21, position = jit, alpha=palpha, color="gray80")
    p2 <- p1 + xlab("") + coord_flip() + ggtitle(title) + guides(color=FALSE,
                                                                 shape=FALSE,
                                                                 fill=FALSE,
                                                                 size = guide_legend(override.aes = list(fill = "black"))) +
        scale_size(breaks=c(20, 40, 75, 100, 300), range=c(1,10)) + scale_color_manual(values=getPalette(colorCount)) + labs(size="Number of Kindergarteners in each School") +
            ylab("Percent of Kindergarteners with a Personal Belief Exemption from Vaccination\n") +
                theme(legend.position = "top")
 return(p2)
}

pdf(file="figures/pbe-by-school-type-jit.pdf", height=8, width=10, pointsize = 11)
p <- make.jit.plot()
p1 <- p + annotate("text", x=seq(1.25, 11.25, 1), y=92, label=aux.info$Summary, size=2.1)
print(p1)
credit("Data: California DPH, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school-type-jit.png",
    p1,
    width=10,
    height=8,
    dpi=300
    )

ggsave(
    "figures/pbe-by-school-type-jit.jpg",
    p1,
    width=10,
    height=8,
    dpi=300
    )


### County-level Jit Plot
data.co <- data.sub
library(gdata)
data.co$county <- reorder.factor(data.co$county, new.order = rev(as.character(by.county$county)))
detach(package:gdata)

ind <- data.co$PBE.pct<10

make.jit.plot <- function(dat=data.co[!ind,],
                          pw=0.25, ph=0.2, palpha=0.6,
                          title="Vaccination Exemption Rates in California Kindergartens by County: \nSchools with 10 percent PBEs or higher"){
    theme <- theme_set(theme_minimal())
    theme <- theme_update(panel.grid.major.x=element_blank())
    jit <- position_jitter(width=pw, height=ph)

    colorCount <- length(levels(dat$county))
    getPalette <- colorRampPalette(brewer.pal(8, "Set2"))

    p <- ggplot(dat,
                aes(y=PBE.pct, x=county, size=enrollment, fill=Kind))
    p1 <- p + geom_jitter(shape=21, position = jit, alpha=palpha, color="gray80")
    p2 <- p1 + xlab("") + coord_flip() + ggtitle(title) + guides(color=FALSE,
                                                                 shape=FALSE,
                                                                 size = guide_legend(override.aes = list(fill = "black", alpha=1))) +
        scale_size(breaks=c(30, 100, 250), range=c(1.2,6)) + scale_color_manual(values=getPalette(colorCount)) + labs(size="Number of Kindergarteners in each School") +
            ylab("Percent with a Personal Belief Exemption from Vaccination (10% or greater shown only)\n") +
                theme(legend.position = "top")
 return(p2)
}

pdf(file="figures/pbe-by-county-jit.pdf", height=22, width=8)
p <- make.jit.plot()
print(p)
dev.off()

ggsave(
    "figures/pbe-by-county-jit.png",
    p,
    width=8,
    height=20,
    dpi=300
    )


### Simpler version
dat <- by.mwc
o <- as.character(dat$MWC[order(dat$Mean.PBE)])
dat$Type <- factor(dat$MWC, levels=o, ordered=TRUE)
aux.info <- data.sub %>%  group_by(MWC) %>% summarize(Schools=n(), Students=sum(enrollment, na.rm=TRUE), Exempt=mean(Exempt, na.rm=TRUE)) %>% arrange(Exempt) %>% na.omit()
aux.info$Summary <- paste(aux.info$Schools, " Schools enrolling\n", aux.info$Students, " Kindergarteners", sep="")

p <-  ggplot(dat, aes(y=Mean.PBE, x=Type))
p1 <- p + geom_point(stat="identity", fill="grey50", aes(order=Mean.PBE), size=3) + xlab("") + ylim(0,60) + coord_flip() + ylab("Percent of Enrolled Kindergarteners with a Personal Belief Exemption\n\n")

pdf(file="figures/pbe-by-type-dot.pdf", height=6, width=9)
p2 <- p1 + annotate("text", x=seq(1, 11.25, 1), y=55, label=aux.info$Summary, size=2.1)
print(p2)
credit("Data: CDPH 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-type-dot.png",
    p2,
    width=9,
    height=6,
    dpi=300
    )


ggsave(
    "figures/pbe-by-type-dot.jpg",
    p2,
    width=9,
    height=6,
    dpi=300
    )


## library(productplots)
## library(Hmisc)
## data.sub$Exemptions <- factor(cut2(data.sub$Exempt, c(1,5,10,20,40,60,80)), labels=c("<1", "1-5", "5-10", "10-20", "20-40", "40-60", "60-80", "80-100"))
## data.sub$Type <- data.sub$MWC
## pdf(file="figures/pbe-by-type-mosaic.pdf", height=15, width=18)
## prodplot(data.sub, ~ Exemptions + Type, na.rm=TRUE, c("vspine", "hspine")) + theme(axis.text.x=element_text(angle = 90), axis.title.x=element_text(" ")) + coord_flip()
## dev.off()
## detach(package:Hmisc)
## detach(package:productplots)

pdf(file="figures/pbe-by-type-mosaic.pdf", height=12, width=13)
tab1 <- table(data.sub$Exemptions, data.sub$Type)
par(las=2)
mosaicplot(tab1, color=cb.palette, main="Exemption Rate", xlab="Percent")
dev.off()

jpeg(file="figures/pbe-by-type-mosaic.jpg", height=4096, width=4000, quality=95, res=300)
tab1 <- table(data.sub$Exemptions, data.sub$Type)
par(las=2)
mosaicplot(tab1, color=cb.palette, main="Exemption Rate", xlab="Percent")
dev.off()


###--------------------------------------------------
### Correlations
###--------------------------------------------------

## Medical exemptions are separate from PBEs
pp <- ggplot(data.sub, aes(x=Exempt, y=Med.Exempt, color=Type, size=log(enrollment)))
p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Medical Exemption") +
        xlab("Percent of Kindergarten Students with a Personal Belief Exemption") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + scale_color_manual(values=cb.palette[c(2,6)]) + theme(legend.position="top")


## Religous exemptions are a subset of personal belief exemptions
p <- ggplot(data.sub, aes(x=Exempt, y=Rel.Exempt, color=Type, size=log(enrollment)))
p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Religious Exemption") +
        xlab("Percent of Kindergarten Students with a Personal Belief Exemption") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + scale_color_manual(values=cb.palette[c(2,6)]) + theme(legend.position="top")



###--------------------------------------------------
### Beeswarm plot
###--------------------------------------------------

library(RColorBrewer)

library(ggbeeswarm)


aux.info <- data.sub %>%  group_by(MWC) %>% summarize(Schools=n(), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit()
aux.info$Summary <- paste(aux.info$Schools, " Schools enrolling\n", aux.info$Students, " Kindergarteners", sep="")

## Format the numbers with commas
aux.info$Summary2 <- paste(formatC(aux.info$Schools, format="d", big.mark = ","),
                           " Schools\n",
                           formatC(aux.info$Students, format="d", big.mark=","),
                           " Kindergarteners", sep="")

aux.info$School.labs <- c("Public", "Charter", "Private\nNon-Specific",
                          "Private\nChristian", "Private\nCatholic", "Private\nMontessori", "Private\nWaldorf", "Charter\nMontessori", "Public\nMontessori", "Private\nChristian\nMontessori",
                          "Private Jewish\nor Islamic")

## Force newlines for top annotation
addline_format <- function(x,...){
    gsub('\\s','\n',x)
}

make.bee.plot <- function(dat=data.sub,
                          balpha=0.3,
                          bwidth=0.9,
                          varwidth=FALSE,
                          method="quasirandom",
                          title="Vaccination Exemption Rates in California Kindergartens",
                          subtitle="Percent of Kindergarteners with a Personal Belief Exemption, by Type and Size of School."){
    theme <- theme_set(theme_minimal())
    theme <- theme_update(panel.grid.major.x=element_blank())

    colorCount <- length(levels(dat$MWC))
    getPalette <- colorRampPalette(brewer.pal(8, "Set2"))

    p <- ggplot(dat, aes(y=PBE.pct, x=MWC, size=enrollment, fill=MWC))

    p1 <- p + geom_quasirandom(shape=21, alpha=balpha,
                               color="gray30",
                               method=method,
                               varwidth=varwidth,
                               bandwidth=bwidth,
                               position=position)

    p2 <- p1 + xlab("") + ggtitle(title, subtitle=subtitle) + guides(color=FALSE,
                            shape=FALSE,
                            fill=FALSE,
                            size = guide_legend(override.aes =
                                                    list(fill = "black"))) +
        scale_size(breaks=c(20, 40, 75, 100, 300),
                   range=c(1,10)) +
        scale_color_manual(values=getPalette(colorCount)) +
        labs(size="Number of Kindergarteners in each School") +
            ylab("Percent") +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x = element_blank())
 return(p2)
}

pdf(file="figures/pbe-by-school-type-bee.pdf", height=8, width=12, pointsize = 12)

aux.info.sub <- subset(aux.info, MWC %nin% c("Private Jewish/Islamic", "Private Christian Montessori"))
auxlen <- nrow(aux.info.sub)

p <- make.bee.plot(dat=subset(data.sub, MWC %nin% c("Private Jewish/Islamic", "Private Christian Montessori")),
                   bwidth=0.7,
                   method="quasirandom")
p1 <- p + annotate("text", x=seq(1, auxlen, 1), y=-4,
                   label=aux.info.sub$Summary2, size=2)
p2 <- p1 + annotate("text", x=seq(1, auxlen, 1), y=-10, size=3,
                    fontface="bold",
                    label=addline_format(aux.info.sub$MWC))

print(p2)
credit("Data: California DPH, 2015.\nKieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school-type-bee.jpg",
    p2,
    width=12,
    height=8,
    dpi=300
    )
