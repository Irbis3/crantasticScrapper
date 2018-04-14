
########################################################
#Gini coefficient of Ciudad Juarez
########################################################
gini <- read.csv("data/gini.csv.bz2")
gini$code <- rownames(gini)
juaritos <- gini[235,]
gini$code <- reorder(gini$code, -gini$Gini)
p <- ggplot(gini, aes(code, Gini)) +
    geom_point(alpha = .1) +
    geom_point(data = juaritos, aes(code, Gini), color = "red",
               size= 4) +
    annotate("text", x = "235", y = .41, label = "Cd. Juarez",
             hjust = 1) +
    opts(title = "Gini Coefficient (2005)") +
    ylab("gini coefficient (lower is more equal)") +
    xlab("Municipalities") +
    scale_x_discrete(breaks = NA) +
    scale_y_continuous(limits = c(0,.7))
savePlot(p, "charts/gini.png")


########################################################
#GDP per Capita
########################################################
pib <- read.csv("data/pib.csv.bz2")
pib$Clave <- as.factor(pib$Clave)
juaritos <- pib[235,]
pib$Clave <- reorder(pib$Clave, pib$PIB)
p <- ggplot(pib, aes(Clave, PIB)) +
    geom_point(alpha = .1) +
    geom_point(data = juaritos, aes(Clave, PIB), color = "red",
               size= 4) +
    annotate("text", x = "235", y = 13700, label = "Cd. Juarez",
             hjust = 1.5) +
    opts(title ="GDP per Capita (2005)") +
    ylab("ppp dollars") + xlab("Municipalities") +
    scale_x_discrete(breaks = NA) +
    scale_y_continuous(limits = c(0,33000))
savePlot(p, "charts/pib.png")




########################################################
#Single Moms in Ciudad Juarez
########################################################
births <- read.csv("data/illeg-mun.csv.bz2", stringsAsFactors = FALSE)
births <- subset(births,births$Name!="#NAME?")
births$Code2 <- as.numeric(gsub(" ", "", births$Name))

pop<-read.csv("data/poblacionmunicipales.csv.bz2",
              stringsAsFactors = FALSE)
pop<-subset(pop,pop$CLAVE!= "")

births.pop <- merge(births, pop, by.x = "Code2", by.y = "CLAVE")
births.pop <- subset(births.pop, X2007 > 100000)

births.pop$ille <- births.pop$Soltera / births.pop$Total
births.pop$sin <- births.pop$Union.libre / births.pop$Total
births.pop$code <- rownames(births.pop)
juaritos <- births.pop[30,]
births.pop$code <- with(births.pop, reorder(code, ille))
#births.pop <- births.pop[order(-births.pop$ille),]

p <- ggplot(births.pop, aes(code, ille, label = Municipio)) +
    geom_point() +
    geom_text(hjust = 1.2) +
    geom_point(data = juaritos, aes(code, ille), color = "red",
               size= 4) +
    opts(title ="Percentage of births registered by single mothers (2007)") +
    ylab("percentage") + xlab("municipalities with more than 100,00 people") +
    scale_x_discrete(breaks = NA) +
    scale_y_continuous(formatter = "percent")
savePlot(p, "charts/single-mom.png")



births.pop$code <- with(births.pop, reorder(code, sin))
p <- ggplot(births.pop, aes(code, sin, label = Municipio)) +
    geom_point() +
    geom_text(hjust = 1.2) +
    geom_point(data = juaritos, aes(code, sin), color = "red",
               size= 4) +
    opts(title ="Percentage o births registered by mothers living in a domestic partnership(2007)") +
    ylab("percentage") + xlab("municipalities with more than 100,00 people") +
    scale_x_discrete(breaks = NA)
savePlot(p, "charts/domestic-p.png")
#births.pop <- births.pop[order(-births.pop$sin),]
