
hom.tj <- subset(hom, MA == "Tijuana")
#hom.tj <- subset(hom, ENTOCU == 02 & MUNOCU %in% c(004))
llcharts <- generateCharts(hom.tj, kmaxy, "Tijuana (Zona Metropolitana)")
llcharts$weekly <- llcharts$weekly +
  geom_vline(aes(xintercept = as.Date("2007-01-03")), alpha = .7,
               linetype = 1) +
  geom_text(aes(as.Date("2007-01-03"), 30,
                label = "Operativo Conjunto\nTijuana"),
             hjust = 1.03, vjust = 0) +
  geom_vline(aes(xintercept = as.Date("2008-10-26")), alpha = .7,
               linetype = 1) +
  geom_text(aes(as.Date("2008-10-26"), 65,
                label = "Captura de\n\"El Doctor\""),
             hjust = 1.03, vjust = 0) +
  scale_x_date(minor = "4 months") 
llcharts$weekly <- labelChart(llcharts$weekly, "¿Tregua?",
                              75, "2008-11-29", -0.03)
#llcharts$weekly <- labelChart(llcharts$weekly, "¿Termina la Tregua?",
 #                             75, "2009-12-07", -0.03)
saveCharts(llcharts, "tijuana-ma")


#chartRegion(hom, c(02), c(004, 003, 005), kmaxy,
 #           "Tijuana (MA)")  
#write.csv(ddply(subset(hom, ENTOCU == 08 & MUNOCU %in% c(37)), .(ANIODEF, MESDEF), nrow), file = "temp.csv")
daily1 <- ddply(hom.tj, .(ANIODEF, MESDEF, DIADEF), nrow)
daily1 <- subset(daily1, ANIODEF %in% c(2008, 2009))
daily1$date <- with(daily1, as.Date(str_c(ANIODEF, MESDEF, DIADEF,
                                        sep = "-")))
#daily1$group2 <- c(rep(1:4, each = 138), 4)
print(ggplot(daily1, aes(date, V1)) +
  scale_x_date(format = "%b", minor = "month") +
  facet_wrap(~ANIODEF, nrow = 2, scales = "free_x") +
  opts(title = "Homicidios Diarios en la Zona Metropolitana de Tijuana (2008-2009)") +
  ylab("número de homicidios") +
  xlab("fecha") +
  geom_vline(data = data.frame(x = as.Date("2008-10-26"), ANIODEF = 2008) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2008-10-26"), ANIODEF = 2008) ,
                        aes(x, 21,
                            label = "Captura de\n\"El Doctor\""),
            hjust = 1.03, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2008-12-01"), ANIODEF = 2008) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2008-12-01"), ANIODEF = 2008) ,
                        aes(x, 15,
                            label = "¿Tregua?"),
            hjust = -.05, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2009-12-07"), ANIODEF = 2009) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2009-12-07"), ANIODEF = 2009) ,
                        aes(x, 20,
                            label = "¿Termina la Tregua?"),
            hjust = 1.03, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2008-04-26"), ANIODEF = 2008) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2008-04-26"), ANIODEF = 2008) ,
                        aes(x, 20,
                            label = "Enfrentamiento entre sicarios\nde \"El Teo\" y \"El Ingeniero\""),
            hjust = 1.03, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2008-09-29"), ANIODEF = 2008) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2008-09-29"), ANIODEF = 2008) ,
                        aes(x, 17,
                            label = "16 ejecuciones"),
            hjust = 1.03, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2009-03-02"), ANIODEF = 2009) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2009-03-02"), ANIODEF = 2009) ,
                        aes(x, 20,
                            label = "Captura de \"El Profe\""),
            hjust = 1.03, vjust = 0, size = 3.5)  +
  geom_vline(data = data.frame(x = as.Date("2008-09-16"), ANIODEF = 2008) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2008-09-16"), ANIODEF = 2008) ,
                        aes(x, 10,
                            label = "Motines en la carcel"),
            hjust = 1.03, vjust = 0, size = 3.5) +
  #geom_vline(data = data.frame(x = as.Date("2009-01-15"), ANIODEF = 2009) ,
   #                     aes(xintercept = x), alpha = .7,
    #           linetype = 1) +
  #geom_text(data = data.frame(x = as.Date("2009-01-15"), ANIODEF = 2009) ,
   #                     aes(x, 10,
    #                        label = "Truce?"),
     #       hjust = -.15, vjust = 0, size = 3.5)  +
  geom_vline(data = data.frame(x = as.Date("2008-12-10"), ANIODEF = 2008) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2008-12-10"), ANIODEF = 2008) ,
                        aes(x, 10,
                            label = "Leyzaola"),
            hjust = -.05, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2009-06-15"), ANIODEF = 2009) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2009-06-15"), ANIODEF = 2009) ,
                        aes(x, 20,
                            label = "Captura de \"La Perra\""),
            hjust = -.05, vjust = 0, size = 3.5) +
  geom_vline(data = data.frame(x = as.Date("2009-07-02"), ANIODEF = 2009) ,
                        aes(xintercept = x), alpha = .7,
               linetype = 1) +
  geom_text(data = data.frame(x = as.Date("2009-07-02"), ANIODEF = 2009) ,
                        aes(x, 15,
                            label = "Captura de \"El Marro\""),
            hjust = -.05, vjust = 0, size = 3.5) +
   geom_line())
ggsave("graphs/tijuana-daily-select.png", height = 5, width = 13, dpi = 100)
ggsave("pdfs/tijuana-daily-select.pdf", height = 5, width = 13)




hom.mexic <- subset(hom, MA == "Mexicali")
llcharts <- generateCharts(hom.mexic, kmaxy, "Mexicali")
saveCharts(llcharts, "mexicali")

drh.tj.month <- ddply(subset(drh.tj, !is.na(date)), .(date),
                      function(df) sum(df$Total))
drh.tj.month$date <- as.Date(drh.tj.month$date)
hom.tj.month <- formatMonthly(formatDaily(hom.tj))
hom.drh.tj <- merge(drh.tj.month, hom.tj.month,
                    by = "date",
                    all.x = TRUE)
names(hom.drh.tj) <- c("Date", "ACO", "M", "Todos")
hom.drh.tj$M <- NULL
mhom.drh.tj <- melt(hom.drh.tj, id = "Date")

mhom.drh.tj$variable <- factor(mhom.drh.tj$variable, levels = c("Todos", "ACO"))
Cairo("graphs/aco-vs-hom.png", height = 500, width = 800)
print(ggplot(mhom.drh.tj, aes(Date, value, group = variable, linetype = variable)) +
  geom_line(size = 1, alpha = .8) +
  scale_linetype("tipo de\nhomicidio", breaks = c("Todos", "ACO")) +
  scale_x_date(minor = "4 months") +
  opts(title = "Homicidios Totales y Asociados con el Crimen Organizado (ACO)\nen la Zona Metropolitana de Tijuana") +
  xlab("fecha") +
  ylab("número de homicidios") +
  ylim(0, max(mhom.drh.tj$value, na.rm = TRUE)))
dev.off()
ggsave("pdfs/aco-vs-hom.pdf", w = 8, h = 5)


accidents.tj <- formatMonthly(formatDaily(accidents.tj))
suicides.tj <- formatMonthly(formatDaily(suicides.tj))
acc.sui.tj <- merge(accidents.tj, suicides.tj, by = "date")
names(acc.sui.tj) <- c("date", "month", "accidents", "m", "suicides")
macc.sui.tj <- melt(acc.sui.tj, id = c("date", "month", "m"))
macc.sui.tj <- subset(macc.sui.tj, date < as.Date("2009-11-30"))

Cairo("graphs/accidents-suicides.png", height = 500, width = 800)
print(ggplot(macc.sui.tj, aes(date, value, group = variable, color = variable)) +
  geom_line(size = 1) +  
  opts(title = "Muertes por Accidente o Suicidio en la Zona Metropolitana de Tijuana") +
  xlab("fecha") +
  ylab("número de muertes") +
  scale_x_date(minor = "4 months") +
  scale_colour_brewer("tipo de\nmuerte",
                      palette="Dark2",
                      breaks = c("accidents", "suicides"),
                      labels = c("accidente", "suicidio")) +
  geom_smooth(method = loess, color = "black"))
dev.off()
ggsave("pdfs/accidents-suicides.pdf", w = 8, h = 5)


drh.tj.month
drh.tj.month$year <- format(drh.tj.month$date, "%Y")
ddply(drh.tj.month, .(year), function(df) sum(df$V1))
df <- ddply(hom.tj, c("ANIODEF", "MESDEF", "CERTIFtxt"), nrow)
df <- ddply(subset(hom.tj, NECROPCIAtxt == "No Especificado" &
                               MESDEF != 0 &
                               ANIODEF %in% 2007:2009), c("ANIODEF", "MESDEF"), nrow)
df$Date <- as.Date(str_c(df$ANIODEF, df$MESDEF, "15", sep = "-"))
hom.drh.tj$diff <- with(hom.drh.tj, Todos - ACO)
no.autopsy <- merge(df,hom.drh.tj, all.y = TRUE)
no.autopsy[is.na(no.autopsy)] <- 0
no.autopsy <- subset(no.autopsy, Date < as.Date("2010-01-01"))

Cairo("graphs/tijuana-diff-necropsia.png", height = 500, width = 800)
print(ggplot(no.autopsy, aes(Date, V1, color = "Sin Necropcia")) +
  geom_line(size = 1) +
  geom_line(size = 1,aes(Date, diff, color = "Diferencia entre\ntodos los homicidios\ny solamente los\nHomicidios ACO")) +
  scale_x_date(minor = "4 months") +
  scale_colour_manual("tipo de\nhomicidio", values = c("#00bfc4", "#f8766d")) +
  xlab("fecha") +
  ylab("cantidad") +
  opts(title = "La diferencia entre el total de homicidios y los homicidios asociados al crimen organizado (ACO)\nincrementó al mismo tiempo que los homicidios donde no se especificó si se contaba con necropsia") +
  annotate("text", x = as.Date("2009-07-15"),
           y = 11, label = "homicidios sin\nnecropsia especificada", color = "#00bfc4") +
  annotate("text", x = as.Date("2009-01-15"),
           y = 95, label = "diferencia entre todos los homicidios\ny los homicidios ACO", color = "#f8766d") +
  opts(legend.position = "none", plot.title = theme_text(size = 12, hjust=0)))
  #xlim(c(as.Date("2007-01-01"), as.Date("2010-12-31")))
dev.off()
ggsave("pdfs/tijuana-diff-necropsia.pdf", w = 8, h = 5)

no.autopsy$diff <- no.autopsy$Todos - no.autopsy$V1
ggplot(no.autopsy, aes(Date, ACO, color = "Sin Necropcia")) +
  geom_line(size = 1) +
  geom_line(size = 1,aes(Date, diff, color = "Diferencia entre\ntodos los homicidios\ny solamente los\nHomicidios ACO")) +
  scale_x_date(minor = "4 months") +
  scale_colour_manual("tipo de\nhomicidio", values = c("#00bfc4", "#f8766d")) +
  xlab("fecha") +
  ylab("cantidad") +
  opts(title = "La diferencia entre el total de homicidios y los homicidios asociados al crimen organizado (ACO)\nincrementó al mismo tiempo que los homicidios donde no se especificó si se contaba con necropsia") +
  annotate("text", x = as.Date("2009-07-15"),
           y = 11, label = "homicidios sin\nnecropsia especificada", color = "#00bfc4") +
  annotate("text", x = as.Date("2009-01-15"),
           y = 95, label = "diferencia entre todos los homicidios\ny los homicidios ACO", color = "#f8766d") +
  opts(legend.position = "none", plot.title = theme_text(size = 12, hjust=0)) 
  #xlim(c(as.Date("2007-01-01"), as.Date("2010-12-31")))

no.autopsy$ANIODEF <- format(no.autopsy$Date, "%Y")
per <- ddply(no.autopsy, .(ANIODEF),
             function(df) c(sum(df$ACO), sum(df$Todos), sum(df$V1))) 
per$V1 / per$V2
428/588
