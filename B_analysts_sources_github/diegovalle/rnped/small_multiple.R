rnpe_c$name <- str_c(rnpe_c$abbrev, ", ",  rnpe_c$fuerocomun_desapmunicipio)
rnpe_f$name <- str_c(rnpe_f$abbrev, ", ",  rnpe_f$fuerofederal_ultimampio)

dis_c <- rnpe_c %>%
  group_by(date = as.Date(as.yearmon(date)), name) %>%
  summarise(count = n()) %>%
  filter(date >= "2008-01-01")
dis_f <- rnpe_f %>%
  group_by(date = as.Date(as.yearmon(date)), name) %>%
  summarise(count = n()) %>%
  filter(date >= "2008-01-01")
dis <- rbind(dis_c, dis_f)
dis %<>% group_by(date, name) %>%
  summarise(count = sum(count)) 



full <- expand.grid(unique(dis$name),
                    seq(as.Date("2008-01-01"), max(dis$date), by = "month"))
names(full) <- c("name", "date")
dis <- merge(dis, full, all.y = TRUE)
dis[is.na(dis)] <- 0

anom <- findAnomalies(unique(dis$name), dis)
anom$name[which(anom$name == "GRO, IGUALA DE LA INDEPENDENCIA")] <- "GRO, IGUALA"
anom$name <- reorder(anom$name, -anom$count, min) 
ggplot(anom, aes(date, count)) +
  geom_line() +
  facet_wrap(~name) +
  ggtitle("MUNICIPALITIES WITH ANOMALIES IN DISAPPEARANCES\nFROM AUGUST 2014 TO JANUARY 2015") +
  sm_theme()
ggsave("graphs/sm.png", width = 14.50, height = 9.6, dpi = 100)
