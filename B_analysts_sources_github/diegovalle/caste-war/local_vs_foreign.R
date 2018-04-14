qroo <- read.csv(file.path("data", "QROO FINAL.pdf.csv.xz"), header = FALSE)
names(qroo) <- c("num", "paterno", "materno", "nombre", "clave", "edo", "mun", "seccion")
qroo <- filter(qroo, num != "NO.")

yuc.prd <- read.csv(file.path("listado_nominal/PRD/csv/NEWFILENAME.csv"), header = FALSE)
names(yuc.prd) <- c("num", "paterno", "materno", "nombre", "clave", "edo", "mun", "seccion")
createGDF(yuc.prd, file.path("gephi", "yuc-prd.gdf"))
view(yuc.prd %>%
  group_by(paterno,materno) %>%
  summarise(weight = n())%>%
  arrange(desc(weight)))

qroo$birth <- parse_date_time(str_sub(qroo$clave, 7, 12), "%y%m%d") - years(100)
qroo$state.of.birth <- as.numeric(str_sub(qroo$clave, 13, 14))
qroo$sex <- str_sub(qroo$clave, 15, 15)

foreign.born <- qroo %>%
  filter(state.of.birth != 23) %>%
  group_by(paterno) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

nrow(filter(qroo,  state.of.birth != 23 & state.of.birth != 4 & state.of.birth != 31))
nrow(qroo)


qroo$yucateco <- ifelse(qroo$state.of.birth != 23 & 
                          qroo$state.of.birth != 4 & 
                          qroo$state.of.birth != 31, 
                        0, 1)

df <- rbind(data_frame(name = qroo$paterno, yuc = qroo$yucateco),
            data_frame(name = qroo$materno, yuc = qroo$yucateco))
per.qroo <- df %>%
  na.omit %>%
  group_by(name) %>%
  summarise(sumyuc = sum(yuc, na.omit = TRUE), 
            sumnonyuc = length(yuc) - sum(yuc),
            per = sumyuc / sum(sumyuc + sumnonyuc)) %>%
  arrange(desc(per))

g.qroo <- read.csv(file.path("gephi", "pan-pri-prd-qroo-modularity.csv"))
groups <- inner_join(per.qroo, g.qroo, by = c("name" = "Id")) %>%
  arrange(desc(per)) %>%
  group_by(Modularity.Class) %>%
  summarise(mean.per = mean(per))



ggplot(groups, aes(Modularity.Class, mean.per, fill = as.factor(Modularity.Class))) +
  geom_bar(stat = "identity") +
  scale_fill_manual("Modularity", values = c("#53cf53", "#0009e6", "#ad0034")) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  ggtitle("Mean percentage of PRD surnames born in the Yucatan Peninsula, by Community") +
  xlab("modularity class") +
  ylab("Mean percentage of surnames born in Quintana Roo, Yucatan, or Campeche") +
  theme_bw()

ggsave(file.path("charts", "mean.svg"), dpi = 100, width = 9.60, height = 7)

