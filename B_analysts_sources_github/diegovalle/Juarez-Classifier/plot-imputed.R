##The Random Forest came out on top, we'll use it to classify the intent of the deaths of unknown intent
fit.unknown <- predict(rfFit, hom.unknown[,c(x)])
print(table(fit.unknown))
hom.unknown$PRESUNTOtxtRF <- fit.unknown

hom.juarez.pred <- rbind.fill(hom.juarez[!is.na(hom.juarez$PRESUNTOtxt),], hom.unknown)
hom.juarez.pred$PRESUNTOtxtRF <- with(hom.juarez.pred,
                                      ifelse(is.na(PRESUNTOtxt),
                                             as.character(PRESUNTOtxtRF),
                                             as.character(PRESUNTOtxt)))
original <- ddply(hom.juarez.pred, .(ANIODEF, month(date), PRESUNTOtxt), nrow)

imputed <- ddply(hom.juarez.pred, .(ANIODEF, month(date), PRESUNTOtxtRF), nrow)

original <- subset(na.omit(original), PRESUNTOtxt == "Homicide")
imputed <- subset(na.omit(imputed), PRESUNTOtxtRF == "Homicide")
juarez.murders <- merge(original, imputed,
                        by = c("ANIODEF", "month(date)"))

names(juarez.murders) <- c("ANIODEF", "month",
                           "PRESUNTOtxt","HomicidesOrig",
                           "PRESUNTOtxtRF", "HomicidesSVM")

juarez.murders$date <- with(juarez.murders,
                            as.Date(str_c(ANIODEF, month, "01", sep = "-")))


ggplot(juarez.murders, aes(date, HomicidesOrig, linetype = "Original")) +
  geom_line() +
  geom_line(aes(date, HomicidesSVM, linetype = "SVM")) +
  scale_x_date() +
  ylab("number of homicides") +
  scale_linetype("type of death",
                 labels = c("Homicides + deaths of unknown intent classified as homicides",
                            "Homicides"),
                 breaks = c("SVM", "Original")) +
                   xlab("month") +
                   opts(legend.position = "bottom",
                        title = "Homicides and Homicides + Deaths of Unknown Intent in Ciudad Juárez")
ggsave("graphs/juarez-correct.png", dpi = 100, width = 9.6, height = 5)

tbl <- merge(ddply(hom.juarez.pred, .(ANIODEF, PRESUNTOtxt), nrow),
             ddply(hom.juarez.pred, .(ANIODEF, PRESUNTOtxtRF), nrow),
             by.x = c("ANIODEF", "PRESUNTOtxt"),
             by.y = c("ANIODEF", "PRESUNTOtxtRF"), all.x = TRUE)
tbl$V1.y[is.na(tbl$V1.y)] <- 0
tbl <- subset(tbl, PRESUNTOtxt == "Homicide")
sum(tbl$V1.y)
names(tbl) <- c("Year", "Injury Intent", "Original Deaths", "Imputed Deaths")
print(xtable(tbl, digits = 0), include.colnames = TRUE)

xtable(data.frame(Total = 'Total 2008-2010',
                  "Original Deaths" = sum(tbl$V1.x[5:7]),
                  "Imputed Deaths" = sum(tbl$V1.y[5:7])))
