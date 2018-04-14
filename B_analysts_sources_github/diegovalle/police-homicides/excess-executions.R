stackedHomicides <- function(name, month, exe) {
  exe$non.narco <- exe$Homicides - exe[[name]]
  mexe2 <- melt(exe[ ,c("date",name, "non.narco")], id="date")
  mexe2$variable <- factor(mexe2$variable, levels = rev(levels(mexe2$variable)))
  mexe2 <- na.omit(mexe2)
  ggplot(mexe2, aes(date, value,
                    group = variable,
                    fill = variable)) +
    theme_bw() +
    geom_area() +
    annotate("text", x =  as.numeric(as.Date("2009-09-15")),
                       y = 250, label = "Non-Drug Related Homicides") +
    annotate("text", x =  as.numeric(as.Date("2009-09-15")),
             y = 930,
             label = str_c("Drug Related Homicides (",
               name, ")")) +
    scale_fill_manual(values = c("#2077b9", "darkred")) +
    opts(legend.position = "none",
         title = str_c("Drug Related and Non-Drug Related Homicides in Mexico (Dec 2006 - ", month, " 2010)")) +
    ylab("number of homicides") 
}

stackedHomicides("Milenio", "Aug", executions)
ggsave("graphs/milenio.png", dpi = 100, height = 5, width = 8)

stackedHomicides("CISEN", "Jul", executions)
ggsave("graphs/cisen.png", dpi = 100, height = 5, width = 8)
