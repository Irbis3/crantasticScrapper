
#Plot the missing values
cols <- c("PRESUNTOtxt","EDADVALOR","CAUSE","SEXOtxt", "LUGLEStxt", "ANIODEF")
missm <- as.data.frame(is.na(hom.juarez[,cols]))
#missm <- as.data.frame(apply(missm, 2, function(x) sort(x)))
missm$row <- 1:nrow(missm)
missm <- melt(missm, id= "row")

missm$variable <- reorder(missm$variable, -missm$value, sum)

p <- ggplot(missm, aes(row, variable, fill = value)) +
  geom_tile() +
  xlab("observation number") +
  ylab("variables") +
  scale_fill_manual("", labels = c("complete", "missing"),
                      values = c("#00bfc4", "#f8766d")) +
  scale_y_discrete(labels = c("Place of\nInjury", "Injury\nMechanism", "Injury\nIntent", 
                              "Age", "Sex", "Year")) +
  opts(title = "Missing data in the Juárez mortality database")
ggsave(plot = p,"graphs/missing.png", dpi= 100, w = 9, h = 5)