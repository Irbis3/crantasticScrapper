# Analysis of Executions
ggplot(mexecutions, aes(date, value,
                 group = variable,
                 color = variable)) +
  geom_line() +
  opts(title = "Homicides and Executions in Mexico") +
  ylab("number of executions or homicides")
ggsave("graphs/reforma-vs-milenio.png", dpi = 100,
       height = 5, width = 8)

executions$b <- ifelse(executions$date <= as.Date("2009-05-15"), 0 ,1) 
ggplot(executions, aes(date, diff)) +
  geom_line() +
  opts(title = "Differences in Number of Executions According to Milenio and Reforma") +
  ylab("difference in number of executions")
ggsave("graphs/diff-reforma-vs-milenio.png", dpi = 100,
       height = 5, width = 8)

# Correlation of exections with the homicide rate
cor(executions$Homicides, executions$Reforma,
    use = "complete.obs")
cor(executions$Homicides, executions$Milenio,
    use = "complete.obs")

executions.reforma <- read.csv("data/executions-reforma.csv")
mexe.ref <- melt(executions.reforma, id = "State")
mexe.ref$variable <- gsub("X", "", mexe.ref$variable)

hom06.09 <- ddply(subset(hom, Year %in% 2006:2009),
      .(State, Year),
      function(df) sum(df$Homicides, na.rm = TRUE))
executions.reforma
reforma <- merge(mexe.ref, hom06.09,
                 by.x = c("State", "variable"),
                 by.y = c("State", "Year"))
names(reforma) <- c("State", "Year", "Reforma", "Homicides")
reforma
ggplot(reforma,
       aes(Reforma, Homicides, label = State)) +
  geom_point() +
#  geom_text() +
  geom_smooth(method = lm) +
  facet_wrap(~ Year)



#tot.06.07 <- cast(subset(tot.06.07, Year %in% 2006:2007), State~Year)
#tot.06.07$diff <- tot.06.07$'2007' -  tot.06.07$'2006'

#exe.ref <- read.csv("data/2006vs2007.csv")
#exe.dif <- merge(exe.ref, tot.06.07, by = c("State")
#exe.dif <- subset(exe.dif, State != "Mexico" &
#       State != "Tlaxcala")
#exe.dif$State <- reorder(exe.dif$State, -exe.dif$diff.x)
#ggplot(exe.dif, aes(State, diff.y)) +
#  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
#  coord_flip() +
#  geom_bar(stat = "identity", data = exe.dif,
#           aes(State, diff.x), fill = "red", alpha = .5)


