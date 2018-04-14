
#Since most deaths by legal intervations are recorded as homicides with a few exceptions
#lets consider all of them as homicides to be consistent
hom.juarez$PRESUNTOtxt <- as.character(hom.juarez$PRESUNTOtxt)
hom.juarez$PRESUNTOtxt[hom.juarez$PRESUNTOtxt == "Legal intervention, operations of war, military operations, and terrorism"] <- "Homicide"

##Chart with the injury mechanism by injury intent
hom.juarez$CAUSE <- as.character(hom.juarez$CAUSE)
hom.juarez$SEXOtxt <- as.character(hom.juarez$SEXOtxt)

hom.cause <- ddply(hom.juarez, .(CAUSE, PRESUNTOtxt, ANIODEF), nrow)
hom.cause <- ddply(hom.cause, .(PRESUNTOtxt, ANIODEF), transform,
                   per = V1 / sum(V1))

test_that(ddply(hom.cause, .(PRESUNTOtxt, ANIODEF), summarise, sum(per)), equals(1))

ggplot(hom.cause,
       aes(per, CAUSE, group= PRESUNTOtxt, color = PRESUNTOtxt,
           label = PRESUNTOtxt)) +
             geom_point(size = 3, alpha = .8, show_guide = FALSE) +
             geom_text(hjust = -0.1, show_guide = FALSE) +
             scale_x_continuous(labels = percent, limits = c(0,1)) +
             ylab("injury mechanism") + xlab("percentage of injury intent deaths due to injury mechanism") +
             scale_color_hue("type of\ndeath") +
             opts(title = "External cause of injury deaths in Ciudad Juárez, by injury intent")


hom.cause$PRESUNTOtxt <- factor(hom.cause$PRESUNTOtxt, levels = c("Homicide", "Unknown",
                                                                  "Accident", "Suicide"))
hom.cause <- merge(hom.cause,
      expand.grid(lapply(hom.cause[,1:3], function(x) levels(as.factor(x)))),
      all.y = TRUE)
hom.cause$per[is.na(hom.cause$per)] <- 0
hom.cause$CAUSE <- reorder(as.factor(hom.cause$CAUSE), hom.cause$per)
ggplot(hom.cause, aes(CAUSE, per,  group= as.numeric(ANIODEF), color = as.numeric(ANIODEF))) +
  geom_line() +
  facet_wrap(~PRESUNTOtxt)+
  xlab("injury mechanism") + ylab("percentage of deaths due to injury mechanism") +
  opts(title = "External cause of injury deaths in Ciudad Juárez, by injury intent",
       axis.text.x  = theme_text(angle=90, hjust=1))+
         scale_y_continuous(labels = percent, limits = c(0,.88))  +
         scale_colour_gradient("year of\ndeath", low="gray80", high="black")
ggsave("graphs/percent.png", dpi = 100, width = 9, height = 6)

#################################
#Clean Data
##PRESUNTOtxt == Injury intent of the death (accident, homicide, unknown, etc)
##EDADVALOR == Age in years
##SEXOtxt == Sex
##LUGLEStxt == Place where the injury took place
##CAUSE == External Cause of Injury Mortality Matrix for ICD-10 from the CDC

#Important: convert to factors or rf fails!
##Add NAs
#age of 998 means NA
hom.juarez$EDADVALOR <- ifelse(hom.juarez$EDADVALOR == 998, NA, hom.juarez$EDADVALOR)
#Place where the body was found
hom.juarez$LUGLEStxt <- ifelse(hom.juarez$LUGLEStxt == "Unknown", NA, hom.juarez$LUGLEStxt)

#Married, single, free union, etc
hom.juarez$EDOCIVILtxt <- ifelse(hom.juarez$EDOCIVILtxt == "Unknown", NA, hom.juarez$EDOCIVILtxt)
#Injury Mechanism from the CDC: Firearm, Poison, Transportation
hom.juarez$CAUSE <- ifelse(hom.juarez$CAUSE == "Unspecified", NA, hom.juarez$CAUSE)
#Presumed Homicides, Accident, Suicide, Unknown Intent
hom.juarez$PRESUNTOtxt <- ifelse(hom.juarez$PRESUNTOtxt == "Unknown", NA, hom.juarez$PRESUNTOtxt)
#Sex
hom.juarez$SEXOtxt <- ifelse(hom.juarez$SEXOtxt == "0", NA, hom.juarez$SEXOtxt)


#Age looks better when sqrt transformed
hom.juarez$EDADVALOR <- sqrt(hom.juarez$EDADVALOR)


#As factors
hom.juarez$CAUSE <- as.factor(hom.juarez$CAUSE)
hom.juarez$SEXOtxt <- as.factor(hom.juarez$SEXOtxt)
hom.juarez$PRESUNTOtxt <- as.factor(as.character(hom.juarez$PRESUNTOtxt))
hom.juarez$LUGLEStxt <- as.factor(hom.juarez$LUGLEStxt)
