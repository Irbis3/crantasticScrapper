# punts <- readRDS("punts.Rds")
# punts[punts$punter %in% names(table(punts$punter)[table(punts$punter) <= 10]), "punter"] <- NA 
# punts <- punts[!is.na(punts$punter),]  
# punts <- punts[,-c(1:4, 6:12)]
# saveRDS(punts, "trimmedPunts.Rds")
punts <- readRDS("trimmedPunts.Rds")

# 
# #summarize stats for each punter
# summPunts <- aggregate(punts$retYards, punts["punter"], quantile, probs=c(0.25, 0.5, 0.75), na.rm=TRUE)
# plotStats <- apply(summPunts, 1, function(x){
#   thisPunter <- punts[punts$punter == x[1] & !is.na(punts$punter),]
#     
#   #find the minimum value greater than (Q1 - (1.5 * IQR)) for the lower wisker
#   min <- min(min(thisPunter$retYards[
#     thisPunter$retYards >= (as.numeric(x[2]) - (as.numeric(x[4])-as.numeric(x[2])) * 1.5)]
#     , na.rm=TRUE), as.numeric(x[2]))
#   
#   max <- max(max(thisPunter$retYards[
#     thisPunter$retYards <= (as.numeric(x[4]) + (as.numeric(x[4])-as.numeric(x[2])) * 1.5)]
#     , na.rm=TRUE), as.numeric(x[4]))
#   
#   q1 <- as.numeric(x[2])
#   q3 <- as.numeric(x[4])
#   med <- as.numeric(x[3])
#   
#   x <- c(min, q1, med, q3, max)  
#   toReturn <- list(box=x)
#   names(toReturn$box) <- c("Min", "Q1", "Med", "Q3", "Max")
#   toReturn$outliers <- c(thisPunter$retYards[thisPunter$retYards < min & !is.na(thisPunter$retYards)],
#                          thisPunter$retYards[thisPunter$retYards > max & !is.na(thisPunter$retYards)])
#   toReturn
# })
# 
# #to data.frame
# boxStats <- data.frame(Punter=rep(summPunts[,1], each=5), yd=as.numeric(sapply(plotStats, "[[", "box")))
# pointStats <- data.frame(Punter=rep(summPunts[,1], times=sapply((sapply(plotStats, "[[", "outliers")), length)), 
#                          yd=as.numeric(unlist(sapply(plotStats, "[[", "outliers"))))
# ggplot(boxStats, aes(Punter, yd)) + geom_boxplot() + 
#   ggtitle("Yards on Returned Punts by Punter") + 
#   geom_point(data=pointStats, mapping=aes(Punter, yd)) 
# 


shinyServer(function(input, output) {
  library(ggplot2)
  
  punterData <- reactive({
    if (input$punter == "All"){
      returned2 <- punts
      #trim out the punters with <= 5 punts to get rid of some noise
      returned2[returned2$punter %in% names(table(returned2$punter)[table(returned2$punter) <= 10]), "punter"] <- NA 
      returned2 <- returned2[!is.na(returned2$punter),]  
      
      return(returned2)
    } else{
      thisPunter <- punts[punts$punter == input$punter & !is.na(punts$punter),] 
      
      return(thisPunter)
    }
  })
  
  teams <- reactive({
    if (input$punter != "All"){      
      teams <- unique(as.character(punterData()$off))
      return(teams)
    }
  })
  
  output$main_plot <- renderPlot({
    if (input$punter == "All"){
      returned2 <- punts
      #trim out the punters with <= 10 punts to get rid of some noise
      returned2[returned2$punter %in% names(table(returned2$punter)[table(returned2$punter) <= 10]), "punter"] <- NA 
      returned2 <- returned2[!is.na(returned2$punter),]  
      print(ggplot(returned2, aes(punter, retYards)) + 
              geom_boxplot() + 
              theme(axis.text.x = element_text(vjust=0.5,angle=90)) + 
              ggtitle("Yards on Returned Punts by Punter") + 
              xlab("Punter") + 
              ylab("Yards"))      
    
    }else{
      thisPunter <- punterData()
        
      thisTeam <- punts[punts$off %in% teams() & !is.na(punts$off),] 
      thisTeam$group <- as.character(thisTeam$off)
      
      thisTeam[thisTeam$punter == input$punter & !is.na(thisTeam$punter),]$group <- input$punter
      
      print(ggplot(thisTeam, aes(group, retYards)) + geom_boxplot() + xlab("Group") + ylab("Yards on Returned Punts") + ggtitle("Comparison of Returned Punts Yards by Group"))
    }
  })
  
  output$resultsBar <- renderPlot({
    print(ggplot(punterData(), aes(as.factor(result))) + geom_bar(binwidth=1) + xlab("Result of Punts") + theme(axis.text.x = element_text(vjust=0.5,angle=90)))
  })
  
  output$years <- renderPlot({
    print(ggplot(punterData(), aes(as.factor(year))) + geom_bar(binwidth=1) + xlab("Year"))
  })
  
  output$data <- renderTable({
    teams <- teams()
    years <- unique(as.character(punterData()$year))
    
    data.frame(Key=c("Teams", 
                     "Years", 
                     "Total Punts"), 
               Value=c(paste(teams, collapse=","), 
                       paste(min(years), max(years), sep="-"),
                       nrow(punterData())))
    
  })
})