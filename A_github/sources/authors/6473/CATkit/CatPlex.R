CatPlex <-
function (myData, foldLen=24, Debug=F) {
  #  filter, Ycolumn, time column, alpha in window formula

    #library("signal", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")
    data<-vector(length=length(myData),mode="numeric") 
    #data$time.n=as.numeric(myData[])   #  this is done only to prime the vector for later use ( so it doesn't give an error)
    dataN<-length(myData)
    
    str(myData)
    #write.matrix(dfData, file = "~/R_WD/testDataFile.txt", sep = ",")
   
    avgIndexes <-factor(rep(1:foldLen,  length = dataN))
      Plex <- by(myData, avgIndexes, mean)

    #--opar = par()
    opar = par()
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
           widths=c(1), heights=c(1,1))
    par(mar=c(2,4,1,1), oma=c(1,0,0,0))  
    plot(myData,ylab="data", type="l",bty='n')
    pplex<-t(Plex[])      #Plex is a list:  Plex[[1]] gives first col;  t rotates it so a col is all means.
    plot(c(1:foldLen)*6,pplex[1:foldLen], ylab=paste("folded over",foldLen), type="l",bty='n')
    
    data$Observation<-myData[]
    data$ObservationHr<-avgIndexes
    aov.ex1 = aov(Observation~ObservationHr,data=data)  #do the analysis of variance
    print("SUMMARY")
    print(summary(aov.ex1))                                #display Type I ANOVA table                              
    print(coefficients(aov.ex1))
    print(model.tables(aov.ex1,"means",se=TRUE),digits=6)       #report the means and the number of subjects/cell
    print("TUKEY")
    # Tukey Honestly Significant Differences
    TukeyHSD(aov.ex1) 
    print("DROP1")
    drop1(aov.ex1,~.,test="F")                 # type III SS and F Tests
    boxplot(Observation~ObservationHr,data=data)        #graphical summary
    layout(matrix(c(1,2,3,4),2,2)) # optional layout 
    plot(aov.ex1) # diagnostic plots
    
    browser()
    #remove read only list components 
    opar$cin<-NULL
    opar$cra<-NULL
    opar$csi<-NULL
    opar$cxy<-NULL
    opar$din<-NULL
    opar$page<-NULL
    par(opar)
    return(list(data=pplex))
    

  end
}
