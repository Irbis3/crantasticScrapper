
# main --------------------------------------------------------------------

GetSummaryPlot <- function(objdfscaled0, objdfscaled1, predictorName, plotit=TRUE) {
        require(ggplot2)
        
        stats0 <- (summary(objdfscaled0[,predictorName]))
        stats0 <- c(stats0[1:6])
        stats1 <- (summary(objdfscaled1[,predictorName]))
        stats1 <- c(stats1[1:6])
        stats <- data.frame('ind'=c(1:6), 'stats1'=stats1,'stats0'=stats0)
        
        spread <- ((stats1[[1]] - stats0[[1]]) +
                           (stats1[[2]] - stats0[[2]]) +
                           (stats1[[3]] - stats0[[3]]) +
                           (stats1[[4]] - stats0[[4]]) +
                           (stats1[[5]] - stats0[[5]]) +
                           (stats1[[6]] - stats0[[6]]))
        
        if (plotit) {
                print(paste('Scaled spread:',spread))
                p <- ggplot(data=stats, aes(ind)) +
                        geom_line(aes(y = stats1, colour = "stats1")) +
                        geom_line(aes(y = stats0, colour = "stats0")) +
                        scale_x_discrete(breaks = 1:6,
                                         labels=c("min","1q","median","mean","3q","max")) +
                        ylab(predictorName) + xlab(paste('Spread:',spread))
                return (p)
        } else {
                return (spread)
        }
}

# using dataset from the UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/)
titanicDF <- read.csv('http://math.ucdenver.edu/RTutorial/titanic.txt',sep='\t')
# creating new title feature
titanicDF$Title <- ifelse(grepl('Mr ',titanicDF$Name),'Mr',ifelse(grepl('Mrs ',titanicDF$Name),'Mrs',ifelse(grepl('Miss',titanicDF$Name),'Miss','Nothing')))
titanicDF$Title <- as.factor(titanicDF$Title)

# impute age to remove NAs
titanicDF$Age[is.na(titanicDF$Age)] <- median(titanicDF$Age, na.rm=T)

# reorder data set so target is last column
titanicDF <- titanicDF[c('PClass', 'Age',    'Sex',   'Title', 'Survived')]

# binarize all factors
require(caret)
titanicDummy <- dummyVars("~.",data=titanicDF, fullRank=F)
titanicDF <- as.data.frame(predict(titanicDummy,titanicDF))


outcomeName <- 'Survived'
predictorNames <- names(titanicDF)[!names(titanicDF) %in% outcomeName]
# Temporarily remove the outcome variable before scaling the data set
outcomeValue <- titanicDF$Survived
# scale returns a matrix so we need to tranform it back to a data frame
scaled_titanicDF <- as.data.frame(scale(titanicDF))
scaled_titanicDF$Survived <- outcomeValue
# split your data sets
scaled_titanicDF_0 <- scaled_titanicDF[scaled_titanicDF[,outcomeName]==0,]
scaled_titanicDF_1 <- scaled_titanicDF[scaled_titanicDF[,outcomeName]==1,]


summaryImportance <- c()
variableName <- c()
for (predictorName in predictorNames) {
        summaryImportance <-  c(summaryImportance, GetSummaryPlot(scaled_titanicDF_0, scaled_titanicDF_1, predictorName, plotit=FALSE))
        variableName <- c(variableName, predictorName)
}
results_global <- data.frame('VariableName'=variableName, 'Weight'=summaryImportance)
 

