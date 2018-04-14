
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

head(titanicDF, 3)

summary(titanicDF)

df_survived_1 <- subset(titanicDF, Survived==1)
df_survived_0 <- subset(titanicDF, Survived==0)
summary(df_survived_1$Sex.female)

summary(df_survived_0$Sex.female)

Sex.Female_0 <- (summary(df_survived_0$Sex.female))
Sex.Female_0 <- c(Sex.Female_0[1:6])
Sex.Female_1 <- (summary(df_survived_1$Sex.female))
Sex.Female_1 <- c(Sex.Female_1[1:6])
stats <- data.frame('ind'=c(1:6), 
                    'Sex.Female_1'=Sex.Female_1,
                    'Sex.Female_0'=Sex.Female_0)

head(stats,6)

# plot Sex.Female predictor
require(ggplot2)
p <- ggplot(data=stats, aes(ind)) +
        geom_line(aes(y = Sex.Female_1, colour = "Sex.Female_1")) +
        geom_line(aes(y = Sex.Female_0, colour = "Sex.Female_0")) +
        scale_x_discrete(breaks = 1:6,
        labels=c("min","1q","median","mean","3q","max"))
p

# calculate predictor's spread value
spread <- ((Sex.Female_1[[1]] - Sex.Female_0[[1]]) +
                (Sex.Female_1[[2]] - Sex.Female_0[[2]]) +
                (Sex.Female_1[[3]] - Sex.Female_0[[3]]) +
                (Sex.Female_1[[4]] - Sex.Female_0[[4]]) +
                (Sex.Female_1[[5]] - Sex.Female_0[[5]]) +
                (Sex.Female_1[[6]] - Sex.Female_0[[6]]))
print(spread)
 
# do the same thing for predictor Title.Nothing
Title.Nothing_0 <- (summary(df_survived_0$Title.Nothing))
Title.Nothing_0 <- c(Title.Nothing_0[1:6])
Title.Nothing_1 <- (summary(df_survived_1$Title.Nothing))
Title.Nothing_1 <- c(Title.Nothing_1[1:6])
stats <- data.frame('ind'=c(1:6), 'stats1'=Title.Nothing_1,'stats0'=Title.Nothing_0)
spread <- ((Title.Nothing_1[[1]] - Title.Nothing_0[[1]]) +
        (Title.Nothing_1[[2]] - Title.Nothing_0[[2]]) +
        (Title.Nothing_1[[3]] - Title.Nothing_0[[3]]) +
        (Title.Nothing_1[[4]] - Title.Nothing_0[[4]]) +
        (Title.Nothing_1[[5]] - Title.Nothing_0[[5]]) +
        (Title.Nothing_1[[6]] - Title.Nothing_0[[6]]))
print(spread)
                                             
p <- ggplot(data=stats, aes(ind)) +
geom_line(aes(y = stats1, colour = "stats1")) +
geom_line(aes(y = stats0, colour = "stats0")) +
scale_x_discrete(breaks = 1:6,
labels=c("min","1q","median","mean","3q","max")) +
ylab('Title.Nothing') + xlab(paste('Spread:',spread))
p

# generalize this in a handy function
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
                # returns a ggplot 
                print(paste('Scaled spread:',spread))
                p <- ggplot(data=stats, aes(ind)) +
                geom_line(aes(y = stats1, colour = "stats1")) +
                geom_line(aes(y = stats0, colour = "stats0")) +
                scale_x_discrete(breaks = 1:6,
                labels=c("min","1q","median","mean","3q","max")) +
                ylab(predictorName) + xlab(paste('Spread:',spread))
                return (p)
        } else {
                # only returns spread final value
                return (spread)
        }
}
                                             
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

for (predictorName in predictorNames)
        print(paste(predictorName,':',GetSummaryPlot(scaled_titanicDF_0,
        scaled_titanicDF_1, predictorName, plotit=FALSE)))
                                             
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
     #http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
     # Multiple plot function
     #
     # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
     # - cols:   Number of columns in layout
     # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
     #
     # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
     # then plot 1 will go in the upper left, 2 will go in the upper right, and
     # 3 will go all the way across the bottom.
     #
     require(grid)
     
     # Make a list from the ... arguments and plotlist
     plots <- c(list(...), plotlist)
     
     numPlots = length(plots)
     
     # If layout is NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
             # Make the panel
             # ncol: Number of columns of plots
             # nrow: Number of rows needed, calculated from # of cols
             layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                              ncol = cols, nrow = ceiling(numPlots/cols))
     }
     
     if (numPlots==1) {
             print(plots[[1]])
             
     } else {
             # Set up the page
             grid.newpage()
             pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
             
             # Make each plot, in the correct location
             for (i in 1:numPlots) {
                     # Get the i,j matrix positions of the regions that contain this subplot
                     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                     
                     print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                     layout.pos.col = matchidx$col))
             }
     }
}
                                            
p1 <- GetSummaryPlot(scaled_titanicDF_0, scaled_titanicDF_1, 'Sex.female', plotit=TRUE)
p2 <- GetSummaryPlot(scaled_titanicDF_0, scaled_titanicDF_1, 'Title.Mrs', plotit=TRUE)
p3 <- GetSummaryPlot(scaled_titanicDF_0, scaled_titanicDF_1, 'Sex.male', plotit=TRUE)
p4 <- GetSummaryPlot(scaled_titanicDF_0, scaled_titanicDF_1, 'Title.Mr', plotit=TRUE)
multiplot(p1,p2,p3,p4,cols=2)

summaryImportance <- c()
variableName <- c()
for (predictorName in predictorNames) {
     summaryImportance <-  c(summaryImportance, GetSummaryPlot(scaled_titanicDF_0, scaled_titanicDF_1, predictorName, plotit=FALSE))
     variableName <- c(variableName, predictorName)
}
results <- data.frame('VariableName'=variableName, 'Weight'=summaryImportance)

# display variable importance on a +/- scale 
results <- results[order(results$Weight),]
results <- results[(results$Weight != 0),]

par(mar=c(5,15,4,2)) # increase y-axis margin. 
xx <- barplot(results$Weight, width = 0.85, 
           main = paste("Variable Importance - Titanic"), horiz = T, 
           xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
           col = ifelse((results$Weight > 0), 'blue', 'red')) 
axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  
                                             