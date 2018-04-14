dummyfier <- function(theDataFrame, theColumnName, alternateIndex=NULL, usefullRank=T) {
        # Function wrapper for Caret's dummyVars to quickly break out every factor for a given data.frame column
        # var ~ . or var or var + var
        # Handles one column formula either with duplicates or unique and returns altered data.frame with new columns minus original
        # Defaults to row index but can be assigned an alternative index in situations where row isn't unique
        # Automatically builds formula, appends new columns to data.frame and removes old one
        # example use:
        # dummyfier(df, 'FLAG') -- 2 vars
        # dummyfier(df, 'CHIEFCOMPLAINT') -- multi vars
        
        if (theColumnName !='.' & (!theColumnName %in% names(theDataFrame)))
                return ("Couldn't find variable in column names")
         
        if (theColumnName == '.') {  
                # get all column names that are factors to transform only those
                originalFactorVariables <- names(theDataFrame)[sapply(theDataFrame, is.factor)]
                # break out everything that is a factor in the data.frame
                dummies <- dummyVars(paste("~ ", originalFactorVariables), data = theDataFrame, fullRank=usefullRank)
                tempdata <- data.frame(predict(dummies, newdata = theDataFrame))
                # join new data to data.frame
                theDataFrame <- cbind(theDataFrame, tempdata)
                # drop original column
                columnNamesToKeep <- names(theDataFrame)[c(!names(theDataFrame) %in% originalFactorVariables)]
               theDataFrame <- theDataFrame[c(columnNamesToKeep)] 
        }
        else if (length(levels(theDataFrame[,theColumnName])) == 2) {
                # no need to dummify column with two unique values
                print(paste("Levels found:",length(levels(theDataFrame[,theColumnName]))))
                theDataFrame[,theColumnName] <- ifelse(theDataFrame[,theColumnName] == levels(theDataFrame[,theColumnName])[1], 1, 0)
        }
        else if (length(levels(theDataFrame[,theColumnName])) > 2) {
                print(paste("Levels found:",length(levels(theDataFrame[,theColumnName]))))
                theDataFrame[,theColumnName] <- droplevels(theDataFrame[,theColumnName])
                if (!is.null(alternateIndex)) {
                        dummies <- dummyVars(paste("~", theColumnName, "+", alternateIndex), data = theDataFrame, fullRank=usefullRank)
                }
                else {
                        # break out everything that is a factor in the data.frame
                        dummies <- dummyVars(paste(" ~",theColumnName), data = theDataFrame, fullRank=usefullRank)
                }
                tempdata <- data.frame(predict(dummies, newdata = theDataFrame))
                # join new data to data.frame
                theDataFrame <- cbind(theDataFrame, tempdata)
                # drop original column
                var.out<- setdiff(names(theDataFrame),c(theColumnName))
                theDataFrame <- theDataFrame[var.out]
        }
        else {
                print("No suitable levels")
        }
        return (theDataFrame)
} 
