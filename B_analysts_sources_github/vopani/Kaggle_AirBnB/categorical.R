### functions for processing categorical data

## function for one-hot encoding categorical features
encode_categories <- function(df1, df2, onehot=c("none"), label=c("none"))
{
	# appending dataframes
  panel <- rbind(df1, df2)
  
  # extracting categorical columns
	col_classes <- lapply(panel, class)
	categorical_columns <- names(col_classes[col_classes %in% c("character","factor")])
  
  if (length(categorical_columns) > 0)
  {
    if (!onehot[1] %in% c("all","none"))
    {
      # loading library
      library(dummies)
      
      # converting to dummy variables
      panel <- dummy.data.frame(panel, names=onehot, sep="_")
      
      for (i in onehot)
      {
        cat("Onehot encoded column:", i, "\n")
      }
    }
    if (!label[1] %in% c("all","none"))
    {
      for (i in which(colnames(panel) %in% label))
      {
        # converting to integer variable
        panel[[i]] <- as.integer(as.factor(panel[[i]]))
        
        cat("Label encoded column:", colnames(panel)[i], "\n")
      }
    }
    if (onehot[1] == "all")
    {
      # loading library
      library(dummies)
      
      # converting to dummy variables
      panel <- dummy.data.frame(panel, names=categorical_columns, sep="_")
      
      for (i in categorical_columns)
      {
        cat("Onehot encoded column:", i, "\n")
      }
    }
    if (label[1] == "all")
    {
      for (i in which(colnames(panel) %in% categorical_columns))
      {
        # converting to integer variable
        panel[[i]] <- as.integer(as.factor(panel[[i]]))
        
        cat("Label encoded column:", colnames(panel)[i], "\n")
      }
    }
  }else
  {
    cat("No categorical columns found\n")
  }
  
  # converting data back
  library(data.table)
  
  df1 <- data.table(panel[1:nrow(df1),])
  df2 <- data.table(panel[(nrow(df1)+1):nrow(panel),])
  
  cat("\n")
  
  return(list("train"=df1, "test"=df2))
}

