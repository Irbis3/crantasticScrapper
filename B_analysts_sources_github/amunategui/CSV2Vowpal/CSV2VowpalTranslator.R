WriteCSVToVW <- function(objDF, vwfilename, labelName=NULL, tagName=NULL, weightName = NULL) {
        # takes a csv file and turns it into vopal format
        # [Label] [Importance [Tag]]|Namespace Features |Namespace Features ... |Namespace Features
        # writeCSVToVW(objTrainRaw, 'vwformat2.txt', 'target', weightName='weights')
       
        # code based on https://github.com/zygmuntz/r-libsvm-format-read-write/blob/master/f_write.libsvm.r
       
        lineHolders <- c()
       
        if (!is.null(weightName)) {
                weights <- objDF[,weightName]
        }
 
        predictors <- names(objDF)[!names(objDF) %in% c(labelName, tagName, weightName)]
        x <- objDF[,predictors]
        if (!is.null(labelName))
                y <- objDF[,labelName]
        else
                y <- rep(0,nrow(objDF))
        
        
        print(paste('Rows to process:', nrow( x )))
       
        # open an output file
        # f = file( filename, 'w' )
       
        # loop over examples
        for ( i in 1:nrow( x )) {
               
                # find indexes of nonzero values
                indexes = which( as.logical( x[i,] ))
                indexes <- names(x[indexes])
               
                # nonzero values
                values = x[i, indexes]
               
                # concatenate to the target format ( "1:6 3:77 6:8" )  
                iv_pairs = paste( indexes, values, sep = ":", collapse = " " )
               
                # add label in the front and newline at the end
               
                if (!is.null(weightName)) {
                        output_line = paste0(y[i], " ", weights[i], " |f ", iv_pairs, "\n", sep = "" )
                } else {
                        output_line = paste0(y[i], " |f ", iv_pairs, "\n", sep = "" )
                }
               
                # write to file
                #cat( output_line, file = f )
                lineHolders <- c(lineHolders, output_line)
               
                # print progress
                if ( i %% 5000 == 0 ) {
                        print( i )
                }
        }
        print('Writing to file...')
        write.table(lineHolders, file=vwfilename, sep=",", row.names=FALSE, col.names=FALSE, quote=F, eol="")
}