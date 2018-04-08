Import <-
function (x, tsCol=1, tsEndCol=0, valCols, sumCols, verbose=0, maxGap=15, tz='GMT', rowDurationSeconds=120) {
    # x is data.frame, 
    # tsCol is column with row starting datetime,
    # tsEndCol is optional column with the row end datetime.
    # valCols is vector with columns containing value data,
    # sumCols is vector with columns containing data that is summable,
    # verbose is integer with 0=no extra info in infoMsg, values 1 and 2 increase the information content
    # maxGap is the max number of contiguous rows that can be missing in the data
    # tz is time zone
    # rowDurationSeconds is the time in seconds in a data row, This is only used if the tsEndCol end time is not given
    
    errorExit = FALSE
    print(tsCol)
    print(x[1,])
    print(length(x[,tsCol]))
    print(x[length(x[,tsCol]),tsCol])   ####
    print ("-")
    print(x[1,tsCol])
    print ("+")
    print(rowDurationSeconds)
    naCnt<-0

    if (anyDuplicated(c(valCols,sumCols))){stop("sumCol and valCol parameter vectors must be mutually exclusive.")}
    if (all(is.na(c(valCols,sumCols)))){stop("sumCol and valCol parameter vectors are both empty.  At least one column must be specified.")}

    xTS<-x[!is.na(x[,tsCol]),]     #  omit any line where the time column is NA
    x<-xTS                              # put back into x

    tsDuration<- as.numeric(difftime(x[length(x[,tsCol]),tsCol], x[1,tsCol], units="secs")) # in seconds to start of last row
    
    timeUnits='seconds' # todo change based on data
    
    if(timeUnits=='seconds') {
      inputRowTimeUnits = rowDurationSeconds
      tsDuration = tsDuration + rowDurationSeconds
    }
    if(timeUnits=='minutes') {
      inputRowTimeUnits = rowDurationSeconds/60
      tsDuration = tsDuration/60 + inputRowTimeUnits
    }
    if(timeUnits=='hours') {
      inputRowTimeUnits = rowDurationSeconds/60/60
      tsDuration = tsDuration/60/60 + inputRowTimeUnits
    }
    if(timeUnits=='days') {
      inputRowTimeUnits = rowDurationSeconds/60/60/24
      tsDuration = tsDuration/60/60/24 + inputRowTimeUnits
    }
    
    # create timeseries in timeunits  
    myTs <- ts(1:tsDuration, start=0, frequency=1)
    
    dataCol <- list()
    for (s in 1:ncol(x)){
      dataCol[[s]] <- vector("numeric", length(myTs))
      dataCol[[s]][1:length(myTs)] <- NA
    }

    GapMsg <- ""
    MaxGapCnt <- 0
    totalGaps <- 0
    lastTs <- -inputRowTimeUnits

    len <- length(x[,tsCol])
    if (len>1000){
      sampleLen<-1000
    } else {sampleLen<-len}
    DT<-mean(diff(x[1:sampleLen,tsCol],lag=1))
    units(DT)<-"secs"
    if (inputRowTimeUnits*1.07<DT || DT<inputRowTimeUnits*.93)     #  error if not -range < DT < +range
      {print(paste("Double check parameter sizePts=",inputRowTimeUnits," -- it may not match actual distance between time points in data file ~",DT))}

    if (any(is.na(x[,c(sumCols,valCols)]))){
      # rows have missing data - impute one column at a time (slower)
      if (any(is.na(x[1,c(sumCols,valCols)]))) {
        # missing data in first row
        if (verbose == 2) {
          print("Imputing missing data in first row ")
        }
        # Use closest 3 values starting with not an na.  If some are NA, they still count.  Divide by valid values to get average.   (If 1 of 3 is NA, divide by 2).
        # ignore any missing rows
        # Then use the average to impute back to the first column
        for (col in c(sumCols,valCols)) {
          if (is.na(x[1,col])) {
            naCnt = 1
            for (row in 2:(length(x[,tsCol])-1)) {
              if (is.na(x[row,col])) {
                naCnt = naCnt+1
              } else {
                avg = x[row,col]
                foundCnt = 1
                if (length(x[,tsCol]) -2 < row) {
                  print(paste("Data has too few usable rows to impute an first row average for column ",col))
                  return(list(errorExit=TRUE,maxGapCnt=naCnt, percentMissing=NA))
                }
                if (!is.na(x[row+1,col])){
                  avg = avg + x[row+1,col]
                  foundCnt = foundCnt+1
                }
                if (!is.na(x[row+2,col])){
                  avg = avg + x[row+2,col]
                  foundCnt = foundCnt+1
                }
                avg <- avg / foundCnt
                for (r in 1:(row-1)) {
                  x[r,col] <- avg
                }
                print(paste("Imputed missing column data from first row to row ",row," using average value of row ",row," and the next two rows column data."))
                break
              }
            }
            if (naCnt > maxGap) {
              #browser()
              print(paste("Data has too large of a gap to be usable. Number of contiguous missing rows: ", naCnt))
              return(list(errorExit=TRUE,maxGapCnt=naCnt, percentMissing=NA))
            }
          }
        }
      }      
      
      # NOTE: the following code is structured repitiously, that is the same code is repeated to minimize testing conditions
      len <- length(x[,tsCol])
      if (any(is.na(x[len,c(sumCols,valCols)]))) {
        # missing data in last row
        if (verbose == 2) {
          print("Imputing missing data in last row ")
        }
        # Use closest 3 values starting with not an na.  If some are NA, they still count.  Divide by valid values to get average.   (If 1 of 3 is NA, divide by 2).
        # ignore any missing rows
        # Then use the average to impute back to the last column
        for (col in c(sumCols,valCols)) {
          if (is.na(x[len,col])) {
            naCnt = 1
            for (row in length(x[,tsCol])-1:1) {
              if (is.na(x[row,col])) {
                naCnt = naCnt+1
              } else {
                avg = x[row,col]
                foundCnt = 1
                if (row<3) {
                  print(paste("Data has too few usable rows to impute a last row average for colunn ",col))
                  return(list(errorExit=TRUE,maxGapCnt=naCnt, percentMissing=NA))
                }
                if (!is.na(x[row-1,col])){
                  avg = avg + x[row-1,col]
                  foundCnt = foundCnt+1
                }
                if (!is.na(x[row-2,col])){
                  avg = avg + x[row-2,col]
                  foundCnt = foundCnt+1
                }
                avg <- avg / foundCnt
                for (r in length(x[,tsCol]):(row+1)) {
                  x[r,col] <- avg
                }
                print(paste("Imputed missing column data from last row to row ",row," using average value of row ",row," and the previous two rows coulmn data."))
                break
              }
            }
            if (naCnt > maxGap) {
              print(paste("Data has too large of a gap to be usable. Number of contiguous missing rows: ", naCnt))
              return(list(errorExit=TRUE,maxGapCnt=naCnt, percentMissing=NA))
            }
          }
        }
      }

      MaxGapcnt<-naCnt     #  add in any missing rows in the beginning

      # build the timeseries column  :  need to omit any line where the time is NA
#       for (s in 1:len){
#         tsInd = na.omit(as.integer(as.numeric(x[s,tsCol])-as.numeric(x[1,tsCol])))
#         dataCol[[tsCol]][c((tsInd+1):(tsInd+inputRowTimeUnits))] <- c((tsInd+1):(tsInd+inputRowTimeUnits))
#       }

      tsInd = na.omit(as.integer(as.numeric(x[,tsCol])-as.numeric(x[1,tsCol])))
      dataCol[[tsCol]][c((tsInd[1]+1):(tsInd[len]+inputRowTimeUnits))] <- c((tsInd[1]+1):(tsInd[len]+inputRowTimeUnits))
      
      # impute each relevant data column individually
      for (col in c(valCols, sumCols)){
        if (verbose == 2) {
          print(paste("Imputing missing data in column ", col))
        }
        #browser()
        lastTs <- -inputRowTimeUnits
        prevDataRow <- 0 # row with not a NA value
        for (s in 1:len){ # each input row
          if ((verbose == 2) && (s %% 100 == 0))
          { print(paste("Processing row ",s))}
          tsInd = as.integer(as.numeric(x[s,tsCol])-as.numeric(x[1,tsCol]))
          if (!is.na(x[s,col])) {
            if (col %in% valCols){
              dataCol[[col]][c((tsInd+1):(tsInd+inputRowTimeUnits))] <- as.numeric(x[s,col])
            } else {
              secInc = as.numeric(x[s,col])/inputRowTimeUnits
              dataCol[[col]][c((tsInd+1):(tsInd+inputRowTimeUnits))] <- secInc
            }
            
            if (tsInd != lastTs+inputRowTimeUnits){
              # do missing row accounting
              if (verbose == 2)
              { print(paste("Gap before row ",s))}
              CurGap <- (tsInd - lastTs - inputRowTimeUnits) / inputRowTimeUnits # in row count
              totalGaps <- totalGaps + CurGap         # we want to count the length of each gap, not just the number of gaps
              if (CurGap > MaxGapCnt){
                MaxGapCnt <- ceiling(CurGap)
              }
              # do interpolation of missing data rows
              missingValCnt <- tsInd - lastTs - inputRowTimeUnits
              missingRowCnt <- ceiling(missingValCnt / inputRowTimeUnits)
              if (col %in% valCols){
                baseVal <- as.numeric(x[(prevDataRow),col])
                stepInc <- (as.numeric(x[s,col]) - baseVal) / (missingRowCnt+1)
                for (r in c(1:missingRowCnt)) {
                  #for (val in c(1:inputRowTimeUnits)){
                    dataCol[[col]][(lastTs+inputRowTimeUnits*r+1):(lastTs+inputRowTimeUnits*r+inputRowTimeUnits)] <- baseVal + stepInc*r
                  #}
                }
              } else {       #   SUM col
                baseVal <- as.numeric(x[(prevDataRow),col])
                stepInc <- (as.numeric(x[s,col]) - baseVal) / (missingRowCnt+1)
                for (r in c(1:missingRowCnt)) {
                  incStepInc <- (baseVal + stepInc*r) / inputRowTimeUnits    #  make this a vector;  r(1:missingRowCnt); sweep?
                  #for (val in c(1:inputRowTimeUnits)){
                    dataCol[[col]][(lastTs+inputRowTimeUnits*r+1):(lastTs+inputRowTimeUnits*r+inputRowTimeUnits)] <- incStepInc
                  #}
                }
              }
            }
            lastTs <- tsInd
            prevDataRow <- s
          }
        }
      }    
      
    } else { # no missing data in rows given
      # but there may be missing rows
      if (verbose == 2) {
        print("No missing data in rows given")
      }
      len <- length(x[,tsCol])

      for (s in 1:len){
        if ((verbose == 2) && (s %% 100 == 0))
        { print(paste("Processing row ",s))}
        tsInd = as.integer(as.numeric(x[s,tsCol])-as.numeric(x[1,tsCol]))
        curRow = as.numeric(x[s,])

        dataCol[[tsCol]][c((tsInd+1):(tsInd+inputRowTimeUnits))] <- c((tsInd+1):(tsInd+inputRowTimeUnits))
        for (ind in valCols){
          dataCol[[ind]][c((tsInd+1):(tsInd+inputRowTimeUnits))] <- as.numeric(curRow[ind])   #  fix
        }
        if (length(sumCols)>0){
          curRowData <- as.vector(curRow[sumCols])
          secInc = curRowData/inputRowTimeUnits
          #aSecInc <- array(data=0.0, dim=c(inputRowTimeUnits,length(sumCols)), dimnames = NULL)
          for (ind in 1:length(sumCols)){
            sc <- sumCols[ind]
            dataCol[[sc]][c((tsInd+1):(tsInd+inputRowTimeUnits))] <- as.numeric(secInc[ind])   # fix
          }
        }
        if (tsInd != lastTs+inputRowTimeUnits){       #  is a row missing here?
          # do missing row accounting
          #browser()   # stop here to see when first missing data is
          if (verbose == 2)
          { print(paste("Gap before row ",s))}
          CurGap <- (tsInd - lastTs - inputRowTimeUnits) / inputRowTimeUnits # in row count
          totalGaps <- totalGaps + CurGap         # we want to count the length of each gap, not just the number of gaps
          if (CurGap > MaxGapCnt){
            MaxGapCnt <- ceiling(CurGap)
          }
          #browser()
          # do interpolation of missing data rows
          missingValCnt <- tsInd - lastTs - inputRowTimeUnits
          missingRowCnt <- ceiling(missingValCnt / inputRowTimeUnits)
          for (ind in c(valCols)){
            baseVal <- as.numeric(x[(s-1),ind])                                 #  instead of x, should be using imputed values??
            #baseVal <- unlist(dataCol[[ind]][lastTs+inputRowTimeUnits])     #   should be lastTs?
            #baseVal <- unlist(dataCol[[ind]][lastTs+1])  
            stepInc <- (as.numeric(x[s,ind]) - baseVal) / (missingRowCnt+1)
            #stepInc <- (unlist(dataCol[[ind]][(lastTs+inputRowTimeUnits*(missingRowCnt+1)+1)]) - baseVal) / (missingRowCnt+1)     #  tsInd-base/missing?
            #stepInc <- (unlist(dataCol[[ind]][tsInd]) - baseVal) / (missingRowCnt+1) 
            for (r in c(1:missingRowCnt)) {
              dataCol[[ind]][(lastTs+inputRowTimeUnits*r+1):(lastTs+inputRowTimeUnits*r+inputRowTimeUnits)]<-baseVal + stepInc*r
              #for (val in c(1:inputRowTimeUnits)){
              #  dataCol[[ind]][lastTs+inputRowTimeUnits*r+val] <- baseVal + stepInc*r
              #}
            }
          }
          for (ind in c(sumCols)){
            baseVal <- as.numeric(x[(s-1),ind])
            #baseVal <- unlist(dataCol[[ind]][lastTs+inputRowTimeUnits] )
            #baseVal <- unlist(dataCol[[ind]][lastTs+1]) 
            stepInc <- (as.numeric(x[s,ind]) - baseVal) / (missingRowCnt+1)
            #stepInc <- (unlist(dataCol[[ind]][(lastTs+inputRowTimeUnits*(missingRowCnt+1)+1)]) - baseVal) / (missingRowCnt+1)
            #stepInc <- (unlist(dataCol[[ind]][tsInd]) - baseVal) / (missingRowCnt+1) 
            for (r in c(1:missingRowCnt)) {
              incStepInc <- (baseVal + stepInc*r) / inputRowTimeUnits
              dataCol[[ind]][(lastTs+inputRowTimeUnits*r+1):(lastTs+inputRowTimeUnits*r+inputRowTimeUnits)]<-incStepInc
              #for (val in c(1:inputRowTimeUnits)){
              #  dataCol[[ind]][lastTs+inputRowTimeUnits*r+val] <- incStepInc
              #}
            }
          }
        }
        lastTs <- tsInd
      }
    }

    # common code
    #percentMissing <- as.numeric(totalGaps / as.numeric(length(x[,tsCol]))) / 100.0   #  tsDuration
    percentMissing <- (totalGaps /((tsDuration/inputRowTimeUnits)+totalGaps)) * 100.0   #  missing data/full data set  %

    if (MaxGapCnt >= maxGap){
      print(paste("Data has too large of a gap to be usable. Number of contiguous missing rows: ", MaxGapCnt))
      return(list(errorExit=TRUE,maxGapCnt=MaxGapCnt, percentMissing=percentMissing,totalGaps=totalGaps))
    } else {
      if (MaxGapCnt == 0){
        print("Data is contiguous. No imputation performed.")
      }
    }
    
    if (errorExit == FALSE)
    {
      # rebin data to that given in seconds parm
      if (verbose == 2){
        print("Rebinning data.")}
      
      newRowCnt <- as.integer(length(myTs)/inputRowTimeUnits)
      rebinData <- matrix(data = NA, nrow=newRowCnt, ncol=ncol(x), byrow=FALSE, dimnames=NULL)
      #Indexes <- factor(rep(1:(newRowCnt*inputRowTimeUnits), each=inputRowTimeUnits, length=newRowCnt*inputRowTimeUnits))   # removed when testing Shade5
      Indexes <- factor(rep(1:(newRowCnt), each=inputRowTimeUnits, length=newRowCnt*inputRowTimeUnits))
      for (s in valCols){
        rebinData[,s] <- by(unlist(dataCol[[s]])[c(1:(newRowCnt*inputRowTimeUnits))], Indexes, mean)  # cannot remove loop
      }
      for (s in sumCols){
        rebinData[,s] <- by(unlist(dataCol[[s]])[c(1:(newRowCnt*inputRowTimeUnits))], Indexes, sum)   #  cannot remove loop
      }
      dfOut <- as.data.frame(rebinData)
      
      names(dfOut) <- names(x)
      myDT <- c(1:nrow(rebinData))
      for (ind in 1:nrow(rebinData)){
        myDT[c(ind)] <- as.POSIXct(x[1,tsCol]+(ind-1)*inputRowTimeUnits,tz=tz)   #  cannot remove loop
      }
    }
    
    # apply timezone hack to get correct time
    origin <- as.POSIXct('1970-01-01 00:00:00',tz=tz) 
    offset <- as.numeric(origin)
    dfOut[,tsCol] <- as.POSIXct(myDT-offset, origin=origin,tz=tz)
    return(list(df=dfOut, maxGapCnt=MaxGapCnt, percentMissing=percentMissing, totalGaps=totalGaps, errorExit=FALSE))
    
  }
