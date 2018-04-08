CATCosinor <-
function (TimeCol=1,Y=2, Components=1, window="noTaper", RefDateTime=NA,  timeFormat="%Y%m%d%H%M", RangeDateTime=list(Start=NA, End=NA), Units="hours", dt=0, Progressive=list(Interval=0, Increment=0), Period=list(Set=0,Start=0,Increment=1,End=0),header=FALSE, Skip=0, Colors="BW",Graphics="pdf",Output=list(Txt=FALSE,Dat=TRUE, Doc=TRUE,Graphs=FALSE),yLabel="", Console=FALSE,Debug=FALSE,IDcol="fileName", fileName=fileName,functionName="") {    #GraphSet is another parameter used to override proper graphics
  # yLabel=expression(paste("Body Temp (",degree~C,")", sep="")), 
  # Future:  NAs are interpolated;  0s are modified to a very close to 0 value as some functions will not accept 0 data values.
  # Any column selected to be read (Y=column) will be scanned for missing data, and that entire row will be removed.
  #          If you specify two or more columns Y=c(1:3), where ever data is missing in any one column, the entire row will be removed
  #          If this is not acceptable, the program must be run with a single column specified.  Then only rows where data is missing for that column will be removed
  # Header:  T/F to indicate if the file has a header  (column titles for data)
  # Skip:  How many lines to skip at the top of the file (before the header)
  # Console:  If T, all data is sent to R console.  Otherwise, sent to file type indicated in Graphics parameter
  #  Notes on coding for time:
  #         %H represents 0-23 hrs, so have to have numeric time in Excel sheet, and numeric time rep option
  #         Dates are represented as the number of days since 1970-01-01
  #         strptime reads in characters and produces POSIX formatted date/time;  strptime accepts the format of date being read -- output is always POSIX
  #         use format wrapped around POSIX to output a date in a different format than POSIX
  #         POSIXct: This class stores a date in a vector, as the number of seconds passed since January, 1st, 1970 at 1AM
  #         POSIXlt: This class stores a date in a list: (day of the month, day of the week, day of the year, month, year, time zone, hour, minute, second).
  # TimeCol:    Column(s) where time is found.  c(1,2) can be used when date is in column 1 and time is in column 2.  Date and time can be in any two columns, i.e., c(6,3). 
  #             If time is in two columns, the input timeFormat should be specified.  (Internally it is converted to %Y%m%d%HH%MM.)
  #             (Any time format in timeFormat paramater is ignored in this case.)
  # Y:          Column holding data to be analyzed
  # Components:  Default=1.  Indicates if this is a single or multiple component cosinor analysis, where the number of components is specified (>0).  
  # window:      Data will be convolved with a window:  Hanning, Hamming, Bartlett, Blackman.  Default="noTaper"
  # RefDateTime:  Default=NA  Date used as reference, and subtracted from all data dates, to make the number smaller. 
  #             The format is always "%Y%m%d%H%M"
  #             not implemented---if RefDateTime = NA, use the 1st date of the data as the RefDateTime 
  #             if RefDateTime = 0, use midnight of the same day as the data starts 
  #             when using years with leading 0s  (0500 AD) be sure to include the leading 0, and put quotes around the RefDateTime
  # timeFormat:   RefDateTime and the time in the data file should both use the same date/time format.   Can be "numeric" or "%Y%m%d%H%M"
  #  ???        i.e., "%Y%m%d%H%M", or if = "numeric", time column in data file can be simple numbers (0 - 99999...)
  #                           if "numeric", data is sorted by time to be sure they are ordered ascending.  First must be smallest , and last largest.
  #               Time can also be in two columns (indicate in TimeCol);  timeFormat is ignored when time is in two columns -- the format use is %d/%m/%y in the first of the two columns, and %H:%M:%S or %H:%M in the second of the two
  # RangeDateTime:    RangeDateTime and the time in the data file should both use the same date/time format.   
  #      $Start:  Analysis is started at this date/time.  May be before 1st data date.  Default = NA
  #             if Start = NA, use the 1st time in the data set
  #             if Start = 0 use Midnight at the start of the 1st date of the data as the StartDate 
  #        $End:    Analysis ends at this date/time (inclusive -- analysis behaves as if this is the last data point given).  May be after last data date.
  #             if End = NA, use the last time in the data set
  #             if End = 0 use the midnight at the end of the last date of the data as the EndDate  
  #               In this case, be sure the first and last data point in the file are not NA   -- because they are used to create the date/time for Range
  # Units:        Units (hour, year, week or day) for Interval and Increment arguments, as well as Period arguments, and any time variable
  #               ToDo:  modify to use units other than hours -- currently only works for hours
  # Progressive
  #   $Interval : length of the time span being analyzed (in Units)  -- multiple spans calculated
  #               If 0, assumes no progession, Interval is set to the full dataset length, and Increment = full data set  
  #  $Increment: number of Days, Wks or Yrs  (uses same unit as set for Interval) to shift forward for each successive Interval analyses
  #               If 0, assumes no progession, Interval is set to the full dataset length, and Increment = full data set 
  # Period
  #       $End :  [only used if $Set=0] Last (and smallest) period to calculate, in units of Days, Wks or Yrs (as set by Units)  EXCLUSIVE
  #               Defaults to 2*dt or 4;  (1 is too small) 0 is invalid  -- default will be used
  #  $Increment : [only used if $Set=0] Increment to starting period, in units of Days, Wks or Yrs (as set by Units)
  #               Defaults to 1;   0 is invalid  -- default will be used
  #     $Start :  [only used if $Set=0] First (and largest) period to calculate, in units of Days, Wks or Yrs (as set by Units);  (Interval/1)
  #               0 is Default: the full time range of the data file is analyzed [in hours?] (RangeDateTime$End-RangeDateTime$Start)= MyData_length; or if progressive, Interval/1;     
  #               Important:  It is normally best if the user sets $Start to a multiple of the largest period of interest  -- Fourier frequencies calculated will be closest to period of interest this way.
  #                           So if one is interested in a weekly period, use a multiple of 1 week as $Start
  #      $Set :  If Set=0, a series of periods are analyzed (spectrum) according to Period$Start, $Increment, $End (or their default value, if not specified)
  #              If not equal to 0, Overrides Period$Start and $Increment, to completely specify an exact set of periods to analyze (as set by Units).  
  #              Can be in the format of a single number, or an array:  c(1,2,3) or c(8,12:24)
  #              When Components=1, each period specified in the vector will be assessed by cosinor independently.
  #              When parameter Components is >1, Period$Set must have a correspondig number of components, which assessed together in a multiple component cosinor.
  #              When 0, only the maximum period, or the maximum period per Interval, from a spectrum is listed on p1 of the graphics file.
  #              Otherwise, all periods are displayed on the graphic 
  #  Colors       "Heat" renders the graph in yellow to red;  "BW" renders the heatmap in grayscale
  #  Graphics    Possible values:  jpg, pdf, tif, png
  #  Output      $Txt will capture the console output to a .txt file
  #              $Dat will generate a computer readable file of data in a not very human friendly form, but nice for subsequent processing (in a Mean Population Cosinor for example)
  #              $Doc will generate a nicely formatted Word doc
  #              $Graphs will enable a set of graphs plotting Data, Model, MESOR, Amplitude, Acrophase over time--too many when you do a progressive
  #              Calculating the model length for Multiple Component models:
  #               The cycle length of the model is calculated as the Least common multiple of all prescribed cycles for multi-component models, and multiple single-component models
  #                   There will be cases where the model includes two or more periods that are very different in size.  In this case, the model may not print well if the full model is printed
  #               If the longest cycle of the model, or the LCM, is more than 2x the length of the data, plot the data and only that part of the model.
  #                 --in this case, the orthophase and bathyphase cannot be calculated, and will be set to NA
  #                 --if it is slightly different than LCM (1%) then the orthophase and bathyphase will be calculated
  #             For all models:
  #              The graph of the model will overlay the model with the data, if less than 540 data points and 7 cycles of data are present
  #                 In this case, the 0 point on the x-axis will be the RefDateTime.  
  #                 Only the StartTimeDate - EndTimeDate will be graphed
  #                 The graph will be plotted in hours from reference time  
  #                 For each span graphed, a starting point and ending point of the span will be graphed on the model to allow a full cycle to be displayed???
  #              otherwise, [or the number of data to plot is > Nd (> what will display well)] only one cycle of the model will be displayed (with no data)
  #                 The model graph will be labelled in hours.  labelling the graph in degrees is confusing due to the negative degrees.
  #                 For Single Component Models where the data is too long to display, only the models will be plotted: one cycle of the longest model, and corresponding lengths of the others
  #                 ****If the calculated model is REALLY long, and there is too much data to plot, plot neither, with a warning why
  #             --Least Squares:  no model is shown, since so many models are built
  #             --Progressive:  a model is displayed for each span;  Normally, Graphs=FALSE will be set to avoid too many graphs
  #             --Other:  In all other cases, the graph displayed will give the Reference Date and Time
  #  GraphSet:    This is an undocumented parameter.  Defaults are used normally, but can be overriden by this parameter
  #               This will set which line graphs are plotted:  Data, Model, MESOR, Amplitude, Acrophase over time            
  #               $Data plots the (filtered) data;  $Model plots the Cosinor model itself;  
  #               $HeatMap=will do a heatmap if enough Intervals and periods are specified for a progressive analysis,
  # dt           When equidistant data, dt indicates the sampling interval.  If dt =0, no periodogram is done.
  #                  Data is assumed to be equidistant when this is nonzero
  # Debug       Turn on when you want to see more diagnostic output for programming debug purposes
  # IDcol       If IDcol="fileName" then use the fileName as the subject ID;  because sometimes the filename is the subject id
  #             Otherwise, this is the data file column number where the subject ID is found, because sometimes the subject id is in the data
  # fileName    Name of the file containing input data
  # functionName:  Name given the run, to help distinguish it from other runs.  Name is printed in output files.
   
  #library(season)

  #####################################################################################################
  #                                                                                                   #
  #        Set up some initial parameter values, error messages, and key variables, LCM               #
  #                                                                                                   #
  #####################################################################################################
  tz="GMT"
  opar = par()
    # read data;  if no data file name given, prompt the user for the filename
if (length(fileName)==0){
    fileName <- file.choose()
  }
is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

LCM<-Period$Set[1]
# calculate greatest common divisor and least common multiple;  used later to calculate time in one degree
if (all(is.wholenumber(Period$Set))){
  tempPeriods<-Period$Set   # no need for a fraction modifier
  fractionModifier<-1
} else {
  fractionModifier<-100
  tempPeriods<-round(Period$Set, digits=2)*fractionModifier
}
minPeriod<-min(Period$Set)  # get the shortest period for use in calculating plotting

if (Period$Set[1]>0){
  LCM<-max(Period$Set)
  if (Components>1){ 
  "%gcd%" <- function(u, v) {ifelse(u %% v != 0, v %gcd% (u%%v), v)}
  "%lcm%" <- function(u, v) { abs(u*v)/(u %gcd% v)}
  LCM<-1

    for (i in 1:Components){
      LCM<-as.integer(tempPeriods[i]) %lcm% LCM
    }
  LCM<-LCM/fractionModifier
  }
  } else {LCM<-24}      #  default to 24 if spectrum given
print(LCM)

  #####################################################################################################
  #  if not yet set, set defaults;  This should be added as an undocumented parameter to override which Graphs print by default
  # if lss1 ..  else if non-progressive2 ... else if gliding spectrum progressive3 ... else if 1-comp progressive4 or multi-comp progressive5
  #  raw data, model, MESOR, Amplitude, Phase, N (# pts), heatmap
  #####################################################################################################

if (!exists("GraphSet")){    
    if (Progressive$Interval==0 || Progressive$Increment==0){    #  default settings for non-progressives (2)
      if (Period$Set[1]==0){      # non-progressive lss (1)
      GraphSet<-list(Data=T,Model=F,MESOR=F,Amp=T,P=T,Phi=F,NPts=F, HeatMap=F)
      } else GraphSet<-list(Data=T,Model=T,MESOR=F,Amp=F,Phi=F,P=F,NPts=F, HeatMap=F)   #  (2) HeatMap will only be created in certain conditions determine later,
        #  progressives:
    } else {if (Period$Set[1]==0){     #  progressive lss  (3)
                 GraphSet<-list(Data=T,Model=F,MESOR=F,Amp=F,P=F,Phi=F,NPts=F, HeatMap=T)   #  raw data, MESOR, Amplitude, Phase, N (# pts)
                  } else GraphSet<-list(Data=T,Model=T,MESOR=T,Amp=T,P=T,Phi=T,NPts=T, HeatMap=F)    #  regular progressive (4)
            }  # end if period$set==0
  }

  # rename parameters
  StartDate<-RangeDateTime$Start
  EndDate<-RangeDateTime$End
  Interval<-Progressive$Interval
  Increment<-Progressive$Increment
  FreqInc<-Period$Increment
  oneCycle<-sort(Period$Set,decreasing=TRUE)     # can be a vector of one or more
  Ys_end<-length(Y)

  paramMsg<-paste("\n  TimeCol=",TimeCol,",  Y=",Y, ",  header=",header,"\n --  Periods=",Period["Set"],", Units=",Units, ",  Interval=",format(Interval,nsmall=3), ",  Increment=",format(Increment,nsmall=3), "\nPeriod$Start=",format(Period$Start,nsmall=3), ",  FreqInc=",format(FreqInc,nsmall=3), ",  Period$End=",format(Period$End,nsmall=3), "\nRefDateTime=",RefDateTime, ", StartDate=",format(StartDate,nsmall=3),", EndDate=",format(EndDate,nsmall=3),"\nPercent of missing (blank) sample values: unknown","\n",functionName,"\n")

  CosMatrixDim<-Components*2+1         #  validate and error if too large -- should it be Components*2?  no Feb 26
  #if (oneCycle[1]>0){   # same rules apply when Period$Set=0 (but cannot do a multipl-component LSS)
    if (Components>1 && length(oneCycle)!=Components){
      errMsg<-paste("Error:  When parameter Components is >1, Period$Set must have a corresponding number of components.  \nComponents=",Components,"; Period$Set=",Period$Set)
      stop(errMsg)
       # Period$Set will contain multiple possible components to check separately, or as a multi-component cosinor, depending on param Components
       # if Components=1, Period$Set can have any number of components.  If Components>1, length(oneCyce) must equal Components
    }    #else if (Components==1 && length(oneCycle)!=Components){
             #errMsg<-paste("Error:  If Components equals 1, Period$Set must also be only one component.  \nComponents=",Components,"; Period$Set=",Period$Set)
              #stop(errMsg)
             #}
  #} 
  errMsg<-""            #   this error message gets printed at the beginning of the rtf output file
  #  symbols currently in use for Err values, printed in the matrix
  errKey<-c("*","**","+","++","@","*@","@@@","@@","*@@","*@@@")   
# messages that correspond to the errKey symbols
  keyMsgs<- vector(mode="character",length = 10)
  keyMsgs[1]<-paste("*  : Error1:  The matrix cannot be inverted for these points and period. This may happen if there are not enough data points for the period selected. Check sampling interval: there should be at least 2p+1 data points per period.\n")
  keyMsgs[2]<-paste("** : Error2:  The matrix cannot be inverted for these points and period. This may happen if there are not enough data points for the period selected. Check sampling interval: there should be at least 2p+1 data points per period.\n")
  keyMsgs[3]<-"+  : Error:  This model does not allow calculation of PR for the individual components. PR is negative, which is invalid.\n"
  keyMsgs[4]<-"++ : Error:  Coefs cannot be calculated\n"        #  this one is likely not needed
  keyMsgs[5]<-"@  : Error:  Requested analysis violates Nyquist.  Time span should be > 2C, where C=#cycles.  Results may be invalid.\n"
  keyMsgs[6]<-"*@ : Warning:  RSS=0.  Not enough data to calculate S.E.  \n"
  #  if there is a period of 8 hours being assessed, there must be 4 data points in 8 hours.
  keyMsgs[7]<-"@@@: Error:  This interval must be at least as long as the trial period (Period$Set).  No analysis performed when Interval<90% of target period.\n"
  keyMsgs[8]<-"@@: Warning:  The interval must be at least as long as the trial period (Period$Set).  Results may be unreliable.\n"
  keyMsgs[9]<-"*@@: ERROR:  RSS/df2 will be infinite because df2<0, so this case was skipped.  (Number of data points <= number of components*2+1)\n"
  keyMsgs[10]<-"*@@@: Warning:  RSS/df2 will be infinite because df2=0, so s.e.s, F and P calculations are skipped.  (Number of data points <= number of components*2+1)\n"
  
  # extract filename from path for display on output 
  if (.Platform$file.sep=="/"){
    m=regexec("[^/]*$",fileName)            #  ./(.$)    pc=[^\\]*$   mac=[^/]*$
  } else {
    m=regexec("[^\\]*$",fileName)
  }

  fileName1<-regmatches(fileName,m) 
  fileLen<-nchar(fileName1)
  fileName6<-substring(fileName1,1,fileLen-4)

  # send output to file  -- build name of file with path to data f
  BaseTime<-Sys.time()        
  thisTime <- format(BaseTime, "--%d%b%y--%H-%M-%OS")
  fileName2<-paste(fileName,window,thisTime,functionName,"Cos.txt",sep="")
  fileName3<-paste(fileName,window,thisTime,functionName,"Cos.rtf",sep="")
  if (Output$Txt){
    sink(fileName2, append=FALSE, split=TRUE)
  }

  if (!Console){   # if console is set to off, check the type of output for graphics
      if (Graphics == "pdf"){
        fileName4<-paste(fileName,window,thisTime,functionName,"Cos.pdf",sep="")
        pdf(file=fileName4,width=8, height=10)
      } else {if (Graphics == "jpg"){
                fileName4<-paste(fileName,window,thisTime,functionName,"Cos.jpg",sep="")
                jpeg(filename=fileName4,width=8, height=10)
                } else {if (Graphics == "png"){
                          fileName4<-paste(fileName,window,thisTime,functionName,"Cos.png",sep="")
                          png(filename=fileName4,width=8, height=10)
                        } else {if (Graphics == "tif"){
                          fileName4<-paste(fileName,window,thisTime,functionName,"Cos.tif",sep="")
                          tiff(filename=fileName4,width=8, height=10)
                        }
                }
            }
      }
  }

#####################################################################################################
#                                                                                                   #
#        Read data file;  assume tab delimited;  if that fails, read as CSV;  get header;           #
#                omit missing data and count missing data;                                          #
#                                                                                                   #
#####################################################################################################

  missingDataCol<-vector(mode="character")
  MyDataDelimiter <- read.table(fileName, nrows=6, header=header, sep="\t",stringsAsFactors=FALSE,skip=Skip)          
  #  only read a few rows to determine if CSV or tab delimited
  colCount<-length(MyDataDelimiter[5,])     #  accurate for tab delimited
  if (colCount>1){   #  if count >1, this is tab delimited
    delim<-'\t'
  } else {delim<-','}

  headerChar1<-''         # Use NA as a comment indicator -- not sure if this will work*****
  myHeader <- read.table(fileName, header=FALSE, nrows=1, sep=delim,skip=Skip)   # grab the header to extract character (tell it header is false)
  if (delim=='\t'){  #  this is for one special kind of file output  AB
    if (header==TRUE){
      myHeader <- read.table(fileName, header=header, nrows=1, sep="\t",skip=Skip)   # grab the header to extract character (tell it header is false)
      #get the header for names  -- all strings matching header will be removed
      #####headerChar1 <- substr(names(myHeader)[1],1,1)    # gets the 1st character of the header to be used as a comment indicator
    }  #  end if header=true  (nrows=0 makes sure 1st line is read  -- it was being skipped)  but erroring on tab delimted file:  data must be read here
    MyDataReada <- read.table(fileName, header=header, sep="\t",stringsAsFactors=FALSE, skip=Skip,col.names = names(myHeader),blank.lines.skip=TRUE,na.strings=c(" ","--"))     #  comment.char=headerChar1,
# got rid of headerChar1 comment indicator, because it was messing up reading the header...
    
  } else {    # end if delimiter is tab
    colCount<-length(myHeader)     #  recalculate if comma delimited
    tableCols<-c(TimeCol,Y)
    tableColLen<-colCount
        tableColTypes<-rep("character",tableColLen)   # need this for FW  (does it work for all with only 2?)

        tableColTypes[Y]<-"numeric"         #  works even when Y is a vector
        if (all(TimeCol>0) && all(TimeCol<=10)) {
            tableColTypes[TimeCol]<-"character"
        } 

     MyDataReada <- read.table(fileName, header=header, sep=',', stringsAsFactors=FALSE, skip=Skip,colClasses=tableColTypes,blank.lines.skip=TRUE,na.strings=c(" ","--"))
  } # end if delimiter is comma

  if (FALSE){     # for JAMES:  time not numeric, "%Y%m%d%H%M" only, only in 1 column, 1st column
    weirdHour<-as.integer(substr(MyDataReada[,1],9,10))    # get hour as integer  (all)
    weirdHourIdx<-which(weirdHour>23)                    # which hours are weird?  Index into character column
    weirdHourInt<-as.integer(substr(MyDataReada[weirdHourIdx,1],9,10))  #  get weird hours integer values

      Hour<-strsplit(MyDataReada[weirdHourIdx,1],substr(MyDataReada[weirdHourIdx,1],9,10),fixed=TRUE)     #  get only bad hours 
    zz<-1
    View(weirdHourIdx)
    for (z in weirdHourIdx){
      if (weirdHourInt[zz]-24 <10){
        zeroPad<-"0"}
      else {zeroPad<-""
      }
      saveRead<-MyDataReada[z,1]
      if (Hour[[zz]][2]==""){Hour[[zz]][2]<-weirdHourInt[zz]}     #  "" if hour=minute, replace "" with same as weird
      MyDataReada[z,1]<-paste(Hour[[zz]][1],zeroPad,weirdHourInt[zz]-24,Hour[[zz]][2],sep="")
      Day<-strptime(MyDataReada[z,1],"%Y%m%d%H%M",tz=tz)

      MyDataReada[z,1]<-format(Day+(24*3600),"%Y%m%d%H%M")
      if (Debug){
        cat(saveRead,"zz",zz,"weirdHourInt[zz]",weirdHourInt[zz],"--","z",z,MyDataReada[z,1],"\n")
      }
      zz<-zz+1
    }
  }

  if (IDcol=="fileName"){    #set once here as fileName if parameter indicates to use the file
    SubjectID<-fileName6
  } else {
    SubjectID<-MyDataReada[1,IDcol]     #  set for each Loop, within i Loop, assumes this is a subject ID
  }
  #browser()
  #####################################################################################################
  #                                                                                                   #
  #        Time column: convert time format to numeric if needed, or to number of Units of time       #
  #                May be in one or two columns;  2 columns must be appended before conversion        #
  #                                                                                                   #
  #####################################################################################################
  
  timeFormatOrig <- timeFormat
  TimeColLen<-length(TimeCol)
  if (timeFormat=="numeric"){
    BaseDate<-format(Sys.Date(), "%Y%m%d%H%M")   # as.POSIXct(strptime(Sys.Date(), "%Y%m%d%H%M",tz=tz))     # midnight of first day of data
    # apply timezone hack to get correct time
    origin <- as.POSIXct('1970-01-01 00:00:00',tz=tz) 
    offset <- as.numeric(origin)
    newOrder<-MyDataReada[order(MyDataReada[,TimeCol], na.last = TRUE, decreasing = FALSE),]
    MyDataReada<-newOrder
    MyDataReada[,TimeCol]<-format(as.POSIXct(as.numeric(as.POSIXct(strptime(BaseDate, "%Y%m%d%H%M",tz=tz)))+(as.numeric(MyDataReada[,TimeCol])*3600),origin=origin,tz),"%Y%m%d%H%M")
    timeFormat<-"%Y%m%d%H%M"
    timeFormatOrig<-"numeric"
    
  } else {   #convert time from 2 columns  (only one format allowed if time is in 2 columns:   17/10/07 \t 19:00:20)
    if (grepl(".",MyDataReada[1,TimeCol[1]],fixed=TRUE)){    # timeFormat not == 'numeric' but time is numeric
      str(MyDataReada)
      message<-paste("ERROR:  The time format does not match timeFormat setting.  Time is in a numeric/decimal format, but timeFormat is not set to 'numeric'.")
      errMsg<-paste(errMsg,message)
      closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
      stop(message) 
    }

    if (length(TimeCol)==2){
      MyDataReada[,TimeCol[1]] <- as.POSIXct(strptime(x=paste(MyDataReada[,TimeCol[1]],MyDataReada[,TimeCol[2]]), format=timeFormat, tz=tz))      #  "%d/%m/%y %H:%M:%S"

      MyDataReada[,TimeCol[2]] <- format(MyDataReada[,TimeCol[1]],"%Y%m%d%H%M")
      names(MyDataReada)[TimeCol[2]] <- "Datetime"
      TimeCol<-TimeCol[2]
    } else {   #convert time from 1 columns
      if (length(TimeCol)==1){
        MyDataReada[,TimeCol[1]] <- as.POSIXct(strptime(x=MyDataReada[,TimeCol[1]], format=timeFormat, tz=tz))
        MyDataReada[,TimeCol[1]] <- format(MyDataReada[,TimeCol[1]],"%Y%m%d%H%M")
        names(MyDataReada)[TimeCol[1]] <- "Datetime"
      }}}
  if (is.na(MyDataReada[2,TimeCol[1]])){
    str(MyDataReada)
    message<-paste("ERROR:  The time format does not match timeFormat parameter setting, or is NA.  The timeFormat parameter may need to be corrected.")
    errMsg<-paste(errMsg,message)
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    stop(message) 
  }
  
  ########################moved#####################################################################
  #                                                                                                   #
  #        RangeDateTime$Start and $End: calculate total time length, StartDate and EndDate           #
  #                May be in one or two columns;  2 columns must be appended before conversion        #
  #                    NA- use the 1st time in the data set                                           #
  #                    0- use Midnight at the start/end of the data                                   #
  #        StartIndex & EndIndex:   Calculate start and ending indexes in the data                    #
  #           (EndIndex is recalculated in each Y loop after removing any rows with missing data)     #
  #                print actual hours to be used for analysis, and 1st and last data point in file    #
  #        MyData_HRs:  difference in time from 1st data point to last data point -- of ALL data      #
  #        MyData_length:  How much data comes ***after*** StartIndex date and time                   #
  #           (MyData_length is recalculated after removing missing data in TimeCol and Y)            #
  #        Using Start/EndIndex, pull desired data into MyDataRead (from MyDataReada)                 #
  #                                                                                                   #
  ##################################################################################################### 

  str(MyDataReada)                                         #  removed +3600 from *tz=tz))+3600)*       6/14
  AnalyzeLength<-difftime(as.POSIXct(strptime(tail(MyDataReada[,TimeCol],1), "%Y%m%d%H%M",tz=tz)), as.POSIXct(strptime(MyDataReada[1,TimeCol], "%Y%m%d%H%M",tz=tz)),tz, units = c("hours")) 
  MyData_HRs<-(AnalyzeLength)             #  *24  # hours in length of actual data
  if (is.na(StartDate)){
    StartDate <- format(as.POSIXct(strptime(MyDataReada[1,TimeCol], "%Y%m%d%H%M",tz=tz)),"%Y%m%d%H%M")
  } else {if (StartDate == 0){
    StartDate <-paste(substr(MyDataReada[1,TimeCol],1,8),"0000",sep="")                #  Use StartTime as start of analysis date-time
  } else StartDate<-format(as.POSIXct(strptime(StartDate, timeFormat,tz=tz)),"%Y%m%d%H%M")     #  needed for when StartDate is specified in a different format--test!!
  }  
  StartIndex<-which(MyDataReada[,TimeCol]>=StartDate)   #  where is the desired startTime?  (if start point is later than first data point)
  MyData_length<-length(StartIndex)  
  
  if (is.na(EndDate)){                                                     #  removed tz=tz))+3600      6/14
    EndDate <- format(as.POSIXct(strptime(tail(MyDataReada[StartIndex,TimeCol],1), "%Y%m%d%H%M",tz=tz)),"%Y%m%d%H%M") # should these be pasted or posix?  todo
  } else {if (EndDate == 0){
    if (substr(MyDataReada[MyData_length,TimeCol],9,12)=="0000"){
      EndDate = paste(substr(format(as.POSIXct(strptime(MyDataReada[MyData_length,TimeCol], "%Y%m%d%H%M",tz=tz)),"%Y%m%d%H%M") ,1,8),"0000",sep="") 
    }
    else {
      EndDate<-paste(substr(format(as.POSIXct(strptime(MyDataReada[MyData_length,TimeCol], "%Y%m%d%H%M",tz=tz))+86400,"%Y%m%d%H%M") ,1,8),"0000",sep="")     #tz=tz))+86400  case:  25 hrs of data Khantyd
    #   changed back 10/10/17  --  this was adding 24 hrs to enddate -- not sure why 9/29/17 
    }
  } else EndDate<-format(as.POSIXct(strptime(EndDate, timeFormat,tz=tz)),"%Y%m%d%H%M")     #  needed for when EndDate is specified in a different format--test!!
  }  # end else

  if (!all(MyDataReada[,TimeCol]>EndDate)){     #  If all data is for one day, the dates could all be greater than EndDate
    EndIndex<-which(MyDataReada[,TimeCol]<=EndDate)   #  where is the desired endTime? 
  } else
    EndIndex
  
  #yloop MyData <- array(data=NA,c(EndIndex-StartIndex, 2))   #  grab time and Y  NO I THINK THIS IS DONE AT LINE 385, 391
  MyDataRead<-MyDataReada[StartIndex[1]:tail(EndIndex,n=1),]     #  grab all the data from array after StartTime, before EndTime

  print(MyDataReada[1,])
  print(tail(MyDataReada,1))
  #browser()
  #####################################################################################################
  #                                                                                                   #
  #        RefDateTime: get correct reference time;  subtract from data, start tim, and end time      #
  #                convert time to a count in Units, as set by Units parameter                        #
  #                                                                                                   #
  ##################################################################################################### 
  
  # convert time
  MyDataRead$time=as.POSIXct(strptime(MyDataRead[,TimeCol], "%Y%m%d%H%M", tz=tz))     #   tz="GMT"
  # convert time to a number (time in seconds, converted into hours)
  # should probably be done with a hard coded time zone, so CDT and DST don't cause problems
  
  if (is.na(RefDateTime)){
    RefTime <-as.POSIXct(strptime(MyDataRead[1,TimeCol], "%Y%m%d%H%M",tz=tz))                #  11/16 chg from timeFormat to "%Y%m%d%H%M": when RefDatTime=NA  Use StartTime as reference date-time
    RefTimeString <- MyDataRead[1,TimeCol]
  } else if (RefDateTime==0){
      if (timeFormatOrig=="numeric"){
        RefTime<-as.POSIXct(strptime(MyDataRead[1,TimeCol], "%Y%m%d",tz=tz))     # midnight of first day of data   why would it be "%Y-%m-%d"??  changed
        RefTimeString <- paste(substr(MyDataRead[1,TimeCol],1,10),"0000",sep="")
      } else {
        RefTime<-as.POSIXct(strptime(MyDataRead[1,TimeCol], "%Y%m%d",tz=tz))     # midnight of first day of data
        RefTimeString <- paste(substr(MyDataRead[1,TimeCol],1,8),"0000",sep="")
      }
    } else if (RefDateTime<=24 && timeFormatOrig=="numeric"){
    
        RefTime<-as.POSIXct(as.numeric(as.POSIXct(strptime(BaseDate, "%Y%m%d%H%M",tz=tz)))+(as.numeric(RefDateTime)*3600),origin=origin,tz)
        RefTimeString <- format(RefTime,"%Y%m%d%H%M")    #  ,"%Y%m%d%H%M")
        
      } else {  #  RefDateTime is a specific date and time
          RefTime<-as.POSIXct(strptime(RefDateTime, timeFormat,tz=tz))
          RefTimeString <- RefDateTime
        }
  #browser()
  #####################################################################################################
  #                                                                                                   #
  #        Start/EndTime: using RangeStart and RangeEnd, the data to be used in the analysis is       #
  #                identified.  It may be a date within the data file, or outside of the data         #
  #                file.  *Then the reference time is subtracted so that all time is relative to      #
  #                units (hours) from the user-given reference time.  *Time is converted from         #
  #                seconds to units (hours).  *If these dates are not valid appropriate errors are    #
  #                printed to the console. *Calculates average time between data points (dt).         # 
  #        variables:  MyDataRead$time.n, MyDataRead$time.hour and  MyData_length (used in analysis)  #                                          #
  #                    print total hours used in analysis and start/end times                         #
  #        MyData_hours:  based on these, the total hours to be analyzed is calculated.               #
  #                                                                                                   #
  #####################################################################################################
  
  StartTime<-(as.numeric(as.POSIXct(strptime(StartDate, "%Y%m%d%H%M",tz=tz)))-as.numeric(RefTime))/3600      # convert to hours
  if (is.na(StartTime) ){
    message<-paste("ERROR:  StartTime or StartDate or Date format is invalid: StartTime",StartTime," StartDate: ",StartDate," RefTime:",RefTime)
    errMsg<-paste(errMsg,message)
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    stop(message) 
  } 
  EndTime<-(as.numeric(as.POSIXct(strptime(EndDate, "%Y%m%d%H%M",tz=tz)))-as.numeric(RefTime))/3600   # convert to hours
  
  if (is.na(EndTime) ){
    message<-paste("ERROR:  EndDate or Date format is invalid: ",EndTime)
    errMsg<-paste(errMsg,message)
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    stop(message) 
  } 
  
  MyDataRead$time.n=as.numeric(MyDataRead$time) - as.numeric(RefTime)
  MyDataRead$time.hour<-MyDataRead$time.n/3600
  MyData_length<-length(MyDataRead[,1])
  drawPgram<-FALSE
  if (dt==0){
    drawPgram<-TRUE
    sumTab<-diff(MyDataRead$time.hour)          #  acts only on integer values, use .n
    #  dt<-sumTab[median(which(sumTab == max(sumTab, na.rm=TRUE)))]
    dt<-median(sumTab, na.rm=TRUE)
  }

  if (EndDate==tail(MyDataRead[,TimeCol],1) && StartDate==MyDataRead[1,TimeCol]){   # only add dt if using actual data points
    MyData_hours<-as.numeric(EndTime-StartTime) + dt         #   get number of hours to analyze
  }else{
      MyData_hours<-as.numeric(EndTime-StartTime)
  }
  cat("The program is using ",MyData_hours," hours for the analysis, starting at hour",StartTime,"and ending at hour",EndTime," relative to RefTime:",RefTimeString,"\n")
  #browser()
  #####################################################################################################
  #                                                                                                   #
  #        Interval, Increment, FreqInc, Period$Start, Period$End:                                    #
  #                Variables used by progressive, and StartSpans vector, are                          #
  #                calculated.  [This section is used to convert time to requested Units, but is not  #
  #                functional.]  *  If these variables are not making sense together errors are       #
  #                reported.  *A string is built that holds all the parameterized values and is       #
  #                printed in the RTF files.                                                          #
  #                                                                                                   #
  #####################################################################################################

  if (Interval==0 || Increment==0){     #  || oneCycle[1]>0){     #  No progression -- analyze full dataset
    Interval <-MyData_hours
    Increment<-MyData_hours
  } else if (Units=='hours'){
    Interval<-Interval
    Increment<-Increment
  } else if (Units=='days'){
    Interval<-Interval*24
    Increment<-Increment*24
  } else if (Units=='years'){
    Interval<-Interval*24*365
    Increment<-Increment*24*365
  } else if (Units=='weeks'){
    Interval<-24*7*Interval      
    Increment<-24*7*Increment
  } else {
    message<-paste("ERROR:  Units is improperly given: ",Units," Valid values:  hours, days, weeks, years")
    errMsg<-paste(errMsg,message)
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    stop(message) 
  }

  if (Period$Start==0){       
    Period$Start <-Interval
  } else if (Period$Start<0){
    message<-"ERROR:  The parameter Period$Start cannot be negative.\n"
    errMsg<-paste(errMsg,message)
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    stop(message)
  } else if (Period$Start>(1.25*MyData_hours)){        #  validation of Period$Start   
    message<-paste("ERROR:  The parameter Period$Start",Period$Start, "is much longer than the observation period",MyData_hours,".  Unreliable results.\n")
    errMsg<-paste(errMsg,message)
    print(message)
  } else if (Period$Start>MyData_hours){        #  validation of Period$Start   
    message<-paste("Warning:  The parameter Period$Start",Period$Start, "is slightly longer than the observation period",MyData_hours,".  Results may be unreliable.\n")
    errMsg<-paste(errMsg,message)
    print(message)
  } # end else Period$Start==0
  
  paramMsg<-paste("\n  TimeCol=",TimeCol,",  Y=",Y, ",  header=",header,"\n --  Periods=",Period["Set"],", Units=",Units, ",  Interval=",format(Interval,nsmall=3), ",  Increment=",format(Increment,nsmall=3), "\nPeriod$Start=",format(Period$Start,nsmall=3), ",  FreqInc=",format(FreqInc,nsmall=3), ",  Period$End=",format(Period$End,nsmall=3), "\nRefDateTime=",RefDateTime, ", StartTime=",format(StartTime,nsmall=3),", EndTime=",format(EndTime,nsmall=3),"\n Data hrs=",format(MyData_hours,nsmall=3),"  dt=",format(dt,nsmall=3)," hours\n",functionName,"\n")
  if (FreqInc==0){
    FreqInc<-1
  } else if (FreqInc<0){
    message<-"ERROR:  The parameter Period$Increment cannot be negative.\n"
    errMsg<-paste(errMsg,message)
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    stop(message)
  } else if (FreqInc<1){
    message<-"Warning:  Using a fractional increment (Freq$Inc<>1) may result in reported maximal amplitudes that are not from separate maxima -- they may be points on the shoulder of the maximal peak.\n"
    errMsg<-paste(errMsg,message)
    print(message)
  } 
  
  if (Period$End==0){
    if (dt==0){                      #  dt is previously calculated so this should never occurr (error?)
      Period$End<-4
    } else if (Period$End<0){
      message<-"ERROR:  The parameter Period$End cannot be negative.\n"
      errMsg<-paste(errMsg,message)
      closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
      stop(message)
    } else if (Period$End>MyData_hours){
      message<-"ERROR:  The parameter Period$End is larger than the number of hours.  ???\n"
      errMsg<-paste(errMsg,message)
      print(message)
    } else Period$End<-4*dt
    if (Period$End>Period$Start){
      Period$End<-Period$Start/4
      FreqInc<-.1
    }
  }    #    end Period$End==0
  cat(StartTime," StartTime ",Period$End," Period$End ",Period$Start,"Period$Start",Interval,"Interval",Increment,"Increment\n")
  
  #This is built at line 546 ---  paramMsg<-paste("\n  TimeCol=",TimeCol,",  Y=",Y, ",  header=",header,"\n --  Periods=",Period["Set"],", Units=",Units, ",  Interval=",format(Interval,nsmall=3), ",  Increment=",format(Increment,nsmall=3), "\nPeriod$Start=",format(Period$Start,nsmall=3), ",  FreqInc=",format(FreqInc,nsmall=3), ",  Period$End=",format(Period$End,nsmall=3), "\nRefDateTime=",RefDateTime, ", StartTime=",format(StartTime,nsmall=3),", EndTime=",format(EndTime,nsmall=3),"\n Data hrs=",format(MyData_hours,nsmall=3),"  dt=",format(dt,nsmall=3)," hours\n Percent of missing (blank) sample values: %",missingData*100,"\n",functionName,"\n")
  
  par(mar=c(4,4,1,1)) # set up margins
  
  #  #####    note on indexes and variables:  one set tracks through the ACTUAL times and data points:  #####
  #                EndIdx, StartIdx, thisIdxCnt, MyData_length, TimeIdx
  #  #####    another set tracks through the periods, in hours, being calculated:       ######
  #                MyData_hours; StartSpans, Interval, Increment, Progression_end, StartTime, StartSpanTime, EndTime
  #  #####    These two must, of course, align. See:   TimeIdx<-which(StartSpanTime <= MyData$time.hour) 
  EndIdx<-0
  
  if (Interval > MyData_hours){    #  warn if interval chosen is longer than data
    message<-paste("Warning: Chosen Progressive$Interval (",Interval,") is longer than data file (",MyData_hours,") (MyData_hours).\n")
    print(message)
    errMsg<-paste(errMsg,message)
  } 
  if (Increment> MyData_hours) {
    message<-paste("Warning:  Chosen Progressive$Increment (",Increment,") is longer than data file (",MyData_hours,") (MyData_hours).\n")
    print(message)
    errMsg=paste(errMsg,message)
  }
  
  #####################################################################################################
  #                                                                                                   #
  #        StartSpans: This master, controlling vector is setup here.  It depends on the length of    #
  #               the Interval (hours of data being analyzed = MyData_hours) and the Increment        #
  #                selected, if any.  If this is not a progressive, there is only one element in      #
  #        Progression_end:  The is a count of the number of elements in StartSpans and is used to    #                                                     #
  #                determine the number of loops                                                      #
  #                                                                                                   #
  #####################################################################################################

  if (MyData_hours==Interval){    #StartTime==MyData$time.hour[1] && 
    StartSpans<-seq(from=0,to=MyData_hours-1,by=Increment)        #StartSpans<-seq(1,Interval, by=Increment)
    
  } else {if ((MyData_hours-Interval+Increment)<=0){
    errMsg<-paste(errMsg,"Error:  MyData_hours-Progressive$Interval+Progressive$Increment is < 1:  ",MyData_hours,"-",Interval,"+",Increment,".  Using ",MyData_hours,"as Interval.\n")
    closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
    Interval<-MyData_hours     # ????? is this ever run?
  }
    StartSpans<-seq(from=0,to=(MyData_hours-Interval+Increment),by=Increment)     #  one too many (SS), perfect for all (NA/NA)
    #StartSpans<-seq(1,Interval, by=Increment)
    
    if (tail(StartSpans,1)==round(MyData_hours)){
      StartSpans<-StartSpans[1:length(StartSpans)-1]    #  lose the last interval.
    }
  }  
  Progression_end<-length(StartSpans)

  #####################################################################################################
  #                                                                                                   #
  #        Initialize and prepare for looping.  Ys_end:  the count of the variable columns param      #
  #                 Original_Y:  save the original Y for use in y loop                                #
  #                 oldTimeCol:  save the original TimeCol for use in y loop                          #
  #                                                                                                   #
  #####################################################################################################
  
  Original_Y<-Y   #yloop  added
  #Y<-2   #  yloop Only one y is processed at a time, and it will be in column 2, with time in column 1
     #lenNonNA<-!is.NA(MyDataReada)
     #maxNonNA<-max(apply(lenNonNA, 2, sum))+1
  # yloop define these outside loop using 
  #  print Y title, and points Interval
  sumN <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumLow <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumHi <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumMean <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumMedian <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumMode <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumSD <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  sumT <- matrix(data=NA,nrow=Ys_end,ncol=Progression_end+1)
  oldTimeCol<-TimeCol  #  preserve original time column 
  
  #####################################################################################################
  #                                                                                                   #
  #        y loop: This loops through the variable columns (Y parameter) and processes each           #
  #                 separately, in order to allow for handling missing data in each column separately #
  #                 Original_Y:  save the original Y for use in y loop                                #
  #                 oldTimeCol:  save the original TimeCol for use in y loop                          #
  #                 missing:     missing data vector is cleared                                       #
  #        output files are created at the end of each iteration. (see program end)                   #
  #                                                                                                   #
  #####################################################################################################
  
  for (y in 1:Ys_end){    #  yloop   y is for preserving results in target arrays.  variable column=2

    missing<-c()
    yNew<-0
    #TimeCol<-TimeColLen   ** 
    #  1st col, if originally in col 1;  2nd col if originally 2 col   Nov 14, 17
    #if (TimeColLen==2){
    #  TimeCol<-oldTimeCol+1      commented out if/then 12/19/17  (already changed at 345)
    #} else {
      TimeCol<-oldTimeCol
    #}
    
    #####################################################################################################
    #                                                                                                   #
    #        missing: This small loop processes the time column and one Y column for missing data       #
    #                 prints any needed error messages                                                  #
    #        EndIndex:  recalculates the EndIndex based on missing data (is MyData_hours recalculated?) #
    #        MyData_length:  recalculated now that missing data is removed                              #
    #        paramMsg, paramMsg2:  stores information about missing data for output files               #
    #                                                                                                   #
    #####################################################################################################
    
    #yloop    need to pass only time and y in col 1,2  TimeCol=1;  Y=2 by the time it gets to MyDataRead
    for (yy in c(TimeCol,Y[y])){      #yloop keep this loop  y is current y;  yy is current column being processed for NA (time or variable)
      #browser()
      is.na(MyDataRead[,yy])<-which(MyDataRead[,yy]=="")    #   replace spaces with NA
      if (all(is.na(MyDataRead[,yy]))){
        message<-paste("ERROR:  You have selected a column that has no data in it:  column",yy)
        errMsg<-paste(errMsg,message)
        closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
        stop(message) 
      }

      keepersY<-na.omit(MyDataRead[,yy])            #  eliminate NA and return list of good (na.actions preserved)
      missing<-union(na.action(keepersY),missing)     #  Add NA rows to previous NA rows and track missing from Time and Y cols
      if (length(na.action(keepersY))>0){     # count how many have an NA action associated
        missingDataCol=append(missingDataCol,yy,length(missingDataCol))      #  
      }
      #   use inverse of omitted NA indexes to get the indexes for all rows with good values
    }

    #yloop     TimeCol=1;  Y=2 by the time it gets to MyDataRead
    nCol<-dim(MyDataRead)[2]       #Number of columns  (cannot use "time" in combining columns for new matrix below)
    
    if (length(missing)>0){
      MyData<-MyDataRead[-missing,c(TimeCol,Y[y], (nCol-2):nCol )]       # yloop   keep only time and current Y  and time cols      #MyDataRead[-missing,c(TimeCol,Y[y], nCol-2:nCol )]      # yloop   keep only time and current Y
      #missingDataCol<-na.omit(missingDataCol)      #  skip any NAs in this vector
      message<-paste("Note:  Missing data was found in column ",paste(missingDataCol,collapse=", "),".  Rows having missing data are omitted from the analysis for this column.  \n")
      print(message)
      errMsg=paste(errMsg,message)
    } else {
      MyData<-MyDataRead[,c(TimeCol,Y[y], (nCol-2):nCol )]        # yloop   c(TimeCol,Y)
    }   #  from here on, TimeCol=1 and data is in column 2
    missingData<-(length(MyDataRead[,TimeCol])-length(MyData[,TimeCol]))/length(MyDataRead[,TimeCol])    #  rowsData<-length(MyDataRead[,1])
    TimeCol<-1
    
    #  recalculate EndIndex after removing data:  Added 11/1/2017 because missing calc was moved here
    if (!all(MyData[,TimeCol]>EndDate)){     #  If all data is for one day, the dates could all be greater than EndDate
      EndIndex<-which(MyData[,TimeCol]<=EndDate)   #  where is the desired endTime? 
    } else
      EndIndex
    
    oldMyData_length<-MyData_length          #  11/1/2017
    MyData_length<-length(MyData[,TimeCol])
    cat("There are ",MyData_HRs," actual hours of data in this file, ",MyData[1,TimeCol]," to ",MyData[tail(EndIndex,n=1),TimeCol],", and ",MyData_length,"data points.\n  %",missingData*100,"of data points are missing.\n")       #  Analysis will proceed from",StartDate," to ",EndDate,"\n")
    #  " update with known missing data amounts"
    paramMsg<-paste("\n  TimeCol=",TimeCol,",  Y=",Y[y], ",  header=",header,"\n --  Periods=",Period["Set"],", Units=",Units, ",  Interval=",format(Interval,nsmall=3), ",  Increment=",format(Increment,nsmall=3), "\nPeriod$Start=",format(Period$Start,nsmall=3), ",  FreqInc=",format(FreqInc,nsmall=3), ",  Period$End=",format(Period$End,nsmall=3), "\nRefDateTime=",RefDateTime, ", StartTime=",format(StartTime,nsmall=3),", EndTime=",format(EndTime,nsmall=3),"\n Data hrs=",format(MyData_hours,nsmall=3),"  dt=",format(dt,nsmall=3)," hours\nPercent of missing (blank) sample values: %",missingData*100,"\n",functionName,"\n")
    paramMsg2<-paste("There are ",MyData_HRs," actual hours of data in this file, ",MyData[1,TimeCol]," to ",MyData[tail(EndIndex,n=1),TimeCol],", and ",MyData_length,"data points.\n  %",missingData*100,"of data points are missing.\n")  #  print missing data info
    
  #####################################################################################################
  #                                                                                                   #
  #        Set variables used by the Data Summary section of the RTF and DAT file                     #
  #               outputs are initialized.  Looping through all the data columns, summation variables #
  #                are calculated for each column (Y).                                                #
  #        Progression_end:  The is a count of the number of elements in StartSpans and is used to    #                                                     #
  #                determine the number of loops                                                      #
  #                                                                                                   #
  #####################################################################################################

    yy=2
    yNew<-yNew+1
    sumN[yNew,Progression_end+1]<-MyData_length
    sumLow[yNew,Progression_end+1]<-min(MyData[,yy], na.rm=TRUE)
    sumHi[yNew,Progression_end+1]<-max(MyData[,yy], na.rm=TRUE)
    if (is.na(mean(MyData[,yy], na.rm=TRUE))){    #  warn if cannot calculate a mean
      message<-paste("Warning: Non-numeric data present -- may indicate there is a header when header is set to FALSE.\n")
      print(message)
      errMsg<-paste(errMsg,message)
    } 
    sumMean[yNew,Progression_end+1]<-mean(MyData[,yy], na.rm=TRUE)
    sumMedian[yNew,Progression_end+1]<-median(MyData[,yy], na.rm=TRUE)   #  could be proper dt if VERY irregular data
    sumSD[yNew,Progression_end+1]<-sqrt(var(MyData[,yy],y=NULL))
    #the next two lines are an old implementation of MODE, but large numbers with decimal places cause it to be SLOOOOOOOOW
    #sumTab<-tabulate(MyData[,y]*1000)          #  acts only on integer values, ensure integers;   each value is counted in it's cardinal location     
    #dTtest<-which(sumTab == max(sumTab, na.rm=TRUE))/1000   #  which data value is most often used? 
    # The next two lines are an alternate method.  But in the case of large numbers with decimal places, there will be no maximal value....all unique values
    #sumTab<-diff(MyData[,y])      #  so we have decided to exclude this calculation from the output     
    #dTtest<-Mode(sumTab, na.rm=TRUE)
    sumT[yNew,Progression_end+1]<-dt
    # if (length(dTtest)>1){    # if more than 4 are the same as the max dT, average them to get Mode
    #   sumMode[yNew,Progression_end+1] <- mean(dTtest, na.rm=TRUE)
    #   } else {sumMode[yNew,Progression_end+1]<-dTtest   #  The one used the most is the Mode
              #}
    #   sumT      hours+dt=T acknowledges that most times points are binned, so should be more than actual hours in file
  #}
  
  #####################################################################################################
  #                                                                                                   #
  #        Interval, Increment, FreqInc, Period$Start: Additional checks are done on these variables  #
  #                and errors are reported if any issues are found.                                   #
  #                                                                                                   #
  #####################################################################################################

  Page<-0
  if (oneCycle[1]==0){                    #  if oneCycle != 0, Period$End and FreqInc are not used.
    if (Period$End <= 0 || FreqInc <= 0){
      message<-paste("ERROR:  Period$End (",Period$End,") and Period$Increment (",FreqInc,") cannot be 0 when Period$Set is 0.\n")
      errMsg<-paste(errMsg,message)
      closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
      stop(message)
    }
    if (Period$Start<Period$End){
      message<-paste("ERROR:  The parameter Period$Start (",Period$Start,") cannot be smaller than Period$End (",Period$End,") when Period$Set is 0.\n")
      errMsg<-paste(errMsg,message)
      closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
      stop(message)
    }
    if (FreqInc > Period$Start){    #  is this needed??    6/14
      message<-paste("ERROR:  Period$Increment (",FreqInc,") cannot be larger than Period$Start (",Period$Start,") (or Progressive$Interval) when Period$Set is 0.\n")
      errMsg<-paste(errMsg,message)
      closeOutput(file=fileName3,output=Output,console=Console,opar=opar,ERR=TRUE,errMsg=errMsg,paramMsg=paramMsg)
      stop(message)
    } else if (FreqInc>1) {
      message<-"Warning:  Chosen Increment (>1) is less than optimal.  Increments would optimally be <=1.\n"
      print(message)
      errMsg=paste(errMsg,message)
    }

    #####################################################################################################
    #                                                                                                   #
    #        RowCnt, printYlab1: *RowCnt* is the count of the number of rows in the matrices needed     #
    #                to hold the repeating variables, one set for each loop.                            #
    #                The Y column header string, *printYlab1*, is built, for use at the top             #
    #                of the PDF file.  It may be a column number, or a string from the header.          #
    #                or a string from the header.                                                       #
    #        Ycol, OriginalYcol:  reset                                                                 #
    #        PDF printing preparation:  margins and layout are setup; numerical results are printed     #
    #        StartDateP, EndDateP:  the dates are converted for printing. (re converted at line 1572)   #
    #                                                                                                   #
    #####################################################################################################
    
    # How many loops?  the integer (truncated) part of 1 + (tau-s/tau-e -1)/delta.
    RowCnt<-floor(1 + ((Period$Start/Period$End)-1)/FreqInc)      #  using ceiling with out 1+ is not equivalent, as non integer result is 1 too small
    RowCntAlt2<-floor(1 + ((1/Period$End)/(1/Period$Start)-1)/FreqInc)    #  for example:  start=52, end=2, inc = 5 should give 6 results;  (52/2)+1 harmonics
    
    if (RowCnt<1){
      RowCnt<-1                              #  8/11/14  for LS needs to be 1   (was 2)
    }
  } else {if (Components>1){
              RowCnt<-1}
          else {RowCnt <- length(oneCycle)}   #  if oneCycle != 0, reset Period$End and FreqInc to default=1
    print("resetting freq start and inc")
  }

    Ycol<-2         #  Y[y]  8/30/2017    the variable data is always in column 2
    OriginalYcol<-Y[y]  #  for all printable info, show the user the original column number and name
    if (yLabel==""){
      printYlab<-names(MyDataRead)[OriginalYcol]      #not printYlab1  10/6/2017
      if (header==FALSE){
        printYlab<-paste("Data Column",OriginalYcol)      #not printYlab1  10/6/2017
      }
    } else {
      printYlab<-yLabel       #not printYlab1  10/6/2017
    }

    #par(mfrow=c(3,1),mar=c(4,4,2,1))   # 6/10/2016
    #  test 6/10/2016  layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), widths=c(1), heights=c(1,3,3))   #   this gets reset in CatWindow, but is needed when j>1       
    #this is built on line 546   paramMsg<-paste("\n  TimeCol=",TimeCol,",  Y=",Y, ",  header=",header,"\n --  Periods=",Period["Set"],", Units=",Units, ",  Interval=",format(Interval,nsmall=3), ",  Increment=",format(Increment,nsmall=3), "\nPeriod$Start=",format(Period$Start,nsmall=3), ",  FreqInc=",format(FreqInc,nsmall=3), ",  Period$End=",format(Period$End,nsmall=3), "\nRefDateTime=",RefDateTime, ", StartTime=",format(StartTime,nsmall=3),", EndTime=",format(EndTime,nsmall=3),"\n Data hrs=",format(MyData_hours,nsmall=3),"  dt=",format(dt,nsmall=3)," hours\nPercent of missing (blank) sample values: %",missingData*100,"\n",functionName,"\n")
    # Use the periodogram
#     if (window!="noTaper"){
#       CatWindowList<-CatWindow(window=window,myData=MyData[,Ycol],Time=MyData$time.hour,dataN=MyData_length,Start=MyData$time.hour[1]+StartSpans[1]-1)    # plots the full set of data w/filter
#       # must pass dataN of integer 
#     }
    par(mar=c(3.85,4,2,1), oma=c(2,0,4,0))         # oma=c(2,0,2,0)  set up margins  par(mar=c(4,4,2,1), oma=c(2,0,1,0))  
     if (oneCycle[1]!=0){  
       cat("non") #  not LSS
            layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
                        widths=c(1), heights=c(1,5,5))          #   c(figure#1, figure#2), 2 row, 1 col, ...   (was c(1,1,1))
     } else {         # LSS     
            layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
                 widths=c(1), heights=c(1,6))          #   c(figure#1, figure#2), 2 row, 1 col, ...   (was c(1,1,1))
     }

    plot.new()
    n<-length(MyData[,Ycol])
    if (is.numeric(MyData[n, TimeCol])){
      startDateP<-as.POSIXct(strptime(x=MyData[1,TimeCol], format="%Y%m%d%H%M", tz=tz))
      endDateP<-as.POSIXct(strptime(x=MyData[n,TimeCol], format="%Y%m%d%H%M", tz=tz))
    } else {startDateP<-MyData[1,TimeCol]
            endDateP<-MyData[n,TimeCol]}
    title(paste('Start time:',startDateP,';    End time:',endDateP),cex=.7)
    #-------------------
#     if (Plex==TRUE){
#       CatPlexigram<- CatPlex(CatWindowList$data, foldLen=foldLen, Debug=FALSE) 
#     }
#     if (dt!=0){
#       CatPeriodogram<- CatPeri(CatWindowList$data,binsPerHr=1/dt, Hx=.15, cex=.9, Debug=FALSE)    # pass samples/day when units=hrs
#     } else {plot.new()}
      cat("\n\tPR\t\t   F\t\t\tP \tSS[j]\t i\t cycle \t  Mesor \t  s.e. \t \t  Amp \t\t s.e. \t \tPhi\t\t s.e.\n")
    
    #####################################################################################################
    #                                                                                                   #
    #        Set up variables needed for printing the model:  decide if all models are printed or only 1#
    #                Usually we want to print the model across all test data, however if there is       #
    #                too much data, or to many models, the printout will be unreadable (black),         #
    #                in which case only one  model is printed.  Thus we need to know the LCM if a       #
    #                multiple component cosinor is being computed.  And we need to define a model       #
    #                length.  These are used in computing how many models or how much data can          #
    #                be sensibly plotted and displayed.                                                 #
    #                                                                                                   #
    #####################################################################################################
    
realLCM<-LCM
if (minPeriod==0){  #  if this is a LS
  modelLen<-360
} else {
  modelLen<-(LCM/minPeriod)*6       #  model should be long enough that each cycle of the shortest period (minPeriod) has 6 pts (LCM/minPeriod is how many cycles in LCM)
  if (LCM>MyData_hours){
    LCM<-MyData_hours
    modelLen<-(LCM/minPeriod)*24
  }
}

if (modelLen<360){      # should be at least 360 points generated in a model
  modelLen<-360        # how many points to calculate in the model (for valid plotting representation of cycle -- to avoid aliasing)  
}
    
    #####################################################################################################
    #                                                                                                   #
    #        Progressive setup:   Set up variables for holding model values and results                 #
    #                There needs to be an array for each, capable of holding a full set of variables    #
    #                for each progressive iteration.  The number of iterations is held in               #
    #        ProgressionEnd
    #                in which case only one  model is printed.  Thus we need to know the LCM if a       #
    #                multiple component cosinor is being computed.  And we need to define a model       #
    #                length.  These are used in computing how many models or how much data can          #
    #                be sensibly plotted and displayed.                                                 #
    #                                                                                                   #
    #####################################################################################################
    
M <- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
Model_Y <- matrix(data=NA,nrow=RowCnt, ncol=modelLen)
Model_Y_mag <- matrix(data=NA,nrow=RowCnt, ncol=modelLen)
#plotModel <- matrix(data=NA,nrow=RowCnt, ncol=modelLen)     # 6/27/2016
plotModel <- vector(mode="numeric",length = modelLen)
multi_Model_Y <- matrix(data=NA,nrow=RowCnt, ncol=modelLen)
multi_Model_mag <- matrix(data=NA,nrow=RowCnt, ncol=modelLen)
newPR <- array(data=NA,c(RowCnt, Progression_end,Components))
PHI <- array(data=NA,c(RowCnt, Progression_end,Components))
PHIr <- array(data=NA,c(RowCnt, Progression_end,Components))
A <- array(data=NA,c(RowCnt, Progression_end,Components))
PR <- array(data=NA,c(RowCnt, Progression_end,Components))
P <- array(data=NA,c(RowCnt, Progression_end,Components))
#P2 <- array(data=NA,c(RowCnt, Progression_end,Components))
newF <- array(data=NA,c(RowCnt, Progression_end,Components))
#testPg <- array(data=NA,c(RowCnt, Progression_end,Components))
#testPb <- array(data=NA,c(RowCnt, Progression_end,Components))
F <- array(data=NA,c(RowCnt, Progression_end,Components))
mesor_se <- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
phi_se <- array(data=NA,c(RowCnt, Progression_end,Components))
amp_se <- array(data=NA,c(RowCnt, Progression_end,Components))
Cycle <- array(data=NA,c(RowCnt, Progression_end,Components))
Err <- array(data="",c(RowCnt, Progression_end,Components))
hours <-matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
nPts<- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
sPts<- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
time<- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
yVar<- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
MSS<- vector(mode="integer",length = Components)
printP<- array(data=NA,c(RowCnt, Progression_end,Components))
if (Components>1){
  multi_P<-matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
  multi_PR<-matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
  Magnitude<-matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
}
Orthophase<-matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
Bathyphase<-matrix(data=NA,nrow=RowCnt, ncol=Progression_end)
newData<- matrix(data=NA,nrow=RowCnt, ncol=Progression_end)    # holds filtered or unfiltered data, myData for each Interval
#----------------------array definitions

Page<-0
      for (j in 1:Progression_end){   #  make a vector with incremental values.  use var(i) for necessary indexes
      # from:to  browser()
        if ((j %in% c(1,13,29,45,61,77,93,109,125,141,157,173,189)) || (oneCycle[1]>0 && Progression_end!=1)){    #  new page or (prog & not LSS)      
          Page<-Page+1
          #browser()
          if (j!=1){    #  for the starting table setup for all progressive pages EXCEPT the very first one

            jht<-7.6 - (Page*3-5)/j*2    #.2   2.5
            cexVar<-.7      #.5
            cexMain<-.95
            htVar<-.62   #.8     #.68
            hdrRow<-10          # 9
            if (oneCycle[1]>0 && Progression_end!=1){   #  progressive, Not LSS
              layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))  #  test 6/10/2016  seems to cause a new page 

              jht<- -0.4 + Page*.5 + Page/5    # 6/19/    -0.2
              cexVar<-.7
              cexMain<-.95
              htVar<-.7    # .8
            } else {     #  LSS
              layout(matrix(c(1), 1, 1, byrow = TRUE), widths=c(1), heights=c(1))  #  test 6/10/2016  seems to cause a new page             
            }
          } else if (j==1) {   #   this is always done when j==1
              Page<-0               # reset to 0 on j==1 because it needs to be 1 less than actual
              jht<- -0.2     # 0    #  for the starting table setup for the VERY first page
              cexVar<-.8      #.7
              cexMain<-1.2
              htVar<- .8   #  .7
              hdrRow<-9.5
              if (oneCycle[1]==0 || (oneCycle[1]==0 && Progression_end!=1)){  #  For gliding spectrum (LSS & progressive)
                cexVar<-.7
                cexMain<-.9
                hdrRow<-10
              }}        #  9.2  

          if (oneCycle[1]>0){   #  not LSS 
            if (Components==1){   #  single component
              main<-paste("Each Period from single-component COSINOR: ",printYlab)       #not printYlab1[y]  10/6/2017
            } else {      #  multiple component
              main<-paste("Each Period from multiple-component COSINOR: ",printYlab)       #not printYlab1[y]  10/6/2017
            }
          } else {   #  LSS
            if (Progression_end==1){     #  not a progressive, but spectrum
              main<-paste("Periods of max Ampl from COSINOR Least Squares Spectrum: ",printYlab)       #not printYlab1[y]  10/6/2017
            } else {
              main<-paste("Periods of max Ampl from COSINOR Gliding Spectrum: ",printYlab)         #not printYlab1[y]  10/6/2017
            }
          }
          if (j==1){
            mtext(paste('Function:',functionName,"  ",fileName1,"; TimeCol=",TimeCol," Ycol=",OriginalYcol, " RefDateTime=",RefDateTime),side=1,line = 1, cex =.7)    # , adj=.9
            mtext(paste(window,"  ",thisTime," Units=",Units, " Interval=",format(Interval,digits=6), " Increment=",format(Increment,digits=6), " header=",header," Set=",Period["Set"], "Start/End=", format(Period$Start,digits=6),"/",format(Period$End,digits=6)),side=1,line = 2, cex =.7)
          } else {
            mtext(paste("P",Page),side=1,line = 4, cex =cexVar, adj=-.1)
          }
          # (TimeCol=1,Y=2, Components=1, window="noTaper", RefDateTime=NA,  timeFormat="%Y%m%d%H%M", RangeDateTime=list(Start=NA, End=NA), Units="hours", dt=0, Progressive=list(Interval=144, Increment=8), Period=list(Set=0,Start=0,Increment=1,End=0),header=F, Skip=0, Colors="BW",Graphics="pdf",Output=list(Txt=F,Dat=T, Doc=T,Graphs=F),yLabel="", Console=F,Debug=FALSE,IDcol="fileName", fileName=fileName,functionName="") 
            
          plot(0:10, 0:10, type="n",xlab="",ylab="",axes=FALSE, main=main,cex=cexVar,cex.main=cexMain)

          text(c(-.2,1.2,2.2,3,3.9,5.3,6.3,7.3,8.3,9.3),c(hdrRow,hdrRow,hdrRow,hdrRow,hdrRow,hdrRow,hdrRow),labels=c("Interval","Period","P","PR","Mesor", "s.e.","Amp","s.e.","Phi","s.e."),cex=cexMain,adj = c(0,0))   #  adj = c(0,0) left justifies   
          abline(h=hdrRow-.3)    
          #page footer
          }
      
          max3Amp<-max2Amp<-maxAmp<-1
          StartSpanTime<-StartTime+StartSpans[j]    # Feb 2014 added back in; 11/4  StartTime+StartSpans[j]-1 # 10/18 added StartTime+ DH;  9/30  MyData$time.hour[1]+StartSpans[j]-1
          EndSpanTime<-StartSpanTime +Interval          # 6/14 no subtraction; 5/2014 subtract 1 oneSec instead; 9/30  -1***adj to match gc  Add Interval length to the last Interval StartSpans[j]+Interval to get the ending of the Interval
         # when using 24 hour progressive Interval, if should not go from 8:00 to 8:00 in one Interval, should only go from 8:00 to 7:59:59!  
        TimeIdx<-which(StartSpanTime <= MyData$time.hour)   #   get indices past the  time of this Interval
        if (is.na(TimeIdx[1])){
          StartIdx<-length(MyData$time.hour)     #  use the largest index (last data point)
        } else StartIdx<-TimeIdx[1]                # index of starting time is FIRST index in array of indices
           if (EndSpanTime>MyData_hours+StartTime){   # Feb 2014 had to add + StartTime to these 2 lines
            EndSpanTime<-MyData_hours+StartTime
          }
#browser()
          if (Progression_end==j){                  #  6/14   last (or only) spans need to be treated differently than all others
            # needed to round because equal numbers didn't compare as equal due to differing precisions  (116.5333333 <> 116.5333)
            TimeIdx<-which(round(EndSpanTime,digits=10) >= round(MyData$time.hour,digits=10))   #  should be gte;  get indices UP TO the end of this Interval
            #  in what case does this need to be gte?   ActivityP1-ZCM-SSend2 serial section might need it to be equal
          } else TimeIdx<-which(EndSpanTime > MyData$time.hour)    #  6/14
          EndIdx<-which.max(TimeIdx)               # index of ending time is LAST index in array if indices
          Interval_hours<-EndSpanTime-StartSpanTime

          thisIdxCnt<-EndIdx-StartIdx+1   # data points in this Interval  -- used for S11, and SigmaHat
          Loops<-RowCnt          
          if (Components==1 && oneCycle[1] > 0){              #  a specified period in oneCycle overrides spectrum calculation  (and Period$Start/End?)
            Cycles<-oneCycle
            Loops<-length(Cycles)        
          } else if (Components>1){
            Cycles<-oneCycle
            Loops<-1
          }
        if (Debug){
          cat("StSpan ",StartSpans[j]," Interval ",Interval," EndDate ",EndDate," StrtIdx ",StartIdx, " EndIdx ",EndIdx," thisIdxCnt ",thisIdxCnt," j ",j,"\n")
        }
        if (window != "noTaper"){          # calculates a window for the Interval
          # pass exact interval-1!!!!
          CatWindowList<-CatWindow(window=window,myData=MyData[StartIdx:EndIdx,Ycol],Time=MyData$time.hour[StartIdx:EndIdx],dataN=Interval_hours,Start=StartSpanTime, debug=Debug)
          newData<-CatWindowList$data
        } else { newData<-MyData[StartIdx:EndIdx,Ycol]}

        #  summary is on the filtered data.
        sumN[y,j]<-length(newData)  # should be changed to this
        #sumN<-length(newData[,Ycol])
        sumLow[y,j]<-min(newData, na.rm=TRUE)
        sumHi[y,j]<-max(newData, na.rm=TRUE)
        sumMean[y,j]<-mean(newData, na.rm=TRUE)
        sumMedian[y,j]<-median(newData, na.rm=TRUE)   

        sumSD[y,j]<-sqrt(var(newData,y=NULL))
        #the next two lines are an old implementation of MODE, but large numbers with decimal places cause it to be SLOOOOOOOOW
        #sumTab<-tabulate(newData*1000)          #  acts only on integer values, ensure integer;   each value is counted in it's cardinal location     
        #dTtest<-which(sumTab == max(sumTab, na.rm=TRUE))/1000   #  
        # The next two lines are an alternate method.  But in the case of large numbers with decimal places, there will be no maximal value....all unique values
        #sumTab<-diff(newData)         #  so we have decided to exclude this calculation from the output            
        #dTtest<-median(sumTab, na.rm=TRUE)

        sumT[y,j]<-dt
        # if (length(dTtest)>1){    # if more than 4 are the same as the max dT, use mean instead of median
        #   sumMode[y,j] <- mean(dTtest, na.rm=TRUE)
        # } else {sumMode[y,j]<-dTtest   #  ????way to find dt for non-equidistant data
        #}

          for (i in 1:Loops){
            # fit sinusoidal model
            if (oneCycle[1]==0){                  #  if a period has been specified, use present cycle
              cycle<-Period$Start/(1+((i-1)*FreqInc))          ####  Interval/(1+((i-1)*FreqInc))  from Interval/i
              } else cycle<-Cycles[i]

            #  set subject ID
            if (IDcol!="fileName"){    # (this could be taken outside the Y loop and made an outer loop)
              if (!is.numeric(IDcol) ){    # not "fileName" and not numeric is an error!!!! fill with spaces
                SubjectID[i]<-" "
              } else {
                  SubjectID[i]<-MyData[StartIdx,IDcol]     #  set for each Loop
                }
            }

            Cycle[i,j,1]<-cycle                    # store for later 
            yVar[i,j]<-names(MyData)[Ycol]    #yloop change to Ycol
            hours[i,j] <- paste(format(StartSpanTime,nsmall=1)," - ",format(EndSpanTime,nsmall=1),sep="")  # took :59 off because can be decimal hrs;  this shows ALL used
            cycleCnt<-(EndSpanTime-StartSpanTime)/cycle
            minPtCnt<-2*cycleCnt+1
            nPts[i,j] <- EndIdx-StartIdx+1
            sPts[i,j] <- paste(StartIdx,"-",EndIdx,"\n(",nPts[i,j],")")

            time[i,j] <- paste(MyData[StartIdx,TimeCol],"-\n",MyData[EndIdx,TimeCol])
            if (nPts[i,j]<=minPtCnt) {        #  not enough data points for the trial period being analzyed.
              print(paste(keyMsgs[5],"C=",2*cycleCnt+1,"min pts=",minPtCnt))
              Err[i,j,]<-paste(Err[i,j,],errKey[5],"min pts=",minPtCnt)   # "@"
              #next   should not skip calculations  -- calculate as many as possible  jiii
            }
            
            #if (cycle>((MyData$time.hour[EndIdx]+dt)-MyData$time.hour[StartIdx])) {     #  data covers 90% of timeperiod < target period   (8-2-2016 added +dt for when "numeric" hours used -Garth)
            if (cycle>Interval) {     #  data covers 90% of timeperiod < target period   (8-2-2016 Useing Interval because StartIdx and EndIdx are first and last, not always interval size when units=numeric)
              if ((.9*cycle)>(MyData$time.hour[EndIdx]-MyData$time.hour[StartIdx])) {    #  Error if much smaller than cycle
                print(keyMsgs[7])
                Err[i,j,]<-paste(Err[i,j,],":",errKey[7])   # "@@@"
                #next
              } else {  #  Warning if cycle about same size as data Interval
                print(keyMsgs[8])
                Err[i,j,]<-paste(Err[i,j,],":",errKey[8])   # "@@"
              }
            }

            # still need to check for gaps
            CosMatrix<-matrix(data=NA,nrow=CosMatrixDim, ncol=CosMatrixDim)
            testXmean<-matrix(data=NA,nrow=CosMatrixDim, ncol=Components)
            YMatrix<-matrix(data=NA,nrow=CosMatrixDim, ncol=1)
            CosCoMatrix1<-matrix(data=NA,nrow=CosMatrixDim, ncol=thisIdxCnt)
            CosCoMatrix1[1,]<-rep(x=1,times=thisIdxCnt)         # need to be 0s for CosMatrix<-Sum...

            for (m in 1:Components){
              # calculate dTime for use in magnitude:  if all components are multiples of largest, divide one period into modelLen (outside loop)
              #       otherwise, divide full length of data hours into modelLen;  -->dT     Starttime+dT-->T
              if (oneCycle[1]>0){    # for all cases except LS
                if (LCM<=Interval){
                  dTime<-LCM/modelLen        # length of time units (hours plotted)/#dots [not for each degree of one period]
                } else {
                  dTime<-Interval/modelLen
                }

                magTime<-vector(mode="integer",length = modelLen)
                # relative to RefTime, so start at 0
                magDt<-(modelLen*dTime)              #  this is the #hours from RefTime of the last time
                magTime<-seq(from=StartSpanTime,by=dTime,to=(StartSpanTime+(magDt-dTime)))       #  changed from 0 to magDt-dTime
                #magTimeEnd<-RefTime + (magDt*3600) 
              }  # end oneCycle[1]>0
              if (Components>1){    #  if components >1, override value of cycle set above.
                cycle<-Cycles[m]
                Cycle[i,j,m]<-cycle 
                              #  this is the last time, in time format (convert hrs to sec)
              }
              
              CosCoMatrix1[m*2,]<-cos(MyData$time.hour[StartIdx:EndIdx]*(2*pi)/cycle)     # x1 for  cycle m
              CosCoMatrix1[m*2+1,]<-sin(MyData$time.hour[StartIdx:EndIdx]*(2*pi)/cycle)   # z1 for  cycle m
              YMatrix[m*2] <- sum(newData%*%(cos(MyData$time.hour[StartIdx:EndIdx]*(2*pi)/cycle)))     # Y*x1
              YMatrix[m*2+1] <- sum(newData%*%(sin(MyData$time.hour[StartIdx:EndIdx]*(2*pi)/cycle)))   # Y*z1 
            }

            # vector multiplication to build matrix for cosine
            for (m in 1:CosMatrixDim){      #  columns
              for (o in 1:CosMatrixDim){    #  rows
                  CosMatrix[o,m]<-sum(CosCoMatrix1[m,] * CosCoMatrix1[o,])
              }
            }
            CosMatrix[1,1]<-thisIdxCnt
            YMatrix[1] <- sum(newData)

            mdat<-CosMatrix
            mdat2<-YMatrix
            #invert matrix to get the solution
            if (any(is.na(mdat))) {
              Err[i,j,]<-paste(Err[i,j,],":",errKey[1])         # "*"     #  Err holds all error symbols for this element;  add this error
              print(keyMsgs[1])
              next
            } else {
              if (det(mdat)<.00000000001){
                Err[i,j,]<-paste(Err[i,j,],":",errKey[2])         # "**     #  Err holds all error symbols for this element;  add this error
                print(keyMsgs[2])
                #browser()
                next
              }  else {mdatInv <- solve(mdat)

            #multiply the inverted matrix by the dependent variable matrix to get vector M, B, G, B2, G2, ...   (x=inv(S)b)
            coefs <- mdatInv  %*%  mdat2

            if (Debug==TRUE || i==Interval){
                #print(TimeIdx)
              #print(crsprd)
              #print(s)
              #print(model$coefficients)
              #print(Amp)
              #cat("data22 ",MyData$time.hour[StartIdx:EndIdx]*(2*pi)/cycle," cycle ",cycle,"\n")
              #cat("cos22 ",cos(MyData$time.hour[StartIdx:EndIdx]*(2*pi)/cycle)," cycle ",cycle,"\n")
              #cat("StartIdx ",StartIdx,"  EndIdx ",EndIdx," thisIdxCnt ",thisIdxCnt,"\n")
                print(mdat)
                print(mdat2)
                print(mdatInv)
                cat("matrix coefs ",coefs,"\n")
                #browser()         #########   Great Debug location
            }
            #calculate MESOR only once, regardless of # of components
            M[i,j]<-coefs[1]
            #calculate the Mean of Ys
            MeanR<-mean(newData, na.rm=TRUE)
            multi_Model_mag<-multi_Model_Y<-Model_Y <- M[i,j]          # Model_Y = Mesor +  bX + gZ 

            for (m in 1:Components){
              beta<-m*2
              gamma<-m*2+1
              if (Components>1){    #  if components >1, override value of cycle set above.
                cycle<-Cycles[m]
                Cycle[i,j,m]<-cycle
              }
                #sometimes coefs will be undefined.  skip calculations and move to next cycle
                if (!is.na(coefs[beta]) && !is.na(coefs[gamma])){
                    #Calculate Amplitude using B and G   (coefs[beta]=Beta)
                    A[i,j,m]<-(coefs[beta]^2+coefs[gamma]^2)^0.5
                    if (Debug){
                      cat("A",A[i,j,m],"max",A[maxAmp,j,m],"m",m,"maxAmp",maxAmp,"\n")
                    }
                    #preserve the index for the cycle with the maximum amplitude  
                    # if (is.na(A[maxAmp,j,m])){   #  removed 10/30/17 #this does not capture the maxAmp for multiple components properly
                    #   maxAmp<-i
                    #   } else 
                    if (A[i,j,m]>A[maxAmp,j,m]) {
                        max3Amp<-max2Amp     # getting top 3 amplitudes
                        max2Amp<-maxAmp
                        maxAmp<-i
                        } else if (A[i,j,m]>A[max2Amp,j,m]) { #  this does not capture the maxAmp for multiple components properly
                          max3Amp<-max2Amp     # getting top 3 amplitudes
                          max2Amp<-i
                          } else if (A[i,j,m]>A[max3Amp,j,m]) {
                            max3Amp<-i     # getting top 3 amplitudes
                          } else if (i==2) {   # setting max2Amp and max3Amp to something other than 1 so they can be compared.
                            if (max2Amp==1) max2Amp <-2
                          } else if (i==3) {
                            if (max3Amp==1) max3Amp <-3
                          }

                      # Calculate Model:  Y(t) = M + bX + gZ    where X=cos(t2pi/cycle) and Z = sin(t2pi/cycle)
                       Model_Y = M[i,j] + (coefs[beta]*cos(2*pi*MyData$time.hour[StartIdx:EndIdx]/cycle)) + (coefs[gamma]*sin(2*pi*MyData$time.hour[StartIdx:EndIdx]/cycle))
                      if (oneCycle[1]>0){  
                        #Calculate model of 1 period of longest period for plot:  Y(t) = M + bX + gZ + e(t)where  b = Acosf  and  g = -Asinfand  X = cos(2pt/t)  and Z = sin(2pt/t)
                        Model_Y_mag[i,] = M[i,j] + (coefs[beta]*cos(2*pi*magTime/cycle)) + (coefs[gamma]*sin(2*pi*magTime/cycle))
                      }
                      if (Components>1){      # multiple components model 
                        multi_Model_Y = multi_Model_Y + (coefs[beta]*cos(2*pi*MyData$time.hour[StartIdx:EndIdx]/cycle)) + (coefs[gamma]*sin(2*pi*MyData$time.hour[StartIdx:EndIdx]/cycle))  

                        #Calculate model of 1 period for magnitude:  Y(t) = M + bX + gZ + e(t)where  b = Acosf  and  g = -Asinfand  X = cos(2pt/t)  and Z = sin(2pt/t)
                        multi_Model_mag = multi_Model_mag + coefs[beta]*cos(2*pi*magTime/Cycles[m]) + coefs[gamma]*sin(2*pi*magTime/Cycles[m])
                      }

                     MYhat_Ymean<- Model_Y-MeanR     #   calculates MSS for single component model
                     MSS[m]<-sum(MYhat_Ymean^2)        #   calculates MSS for single component model
                      #  new PR 
                    yTSS<-newData-MeanR       #  vector Y-Ybar for each Y
                    TSS_2<-sum((yTSS)^2)       #   sum of Y-Ybar^2
                    #beta1   *  sum[(Y-Ybar)x1]  * gamma1  *  sum[(Y-Ybar)z1] /TSS^2
                    #beta2   *  sum[(Y-Ybar)x2]  * gamma2  *  sum[(Y-Ybar)z2] /TSS^2  (one for each component)
                     newPR[i,j,m]<-100*(coefs[beta]*sum(yTSS*cos(2*pi*MyData$time.hour[StartIdx:EndIdx]/cycle)) + coefs[gamma]*sum(yTSS*sin(2*pi*MyData$time.hour[StartIdx:EndIdx]/cycle)))/TSS_2        #  my Y-Ymean*x1*coef
                    if (newPR[i,j,m]<0){
                      Err[i,j,m]<-paste(Err[i,j,m],":",errKey[3])          #  +
                      print(keyMsgs[3])
                      #next            should do all calculations as possible;  multiple component PR doesn't get put into PR
                    }
                    #calculate phi:  atan(-G/B) + Kpi
                    ph<-atan(-coefs[gamma]/coefs[beta])
                    if (coefs[beta]>=0) {phi2<-ph}
                    if (coefs[beta]<0&coefs[gamma]>=0) {phi2<-ph+pi}
                    if (coefs[beta]<0&coefs[gamma]<0) {phi2<-ph-pi}
                    # put in 0 to 2pi range
                    PHIr[i,j,m]<-phi2
                    if (phi2<0) { PHIr[i,j,m]<-phi2+(2*pi)} 
                    if (phi2>(2*pi)) { PHIr[i,j,m]<-phi2-(2*pi)}
                    # convert to degrees betwen 0 and 360
                    PHI[i,j,m] <- (PHIr[i,j,m]/(2*pi))*360
                    if (PHI[i,j,m]>0) {PHI[i,j,m]<-PHI[i,j,m]-360}
                    
                } else {  #  end only-if coefs can be calculated  -- this error not needed
                        Err[i,j,m]<-paste(Err[i,j,m],":",errKey[4])   #  ++
                        print(keyMsgs[4])
                        next
                }    #  end only-if coefs can be calculated
            }   # end coef calculation for each component

            # sum(Y-meanY)^2 = sum(Y - Yhat)^2 + sum(Yhat - meanY)^2
            if (Components>1){      # multiple components model 
               Model_Y <- multi_Model_Y       #  use the full (data derived) model for calculations from here on
              # calculate MSS for the full model
              MYhat_Ymean<- Model_Y-MeanR
              multi_MSS<-sum(MYhat_Ymean^2)
            }
            MY_Yhat <- newData- Model_Y     # my Y-Yhat
            RSS<-sum(MY_Yhat^2)
            if (RSS==0) {
              print(keyMsgs[6])
              Err[i,j,]<-paste(Err[i,j,],":",errKey[6])         # "@@"
              next
            }
            df1<-Components*2
            df2<-thisIdxCnt-CosMatrixDim                      # N-2*Components+1
            if (df2<0){
              print(paste(keyMsgs[9],"2*Components+1=",CosMatrixDim,"thisIdxCnt=",thisIdxCnt))
              Err[i,j,]<-paste(Err[i,j,],":",errKey[9],"thisIdxCnt=",thisIdxCnt)   # "*@@"
              next
            }else if (df2<1){
              print(paste(keyMsgs[10],"2*Components+1=",CosMatrixDim,"thisIdxCnt=",thisIdxCnt))
              Err[i,j,]<-paste(Err[i,j,],":",errKey[10],"thisIdxCnt=",thisIdxCnt)   # "*@@"
              df2<-NA   #  this will cause se's and F and P to also be NA
            }

            if (Components>1){      # multiple components model 
              multi_F<-(multi_MSS/df1)/(RSS/(df2))       #  df1 = 2, since MSS is for one component;  df2 is for full model
              multi_P[i,j]<-1-pf(multi_F,df1=df1, df2=df2)              # Fisher-Snedecor (F ) (X2 ) 
              multi_PR[i,j]<-(multi_MSS/(RSS+multi_MSS))*100
              if (Debug==TRUE){
                cat("aa  F-multi:",multi_F," df1 ",df1," df2 ",df2," multi_P ",multi_P[i,j],"\n")
              }
            } 
           
            SigmaHat<-(RSS/(df2))^.5
            if (Debug==TRUE){
              cat("bb ", thisIdxCnt," sHat ",SigmaHat,"PR\t",PR[i,j,m],"\tF\t",F[i,j,m],"\tP\t ",P[i,j,m],"\t ")
            }
            mesor_se[i,j] <-SigmaHat * mdatInv[1,1]^.5

            #   for testing alternate formula for individual P
            for (m in 1:Components){
              ## F[i,j,m]<-(MSS[m]/2)/(RSS/(df2))       #  df1 = 2, since MSS is for one component;  df2 is for full model
              ## P[i,j,m]<-1-pf(F[i,j,m],df1=2, df2=df2)              # Fisher-Snedecor (F ) (X2 )
              ## if (Debug==TRUE){
              ##   cat("cc  F:df1,df2 ",F[i,j,m],": ",2,", ",df2," MSS ",MSS[m],"RSS",RSS,"\n")
              ## }
              # corrected formula for P;  this is validated 6/7/2017 for EACH component's F, P when multiple component model
              tInd1<-2*m
              tInd2<-2*m+1
              beta<-2*m
              gamma<-2*m+1

              newF1<-(mdatInv[tInd2,tInd2]*coefs[beta]^2-2*mdatInv[tInd1,tInd2]*coefs[beta]*coefs[gamma]+mdatInv[tInd1,tInd1]*coefs[gamma]^2)/(mdatInv[tInd1,tInd1]*mdatInv[tInd2,tInd2]-mdatInv[tInd1,tInd2]^2)
              F[i,j,m]<- (newF1/2)/(RSS/df2)
              P[i,j,m]<-1-pf(F[i,j,m],df1=2, df2=df2)
            
              #   Pger  -- excludes diagonals of inverse matrix, which may be important
              # test<-(mdatInv[tInd2,tInd2]*coefs[beta]^2)+2*mdatInv[tInd1,tInd2]*coefs[beta]*coefs[gamma]+mdatInv[tInd1,tInd1]*coefs[gamma]^2
              # test2<-test/(mdatInv[tInd1,tInd1]*mdatInv[tInd2,tInd2]-mdatInv[tInd1,tInd2]^2)
              # test3<-test2/(2*SigmaHat^2)
              # testPg[i,j,m]<-1-pf(test3,df1=2, df2=df2) 
              if (Debug==TRUE){
                cat("cc2  F:df1,df2 ",F[i,j,m],": ",2,", ",df2," P ",P[i,j,m],"\n")
                cat("pre  tInd1",tInd1," mdatInv[tInd2,tInd2] ",mdatInv[tInd2,tInd2],"  coefs[beta] ",coefs[beta]," mdatInv[tInd1,tInd2] ",mdatInv[tInd1,tInd2]," coefs[gamma] ",coefs[gamma]," mdatInv[tInd1,tInd1] ",mdatInv[tInd1,tInd1],"\n")
                #cat("cc  F:df1,df2",test3,": ",2,", ",df2," test2 ",test2," test ",test,"\n")
              }

              #  Pbin  -- alternative calculation for F    p 409 Bingham [33] 
              # testXmean<-(CosMatrix[1,tInd1])/thisIdxCnt    #  X mean
              # testZmean<-(CosMatrix[1,tInd2])/thisIdxCnt    #  Z mean
              # testX2<-sum((CosCoMatrix1[tInd1,] * CosCoMatrix1[1,]-testXmean)^2)   #  X 
              # testZ2<-sum((CosCoMatrix1[tInd2,] * CosCoMatrix1[1,]-testZmean)^2)   #  Z 
              # testXZ<-sum((CosCoMatrix1[tInd1,] * CosCoMatrix1[1,]-testXmean)*(CosCoMatrix1[tInd2,] * CosCoMatrix1[1,]-testZmean))   #  XZ 
              # testF<-(testX2 * coefs[beta]^2 + 2*testXZ * coefs[beta] * coefs[gamma] + testZ2 * coefs[gamma]^2)/(2*SigmaHat^2)
              # testPb[i,j,m]<-1-pf(testF,df1=2, df2=df2)

              if (Components>1){      # multiple components model 
                PR[i,j,m]<-(MSS[m]/(RSS+multi_MSS))*100
                } else {
                PR[i,j,m]<-(MSS[m]/(RSS+MSS[m]))*100
              }

              #calculate standard error for mesor, amplitude, phi -- convert se for phi to degrees
              amp_se[i,j,m] <-SigmaHat * (mdatInv[beta,beta] * cos(PHIr[i,j,m])^2 - (2 * mdatInv[beta,gamma] * cos(PHIr[i,j,m]) * sin(PHIr[i,j,m])) + mdatInv[gamma,gamma] * sin(PHIr[i,j,m])^2)^.5
              phi_se[i,j,m] <- (SigmaHat * (mdatInv[beta,beta] * sin(PHIr[i,j,m])^2 + (2 * mdatInv[beta,gamma] * cos(PHIr[i,j,m]) * sin(PHIr[i,j,m])) + mdatInv[gamma,gamma] * cos(PHIr[i,j,m])^2)^.5)/A[i,j,m]
              phi_se[i,j,m]<-phi_se[i,j,m]*180/pi       # Convert to degrees
              
              if (Debug==TRUE){
                cat(" seM ",mesor_se[i,j]," seA ",amp_se[i,j,m]," sePHI ",phi_se[i,j,m]," \n")
                #cat("test3",m,"   ",test3,"\n")
                #cat("testPg",m,"   ",testPg[i,j,m],"\n")
                #cat("testF",m,"   ",testF,"\n")
                #cat("testPb",m,"   ",testPb[i,j,m],"\n")
                cat(thisIdxCnt,"\t","sigma ",SigmaHat,"\t",PR[i,j,m],"\t",F[i,j,m],"\t ",P[i,j,m],"\t \n")
                cat(StartSpans[j],"\t",i,"\t",format(Cycle[i,j,m],width=3), "\t ",M[i,j],"\t", format(mesor_se[i,j],digits=8),"\t",A[i,j,m],"\t",format(amp_se[i,j,m],digits=8),"\t",format(PHI[i,j,m],digits=8),"\t",format(phi_se[i,j,m],digits=8),"\n")
              }
              if (oneCycle[1]>0){   # print this cycle result if  oneCycle parameter set is>0
                #ht<-hdrRow+.5-(i+(j*htVar))+jht-(m-1)
                ht<-hdrRow+.5-(i+(j*htVar))+jht-(m-1)

                #  prints period and estimates for each period in a range
                text(c(-.4,1.2,2,3,3.9,5.1,6.2,7.2,8.2,9.2),c(ht,ht,ht,ht,ht,ht,ht,ht,ht,ht,ht),labels=c(paste(StartSpans[j]," - ",format(StartSpans[j]+Interval-1,nsmall=3)),format(Cycle[i,j,m],nsmall=2),format(P[i,j,m],digits=3,nsmall=3),format(newPR[i,j,m],digits=5),format(M[i,j],digits=8), format(mesor_se[i,j],digits=6),format(A[i,j,m],digits=6),format(amp_se[i,j,m],digits=6),format(PHI[i,j,m],digits=6),format(phi_se[i,j,m],digits=6)),cex=(cexVar*1.2),adj = c(0,0))
              }   #   format(F[i,j,m],digits=5),   removed 
            }  # end  Components loop

          
            }  #  end check for matrix validity
            }   # end matrix cannot be inverted
          }    #  end i=Loops for loop

          # Calculate range from 0 to max value                 #  calculate x axis ticks for plotting degrees on the X-axis for each model point
          if (Interval_hours<48)  {
            by.x<-2
          } else if (Interval_hours<200){
            by.x<-4
          } else if (Interval_hours<400){
            by.x<-8
          } else if (Interval_hours<800){
            by.x<-12
          } else if (Interval_hours<1000){
            by.x<-16
          } else if (Interval_hours<1200){
            by.x<-20
          } else by.x<-40

          if (oneCycle[1]==0){   # print cycle results here at end if  oneCycle parameter set 0  (spectrum)
            if (j<=28){    #  change calculation when you move to 3rd page (j=29)
              jMult<-j
            } else {
              jMult<-(j+3)%%16 + 12.5      # 13  32%%16=0  11.25  11.75   %16
            }

            ht<-hdrRow-(jMult*htVar)+jht #- (j/10)       #8    5, 10, 11, 20, 15, 25
            # prints only the max for each spectrum of frequencies
            text(c(-.4,1.2,2,3,3.9,5.1,6.2,7.2,8.2,9.2),c(ht,ht,ht,ht,ht,ht,ht,ht,ht,ht,ht),labels=c(paste(StartSpans[j]," - ",format(StartSpans[j]+Interval-1,nsmall=3)),format(Cycle[maxAmp,j,Components],nsmall=2),format(P[maxAmp,j,Components],digits=3,nsmall=3),format(newPR[maxAmp,j,Components],digits=5),format(M[maxAmp,j],digits=6), format(mesor_se[maxAmp,j],digits=6),format(A[maxAmp,j,Components],digits=6),format(amp_se[maxAmp,j,Components],digits=6),format(PHI[maxAmp,j,Components],digits=6),format(phi_se[maxAmp,j,Components],digits=6)),cex=cexVar,adj = c(0,0))
            ht<-hdrRow-.2-(jMult*htVar)+jht #- (j/10)       #8    5, 10, 11, 20, 15, 25   .3
            # prints 2nd max for each spectrum of frequencies
            text(c(-.4,1.2,2,3,3.9,5.1,6.2,7.2,8.2,9.2),c(ht,ht,ht,ht,ht,ht,ht,ht,ht,ht,ht),labels=c(paste(StartSpans[j]," - ",format(StartSpans[j]+Interval-1,nsmall=3)),format(Cycle[max2Amp,j,Components],nsmall=2),format(P[max2Amp,j,Components],digits=3,nsmall=3),format(newPR[max2Amp,j,Components],digits=5),format(M[max2Amp,j],digits=6), format(mesor_se[max2Amp,j],digits=6),format(A[max2Amp,j,Components],digits=6),format(amp_se[max2Amp,j,Components],digits=6),format(PHI[max2Amp,j,Components],digits=6),format(phi_se[max2Amp,j,Components],digits=6)),cex=cexVar,adj = c(0,0))
            ht<-hdrRow-.4-(jMult*htVar)+jht #- (j/10)       #8    5, 10, 11, 20, 15, 25    .6
            # prints 3rd max for each spectrum of frequencies
            text(c(-.4,1.2,2,3,3.9,5.1,6.2,7.2,8.2,9.2),c(ht,ht,ht,ht,ht,ht,ht,ht,ht,ht,ht),labels=c(paste(StartSpans[j]," - ",format(StartSpans[j]+Interval-1,nsmall=3)),format(Cycle[max3Amp,j,Components],nsmall=2),format(P[max3Amp,j,Components],digits=3,nsmall=3),format(newPR[max3Amp,j,Components],digits=5),format(M[max3Amp,j],digits=6), format(mesor_se[max3Amp,j],digits=6),format(A[max3Amp,j,Components],digits=6),format(amp_se[max3Amp,j,Components],digits=6),format(PHI[max3Amp,j,Components],digits=6),format(phi_se[max3Amp,j,Components],digits=6)),cex=cexVar,adj = c(0,0))
          }   else {   #  end oneCycle==0  LS

          if (yLabel==""){
              printYlab<-names(MyData)[yy]    #  yloop change to Ycol  Not OriginalYCol, but yy ()
              if (header==FALSE){
                printYlab<-paste("Column",OriginalYcol)
              }
            } else {
              printYlab<-yLabel
            }
            # calculate offset from reference time to start of data (when not the same) for the first span
            #plotOffset<-0
            plotModel<-Model_Y_mag
            plotStart<-StartSpanTime*3600      #  start time of analysis range (RangeStartTime)
            plotEnd<-EndSpanTime*3600          #  end time of analysis range (RangeEndTime)
            plotTitle<-"Single-component model:"
            cosPalette<-"blue"             # cosPalette<-"#FF9999"
            cycleCnt<-length(oneCycle)

            if (Components>1){
              plotModel<-multi_Model_mag
              Magnitude[i,j]<-max(plotModel, na.rm=TRUE) - min(plotModel, na.rm=TRUE)
              cycleCnt<-1
              plotTitle<-"Multi-component model:"
            } else {
              if (Components==1){   #  not LS, not multi-, only one model for progressive
                # should skip out here if j==1 and cycleCnt>5 and i>1 (fundamental primted)  Are periods sorted so first gets printed first I hope?  LS??
                if (cycleCnt>5){
                  cycleCnt<-1}     #  do only fundamental if there are too many periods selected
                if (cycleCnt>1){
                  cosPalette<-rainbow(cycleCnt, start=.16, end=1)   #  to see colors:  sapply(rainbow(6,start=.16, end=1),color.id)
                }
                cosPalette[1]<-"blue"       # make fundamental period black
              }
            }

            if ((realLCM*.99)<=LCM && LCM<=(realLCM*1.01)){
              # calculate bathyphase, orthophase and appropriate times for them  (don't print these if i>1)  ONLY IF FULL MODEL in plotModel
              Bathyphase[i,j]<- 0-(which.min(plotModel)+((StartSpans[j]%%LCM)*360/LCM)+((StartTime%%LCM)*360/LCM))%%360      #     0-which.min(plotModel)  xxx0-(which.min(plotModel)/(Interval/LCM)*360)  
              #  get degrees from start of interval.  Add degrees from modulus of start of reftime:  StartSpans[i]/LCM
              bathyTime<- RefTime + (which.min(plotModel)*dTime*3600) 
              Orthophase[i,j]<- 0-(which.max(plotModel)+((StartSpans[j]%%LCM)*360/LCM)+((StartTime%%LCM)*360/LCM))%%360 
              orthoTime<- RefTime + (which.max(plotModel)*dTime*3600) 
              if (Debug){
                cat("ortho",Orthophase[i,j],"bathy",Bathyphase[i,j]) 
                cat("ortho",orthoTime,"bathy",bathyTime)
              }
            } else {
              # If the interval is not the same as the REAL LCM, then we do not have a full model, so max/min would not be correct for full model
              Bathyphase[i,j]<- NA
              bathyTime<- NA 
              Orthophase[i,j]<- NA
              orthoTime<- NA 
            }
            if (GraphSet$Model){     #  start Model plot
              # Plotted values use the StartSpans[j], which may include a FULL cycle span, not just the starting and ending data points
              # And also uses the start and end Ranges 
              dateRange<-paste("Data range: ",format(MyData$time[StartIdx],"%m/%d/%Y %H:%M")," to ",format(MyData$time[EndIdx],"%m/%d/%Y %H:%M"),"\nData analysis range: ",format(RefTime+plotStart,"%m/%d/%Y %H:%M")," to  ",format(RefTime+plotEnd,"%m/%d/%Y %H:%M"))
              if (Components>1 || cycleCnt==1){  
                if ((realLCM*.99)<=LCM && LCM<=(realLCM*1.01)){
                  printXlab  <-  list(bquote("Hours from Reference Time:  "*.(RefTimeString)* ";       One Model cycle = "*.(format(realLCM,digits=3,nsmall=2))* " hrs      o " * .(Orthophase[i,j]) * degree*"     b "* .(Bathyphase[i,j]) * degree))       #  magDt
                } else {
                  printXlab  <-  paste("Hours from Reference Time:  ",RefTimeString,";       One Model cycle = ",format(realLCM,digits=3,nsmall=2)) 
                }
              } else {  #  if several single components PHI cannot be printed, and there is no one model
                printXlab  <-  paste("Hours from Reference Time:  ",RefTimeString)    # magDt
              }
              # how high does y-axis need to be for model?  pmin pmax will get vector max/min of matrix
              hi_range<-max(pmax(plotModel))
              lo_range<-min(pmin(plotModel))
              y_range<-c(lo_range,hi_range) 
              CyclesLCM<-as.numeric(Interval_hours/LCM)         #  MyData_hours is # hours in analysis range (MyData_HRs is hours of actual data)
              if (j==1){
                cexLab<-1
                cexAxis<-.9
              }
              else { cexLab<-.8    #  no smaller than .78
                     cexAxis<-.7
              }
              #  if not too long, graph both model and data;  else skip this and print ONLY MODEL
              if ((Interval_hours<500 || MyData_length<500) && CyclesLCM<100){          #  j<11 is for debug, and should be removed   #Interval_hours<350 && 
                #  ??????MyData_hours should not be needed here.....not always valid;  MyData_length can make plot unreadable if too many points plotted;  
                #  MyData_hours is the analysis time frame.  Only the analysis timeframe is plotted, not full data
                #  If modelLen >360, the ration of LCM to minPeriod is more than 60/1, and the plot may be lacking visible relevant info becasue of the long LCM vs minPeriod 
                
                # how high does y-axis need to be for model and data together?  adjust to include data
                if (max(newData)>max(pmax(plotModel))){
                  hi_range<-max(pmax(newData))
                }
                if (min(newData)<min(pmin(plotModel))){
                  lo_range<-min(pmin(newData))
                } 
                y_range<-c(lo_range,hi_range)
                x_range<-c(StartSpanTime,EndSpanTime) 
                
                                 # xaxt="n",
                plot(x=MyData$time.hour[StartIdx:EndIdx],newData,type="l", lwd=1, xlab=printXlab,ylim=y_range,xlim=x_range,ylab=printYlab,main=paste(plotTitle,deparse(Period$Set),dateRange),cex.lab=cexLab,cex.axis=cexAxis,cex.main=cexMain)   # xlim=c(1,modelLen)
                
                # plot the model in hours
                waves<-round(CyclesLCM*modelLen)    #  MyData_hours= EndSpanTime-StartSpanTime
    
                for (C in 1:cycleCnt){ 
                  tempModel<-plotModel
                  if (Components==1){  
                    tempModel<-plotModel[C,]
                  }

                  if (C==1){           #  multi-component always C==1
                    par(new = TRUE)
                    plot(x=c(1:waves),y=rep(tempModel,length.out=waves),type="l",lwd=1, col=cosPalette[C], axes=FALSE, xlab = "",ylim=y_range,ylab = "",cex.lab=cexLab,cex.axis=cexAxis,cex.main=cexMain)    #bty = "n",  added cex=cexVar
                    #  "black"   "red"     "green3"  "blue"    "cyan"    "magenta" "yellow"  "gray" deepskyblue
                  } else {
                    lines(x=c(1:waves),y=rep(tempModel,length.out=waves),type="l",lwd=1,col=cosPalette[C]) 
                  }
                }
                mtext("", side=1,line=3)
    
                label.x<-seq(from=StartSpanTime, to=EndSpanTime, by=by.x)       #  these are the numbers (labels)   on the x axis
                at.x<-(1/dTime) * (label.x-StartSpanTime)               #  these are the plot points for the data n* label            

              } else {  #  if too many then print only one model
                tempModel<-plotModel
                  x_range<-c(0,modelLen)         #+StartSpanTime         # this is same units as at.x
                  for (C in 1:cycleCnt){
                    if (Components==1){  
                      tempModel<-plotModel[C,]
                    }
                    if (C==1){                    #  
                      plot(x=c(1:modelLen),y=tempModel,xlab=printXlab,ylab=printYlab,col=cosPalette[C], type="l",lwd=1,xaxt="n",main=paste(plotTitle,deparse(Period$Set),dateRange),xlim=x_range,ylim=y_range,cex.lab=cexLab,cex.axis=cexAxis,cex.sub=.5,cex.main=cexMain)
                      par(new = TRUE)
                    } else {
                      lines(x=c(1:modelLen),y=tempModel,type="l",lwd=1,col=cosPalette[C]) 
                    }
                  }
                  # Calculate range from 0 to max value                 #  calculate x axis ticks for plotting degrees on the X-axis for each model point
                  if (LCM<48)  {
                    by.xx<-2
                  } else if (LCM<200){
                    by.xx<-4
                  } else if (LCM<400){
                    by.xx<-8
                  } else if (LCM<800){
                    by.xx<-12
                  } else if (LCM<1000){
                    by.xx<-16
                  } else if (LCM<1200){
                    by.xx<-20
                  } else by.xx<-40 
                  
                  label.x<-seq(from=0, to=LCM, by=by.xx)+StartSpanTime       #  these are the numbers (labels)   on the x axis
                  at.x<-(1/dTime) * (label.x  -StartSpanTime)               #  these are the plot points for the data n* label
                  
                  # Make x axis with horizontal labels that display ticks at 
                  # every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12....EndIdx).
                  axis(side=1, at=at.x, labels = formatC(label.x, format = "fg"),cex.lab=cexLab,cex.axis=cexAxis,cex.main=cexMain)                    #  las=1,4*0:g_range[2]
                #}
              }   #  end model only

              ht<-6-(j*htVar)+jht
              #restore palette in case it was changed
              palette("default")
              if (cycleCnt>1){
                 legend(x="topleft",inset=c(.15,.07),legend=oneCycle,fill=cosPalette, title="Periods")
              }
              if (Progression_end!=1 && j==1){        #   if not a progression, but first time through, new page.
                layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
                       widths=c(1), heights=c(1,1))          #   c(figure#1, figure#2), 2 row, 1 col, ...  
              }
                # end Model plot
          }  else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area  instead of model
        } # end NOT LS
      }    #   END for (j in 1:Progression_end){  ,seq(from=magTime[1],by=dTime,to=tail(magTime,1))

    #  build title   (is this used or line 855?  11/29/2017)
    startDateP<-as.POSIXct(strptime(x=StartDate, format="%Y%m%d%H%M", tz=tz))
    endDateP<-as.POSIXct(strptime(x=EndDate, format="%Y%m%d%H%M", tz=tz))
    yTitle<-OriginalYcol   #  yloop change all to Ycol

    if (header==TRUE){
      if (length(names(myHeader))==0){
        yTitle<-names(myHeader)[OriginalYcol]
      } 
    }

    titleMain<-paste('Column:',yTitle,';   ',format(startDateP, "%Y-%m-%d %H:%M"),"--",format(endDateP, "%Y-%m-%d %H:%M"))       # graph title
    m<-Components

    if ((nrow(A)>1 || m>1) & ncol(A)>1 & GraphSet$HeatMap & any(GraphSet)){      #  
      par(mfrow=c(1,1),mar=c(4,4,8,4),oma=c(1,1,1,1))   #  oma=c(2,1,1,1)
      if (m==1){
        mapx <- 1/Cycle[,1,1:m]                ###F    m is current component      rev(Cycle[1:(i),1,m]) 
        mapx2 <- c(1:RowCnt)
        ampz <- A[1:i,,m] 
        #ampz[1,]<-1    #    why do I have to put fake data in here???? 
        #ampz[i,]<-1   #     map is all light otherwise because 1st and last are bigger
      } else {
        mapx <- 1/Cycle[,1,1:m]
        mapx2 <- c(1:m) 
        ampz <- t(A[i,,1:m])
      }

      mapy <- StartSpans+StartTime+(Interval/2)      #  10*(1:ncol(A))  Adds Interval/2 so ticks are in middle of Interval

      tickCnt<-format(seq(from=StartSpans[1]+StartTime+(Interval/2), to=MyData_hours+StartTime-(Interval/2),by=24),digits=4)
      if (Period$Set[1]==0){                      #  Set=0 means spectrum;
        #  y axis label
        HeatYlab<-paste('Period (hours): ',Period$Start,'/1, ',Period$Start, '/',1+FreqInc,' . . . to ',Period$Start,'/',format((1+((i-1)*FreqInc)),digits=4,nsmall=2),'=',format(cycle,digits=4,nsmall=2),", ","Harmonic increment:",FreqInc)
        HeatYlab2<-paste('Frequency (cycles/',Period$Start,'hour):  1, ',FreqInc+1,', . . . to ',format(RowCnt,digits=4,nsmall=1),", ","Harmonic increment:",FreqInc)
      }else {HeatYlab<-paste('Period (hours):  (At',Period["Set"],"periods)")
             HeatYlab2<-paste('Frequency (cycles/',Period$Start,'hour):  (At',Period["Set"],"periods)")
             mapx<-sort(mapx, decreasing=FALSE)    #  Set<>0, periods need to be sorted for proper graphing
      }

      lvlCnt<-10        # number of colors to use
      colCount<-100    #**  19 for hanning filter;  9 w/o filter
      Nscale<-((0:colCount)/colCount)
      Nbw<-gray(Nscale)
      HeatMain<-titleMain      # graph title
      HeatXlab<-paste('Time (hours from reference time, ',RefTime,')\n','Interval length: ',Interval,Units,'   Increment: ',Increment,Units)     #  x axis label
      HeatLegend<-Nscale*max(ampz, na.rm=TRUE)      # A[2:(i-1),1,m]  use the number of levels needed, multiply by max, to get character labels for legend
      HeatLegend2<-c(HeatLegend,max(ampz, na.rm=TRUE)+max(A[2:(i-1),1,m], na.rm=TRUE)/colCount)    # this is only needed for image breaks=HeatLegend2, where breaks needs to have one more element than legend
      HeatIndex<-rep(1:(colCount/lvlCnt),lvlCnt)
      HeatLegend3<-(HeatLegend2[HeatIndex==1])
      NbwFill<-(Nbw[HeatIndex==1])

      if (Colors=="BW"){    #  begin Colors
        colorVar<-rev(Nbw)
        fillVar<-rev(NbwFill)
      }else {if (Colors=="Heat"){   
        colorVar<-rev(heat.colors(colCount))
        fillVar<-rev(heat.colors(lvlCnt+1))
      } else {colorVar<-rev(terrain.colors(colCount))
              fillVar<-rev(terrain.colors(lvlCnt+1))
             }   
      }#   end Color

      image(mapy, mapx, t(ampz), axes = FALSE, col=colorVar,main=HeatMain,ylab=HeatYlab,xlab=HeatXlab) #format(max(A, na.rm=TRUE)+max(A, na.rm=TRUE)*.2,digits=4)))      ###F    fill=rev(Nbw)
      #col controls the colors used in graph;  breaks controls the range of values for each color
      par(xpd=TRUE)  # Do not clip to the drawing area
      lambda <- .025
      #  legend controls the location, numbers and colors shown on the legend.  No relationship to actual unless same is used.    changed:   par("usr")[4]-par("usr")[3])*.5
      legend(par("usr")[2],(par("usr")[4]-par("usr")[3])*.5,xjust = .5, yjust = -3,lwd=3, lty=1, title="Amplitude", legend=format(HeatLegend3,digits=4),fill=fillVar,text.width = strwidth("1.00"),cex=.5)                
      #x axis corresponds to row number and the y axis to column number, with column 1 at the bottom, 
      #i.e. a 90 degree counter-clockwise rotation of the conventional printed layout of a matrix.
      axis(1, at = tickCnt)  
      axis(2, at = mapx,labels=format(1/mapx,digits=4) )      # 1/Cycle[1:(i),1]  

      rightAxis<-c(1:RowCnt)
      axis(4, at = mapx,labels=format(mapx2,digits=4))    #  cycles
      box()

    }  #else {  #  if a heatmap is built, too many periods for output
      
   if (length(which(GraphSet==T))>0 ) {    # if something set to be TRUE
      if (any(GraphSet)){           #  (m>1 || i>1) &&   GraphSet   Data MESOR Amp Phi NPts
        
        #layoutSize<-length(which(GraphSet==T))+1    #  need 2 more than number of graphs
        if (j>1){
          layoutSize <- 8
        } else layoutSize <- 8   #7   change to 8 fixes the green text moving to the next line CallCosCg1Multi1 defaults.  where is 8 needed?? 
        layout(matrix(c(1:layoutSize), layoutSize, 1, byrow = TRUE), 
               widths=c(1), heights=c(.5,rep(x=2,times=layoutSize-4),1,.5,.5))          # c(figure#1, figure#2), 2 row, 1 col, ....5,2,2,2,[2],1,.5,.5
        par(mar=c(2,4,1,1), oma=c(2,0,1,0))         # set up margins

        if (FreqInc !=1){    #  display increment on y axis if other than 1
          Ylable_inc<- paste("; harmonic increment:",FreqInc,"cycles")       
        } else {Ylable_inc <-" "}

        if (j>1) {              #  J is time Interval(s) -- progression through Interval start to ends  
          for (l in m){             # only do where single component(m=1) or this is last multi-component (l=m)      for (l in 1:m){
            for (k in 1:Loops){   #  i/k is frequency calculations for each time Interval
              # all cycles repeated, so using only list of cycles from first loop, if more than one loop
              # when there is a progressive on a multi-component, the multi-components are shown, not the progressive (should be only 1 raw data/MESOR)
              # J's should be tested -- need to print sections in $Data
              #  may want a dividor, or put raw data and MESOR on a different page, to make it more clear that raw data and Mesor are for all, others are for component
              #  perhaps the top title "Column...." right below, could be AFTER the MESOR/data when not a progressive?
              cat(k,j,l,"\n")

              lineX<-StartSpans + StartTime + (Interval/2)      #   1:j   calculate halfway points for graphing each Interval
              if ((length(which(GraphSet==T))==1 && GraphSet$Data && l==1 && k==1) || length(which(GraphSet==T))>2) {
              # Calculate range from 0 to max value 
              if (m==1){      #    plotting when m==1, plot spans of progressive cosinor 
                mainX<-paste("Column ",OriginalYcol,";  Period",Cycle[k,j,l],";   Time (",Units,") from reference date: ",RefTime)
              } else {      #     plotting when m>1 plots periods of each component
                mainX<-paste("Column ",OriginalYcol,";  Components",list(Cycle[,j,]),":   Time (",Units,") from reference date: ",RefTime)   #   Period$Set[l], = Cycle
              }

            if (GraphSet$Data && (l==1 || l==m) && k==1){    #2   if more than 1 component, do not print raw data again
                plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # 1 blank area
                plot(MyData$time.hour[1:EndIdx],MyData[1:EndIdx,Ycol],type="l",xaxt="n",ylab=paste("data"),main=mainX)   #  should not use newData
                g_range <- range(MyData$time.hour[1:EndIdx])
                at.x<-seq(from=round(g_range[1]), to=round(g_range[2]), by=by.x)
                # Make x axis with horizontal labels that display ticks at 
                # every LCM marks. .
                axis(side=1, at=at.x, labels = formatC(at.x, format = "fg"),cex.sub=cexMain,cex.main=cexMain)                   
                
                } else {plot(1:10,1:10,type="n",ylab="",axes=FALSE,main=mainX,cex.sub=cexMain,cex.main=cexMain)   # blank area
                        plot(1:10,1:10,type="n",ylab="",axes=FALSE)}   # blank area

              g_range <- range(lineX)

              if (m==1){      #  only use for single components
                if (GraphSet$MESOR==T && !all(is.na(M[k,])) && l==1){   # 3 if all NA, which.max will fail;  if more than 1 component, do not print MESOR again
                  plot(lineX,M[k,],type="l",xaxt="n",ylab="MESOR",col=8)     # each Interval has a different MESOR
                  # Make x axis with horizontal labels that display ticks at 
                  # every LCM marks. .
                  axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
                } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
    
                if (GraphSet$Amp && !all(is.na(A[k,,l]))){   # 4 if all NA, which.max will fail
                  ampMax<-which.max(A[k,,l])
                  ampMin<-which.min(A[k,,l])
                  seMax<-which.max(amp_se[k,,l])
                  seMin<-which.min(amp_se[k,,l])
                  plot(lineX,A[k,,l],type="l",xaxt="n",ylab="Amplitude",col=4,ylim=c((A[k,ampMin,l]-amp_se[k,seMin,l]-1),(A[k,ampMax,l]+amp_se[k,seMax,l]+1)))
                  points(lineX,(A[k,,l]+amp_se[k,,l]),type="p",pch=20,cex=.1,col=3)       # ,ylim=c(-360,0)
                  points(lineX,(A[k,,l]-amp_se[k,,l]),type="p",pch=20,cex=.1,col=3)
                  # Make x axis with horizontal labels that display ticks at 
                  # every LCM marks. .
                  axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
                } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
    
                if (GraphSet$Phi  && !all(is.na(PHI[k,,l]))){   # 5 if all NA, which.max will fail
                  plot(lineX,PHI[k,,l],type="l",xaxt="n",ylab="Acrophase",col=6,ylim=c(0,-360))
    
                  points(lineX,(PHI[k,,l]+phi_se[k,,l]),type="p",pch=20,cex=.2,col=3)   
                  points(lineX,(PHI[k,,l]-phi_se[k,,l]),type="p",pch=20,cex=.2,col=3) 
                  # Make x axis with horizontal labels that display ticks at 
                  # every LCM marks. .
                  axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
                } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
                #  end m=1
              }  else { #  if m!=1 then this is the full component model, and we need different graphs
               if (GraphSet$Amp && !all(is.na(Magnitude[k,]))){   # 4 if all NA, which.max will fail

                  plot(lineX,Magnitude[k,],type="l",xaxt="n",ylab="Magnitude",col=4)

                  # Make x axis with horizontal labels that display ticks at 
                  # every LCM marks. .
                  axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
                } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
                
                if (GraphSet$Phi  && !all(is.na(Orthophase[k,]))){   # 5 if all NA, which.max will fail
                  plot(lineX,Orthophase[k,],type="l",xaxt="n",ylab="Orthophase",col=6,ylim=c(0,-360))
                  
                  # Make x axis with horizontal labels that display ticks at 
                  # every LCM marks. .
                  axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
                } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area

                if (GraphSet$Phi  && !all(is.na(Bathyphase[k,]))){   # 5 if all NA, which.max will fail
                  plot(lineX,Bathyphase[k,],type="l",xaxt="n",ylab="Bathyphase",col=6,ylim=c(0,-360))
                  
                  # Make x axis with horizontal labels that display ticks at 
                  # every LCM marks. .
                  axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
                } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
              }     #  end m!=1

              if (GraphSet$P){   # 6 for serial section only
                plot(lineX,P[k,,l],type="l",xaxt="n",ylab="P Value",col=9,ylim=c(0,.5))
                abline(h=.05,lty="dashed")
                # Make x axis with horizontal labels that display ticks at 
                # every LCM marks. .
                axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
              } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
              
              if (GraphSet$NPts){   #7
                plot(lineX, nPts[k,],type="l",xaxt="n",ylim=c(min(nPts[k,], na.rm=TRUE)-5,max(nPts[k,], na.rm=TRUE)+5) ,ylab=paste("N: #Pts"))   # blank area
                # Make x axis with horizontal labels that display ticks at 
                # every LCM marks. .
                axis(side=1, at=at.x)        #, labels = formatC(label.x, format = "fg")) 
              } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area

              if (m==1){
                plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # 8 blank area
                mtext("Green dots = S.E. standard error",side=1,line = 2, cex =.8, adj=.7,col=3)
              }
              #  if progressive, plot(lineX, X[k,]);   otherwise, plot(1:i,X[k,])  except variable always w/time
              # http://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_addat.html
              } # end datagraph only for some cases.
            }       # end K to Loops
          }   # end L to m
        }  else {    # && !any(errKey %in% Err)       # end if j>1, begin j==1,   more than 1 period, end if inversion error

          layout(matrix(c(1:layoutSize), layoutSize, 1, byrow = TRUE), 
                 widths=c(1), heights=c(1,rep(2,layoutSize-2),1))          #   c(figure#1, figure#2), 2 row, 1 col, ...
 
          #  j==1 in several cases:  >1 period single cosinor or multiple cosiner;  progressive;  range progressive
          #  perhaps spectrum could be line graph, and inc<1 could be bar graphs
          #  if Inc<1, this section may be dropped out per GCG:  should not graph these -- may show bar graphs of Amp only
          CycleLen<-length(Cycle)
          cat("cycle",CycleLen)
          if (CycleLen>6){    #   if Cycle = 0 there is a long sequence, otherwise only a few
            titleLable2<-paste ("Periods:",format(Cycle[i,1,],digits=4,nsmall=2),' to ',format(Cycle[1,1,],digits=4,nsmall=2),Units, Ylable_inc)
            } else {
            titleLable2<- paste("Periods:",list(Cycle[,1,]),Units, Ylable_inc)
          }
          plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # 1 blank area

          if (GraphSet$Data){   #2, 3 cond
            plot(MyData$time.hour[StartIdx:EndIdx],newData,type="l",xaxt="n",ylab=paste("data*",window),main=titleMain)
            g_range <- range(MyData$time.hour[1:EndIdx])
            at.x<-seq(from=round(g_range[1]), to=round(g_range[2]), by=by.x)
            # Make x axis with horizontal labels that display ticks at 
            # every LCM marks. .
            axis(side=1, at=at.x, labels = formatC(at.x, format = "fg"))    
            plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # EXTRA blank area
            text(x=5.3,y=9.9, paste("Time (",Units,") from reference date: ",RefTime),cex=1)
            text(x=5.3,y=1.1,titleLable2,cex=1)
            abline(h=5,lty="longdash")
          } else plot(1:10,1:10,type="n",ylab="",axes=FALSE,main=titleMain)   # blank area
            #}
          
          if (m>1 || i>1)  {
            if (GraphSet$MESOR==T && !all(is.na(M[CycleLen:1]))){   # 3 if all NA, min/max will fail
              plot(1/Cycle[CycleLen:1],M[CycleLen:1],type="l",ylab='MESOR2',col=8)
            } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
    
            if (GraphSet$Amp && !all(is.na(A[CycleLen:1]))){   # 4 if all NA, min/max will fail
              AmpRange<-c((A[CycleLen:1]-amp_se[CycleLen:1]),(A[CycleLen:1]+amp_se[CycleLen:1]))
              plot(1/Cycle[CycleLen:1],A[CycleLen:1],type="l",ylab="Amplitude",col=4,ylim=c(min(AmpRange, na.rm=TRUE),max(AmpRange, na.rm=TRUE)))    #,ylim=c(0,(A[]+amp_se[])))
              points(1/Cycle[CycleLen:1],(A[CycleLen:1]+amp_se[CycleLen:1]),type="p",pch=20,cex=.6,col=3)   #,ylim=c(-360,0))   
              points(1/Cycle[CycleLen:1],(A[CycleLen:1]-amp_se[CycleLen:1]),type="p",pch=20,cex=.6,col=3)   #,ylim=c(-360,0))
              #lines(A[1,]+M[1,],type="l")
            } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
    
            if (GraphSet$Phi && !all(is.na(PHI[CycleLen:1]))){   # 5 if all NA, min/max will fail
              plot(1/Cycle[CycleLen:1],PHI[CycleLen:1],type="l",ylab="Acrophase",col=6,ylim=c(0,-360))
              points(1/Cycle[CycleLen:1],(PHI[CycleLen:1]+phi_se[CycleLen:1]),type="p",pch=20,cex=.6,col=3)   
              points(1/Cycle[CycleLen:1],(PHI[CycleLen:1]-phi_se[CycleLen:1]),type="p",pch=20,cex=.6,col=3)   
              #plot(1/Cycle[CycleLen:1],PR[CycleLen:1],type="l",ylab="Percent Rhythm",col=6) 
            } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
            
            if (GraphSet$P && !all(is.na(P[CycleLen:1]))){   # 6 if all NA, min/max will fail
              plot(1/Cycle[CycleLen:1],P[CycleLen:1],type="l",ylab="P Value",col=9,ylim=c(0,.5))
              abline(h=.05,lty="dashed")
            } else plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # blank area
            
            plot(1:10,1:10,type="n",ylab="",axes=FALSE)   # 6 blank area
            mtext("Green dots = S.E. standard error",side=1,line = 2, cex =.8, adj=.7,col=3)

        }  #  end m>1 || i>1  
        } # end else if j=1
      } # end if i>1 and Graphs == T
  }  #  end LineSetGraph==T
#browser()
    if(Output$Doc || Output$Dat){
      if (y==1){
        if (Output$Doc){
          #  may need to chg to wd:  http://www.r-statistics.com/2010/05/exporting-r-output-to-ms-word-with-r2wd-an-example-session/
          rtf<-RTF(fileName3,width=11,height=8.5,omi=c(.5,.25,.5,.25), font.size=11)
          output<-gsub(pattern='\b\b',replacement='\b',fileName3)
          #if (Period$Set[1]!=0){
            #this is built earlier at 546  paramMsg<-paste("Parameters:-----------------------------\n",output,"\n  TimeCol=",TimeCol,",  Ycol=",OriginalYcol, ",  header=",header,"\n --  Periods=",Period["Set"],", Units=",Units, ",  Interval=",format(Interval,nsmall=3), ",  Increment=",format(Increment,nsmall=3), "\nRefDateTime=",RefDateTime, ", StartTime=",format(StartTime,nsmall=3),", EndTime=",format(EndTime,nsmall=3),"\n Data hrs=",format(MyData_hours,nsmall=3),"  dt=",format(dt,nsmall=3)," hours\nPercent of missing (blank) sample values: %",missingData*100,"\n",functionName,"\n-----------------------------\n\nReference Time:",RefTimeString)
            paramMsg<-paste("Parameters:-----------------------------\n",output,paramMsg,"\n-----------------------------\n\nReference Time:",RefTimeString)

            addParagraph(rtf,paramMsg)

          #errMsg<-""  
        }
        PRdigits<-2
        PMAdigits<-2
        PHIdigits<-1
        fileoutCosX <- paste(fileName,window,thisTime,functionName,"Cos.dat",sep="")
      }

      if (!is.na(P[1])){
        printP<-formatC(P,digits=4,format="f")    #  this Cformat works better when formatting an entire vector!
      }
      printP[P<.0005]<-c("<.001")
      sPtsX<-array(data=sPts,c(i,j,m))
      MX<-array(data=M,c(i,j,m))
      mesor_seX<-array(data=mesor_se,c(i,j,m))

      timeX<-array(data=time,c(i,j,m))
      yVarX<-array(data=yVar,c(i,j,m))    #  reformats yVar to 3D
      hoursX<-array(data=hours,c(i,j,m))

      if(Output$Doc){
        if (window=="noTaper"){
          addParagraph(rtf,paste("\n        Rhythmometric Summary of column ", OriginalYcol, printYlab,"-------------------------------------\n"))   #not printYlab1[y]  10/6/2017
        } else {
          addParagraph(rtf,paste("\n        Rhythmometric Summary of filtered data from column ", OriginalYcol, printYlab,"-------------------------------------\n"))   #not printYlab1[y]  10/6/2017
        }
        addParagraph(rtf,errMsg)
        addParagraph(rtf,paramMsg2)
        errMsg<-''
        # if (Progression_end==1){
        #   addParagraph(rtf,paste("\n      Time Pts=",time[,1],"     hours from RefDateTime=",hours[,1],"     #Pts",sPts[,1]," "))   #  ,"Period in hrs=",Cycle  should not print when single component, multiple individual periods
        # }
        if (Components>1){           # ***************   Multiple   ************
          for (n in 1:Progression_end){
            addParagraph(rtf,paste("\n     MESOR=",format(M[,n],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),"     MESOR s.e.=",format(mesor_se[,n],digits=2*PRdigits,nsmall=PMAdigits)))
            if (Progression_end>1){     # Multiple Progressive needs to see additional variables
              mat<-data.frame(matrix(c(Err[,n,],timeX[,n,],hoursX[,n,],sPtsX[,n,],format(Cycle[,n,],nsmall=PRdigits),format(newPR[,n,],digits=2*PRdigits,nsmall=PRdigits,scientific=FALSE),printP[,n,],format(A[,n,],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(amp_se[,n,],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(PHI[,n,],digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE),format(phi_se[,n,],digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE)),ncol=11))  #,dimnames=list(row_names,colName))
              #if (n==1){                  #  Don't need header on successive rows of progressive 8/1/2017 but cannot get rid of it
                names(mat)<-c("Err","Time Pts","hours from RefDateTime","#Pts","Period in Hrs","PR","P","Amp","Amp SE","PHI","PHI SE") # format column names
              #}
              # changed from above to below after a long time of not running multis  --  too many output values in col.widths vector
              addTable(rtf,mat,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.75,1.3,1.05,.75,.6,.7,.6,.75,.6,.6, .6))
              } else {      #  Multiple non progressive
            #  chnaged from printP 5/24/2017
              addParagraph(rtf,paste("\n      Time Pts=",time[,1],";               Hours from RefDateTime=",hours[,1],";     #Pts",sPts[,1],"Period in hrs=",Cycle," "))   #    should not print when single component, multiple individual periods
              mat<-data.frame(matrix(c(Err[,n,],format(Cycle[,n,],nsmall=PRdigits),format(newPR[,n,],digits=2*PRdigits,nsmall=PRdigits,scientific=FALSE),printP[,n,],format(A[,n,],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(amp_se[,n,],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(PHI[,n,],digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE),format(phi_se[,n,],digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE)),ncol=8))  #,dimnames=list(row_names,colName))
              names(mat)<-c("Err","Period in Hrs","PR","P","Amp","Amp SE","PHI","PHI SE") # format column names
              # changed from above to below after a long time of not running multis  --  too many output values in col.widths vector
              addTable(rtf,mat,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.75,.65,.75,.75,.7,.7,.7,.7))
          }
            printP_multi<-format(multi_P[i,n],digits=4,nsmall=PMAdigits)
            printP_multi[multi_P[i,n]<.0005]<-c("<.001")
            addParagraph(rtf,paste("\nLCM",LCM,"  Overall:   PR = ",format(multi_PR[i,n],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),"      P ",printP_multi,"       Magnitude = ",format(Magnitude[i,n],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),"        Orthophase = ",format(Orthophase[i,n],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),"        Bathyphase = ",format(Bathyphase[i,n],digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),"\n\n"))
          }   #   End progressive where Components>1
          } else {                   #  ****************  Components =1  **********
            #if (Progression_end>1){     # Progressive needs to see additional variables
             # varProg<-rbind(Err, time,hours,sPts)
              #nameProg<-c("Err","Time Pts","hours from RefDateTime","#Pts")
              colProg<-c(.75,1.3,1.05,.75,.6,.7,.6,.75,.6,.6,.6,.6,.6)
              ncolProg<-13
            #} else {
             #browser()
              #varProg<-c(Err)
              #nameProg<-c("Err","")
              #colProg<-c(.1,.6,1,.6,.6,.65,.75,.75,.7,.7,.7)
              #ncolProg<-11
            #}
            if (is.na(newPR[1])){   #  formatC doesn't handle NAs
              mat<-data.frame(matrix(c(Err, time,hours,sPts,format(Cycle,digits=2*PRdigits,nsmall=PRdigits),format(newPR,digits=4),printP,format(M,digits=2*PMAdigits),format(mesor_se,digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(A,digits=2*PMAdigits),format(amp_se,digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(PHI,digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE),format(phi_se,digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE)),ncol=13))  #,dimnames=list(row_names,colName))
            } else {
              mat<-data.frame(matrix(c(Err, time,hours,sPts,format(Cycle,digits=2*PRdigits,nsmall=PRdigits),formatC(newPR,digits=4,format="f"),printP,formatC(M,2*PMAdigits,format="f"),format(mesor_se,digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),formatC(A,digits=2*PMAdigits,format="f"),format(amp_se,digits=2*PRdigits,nsmall=PMAdigits,scientific=FALSE),format(PHI,digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE),format(phi_se,digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE)),ncol=13))  #,dimnames=list(row_names,colName))
            }
            # cannot append because it has to be the same length.  for some reason, putting header in doesn't work....
            #  commented out when doing trials of non-invertible matrix
            #mat<-data.frame(matrix(c(t(varProg),t(mat)), ncol=ncolProg, byrow=FALSE))
            names(mat)<-c("Err","Time Pts","hours from RefDateTime","#Pts","Period in hrs","PR","P","Mesor","Mesor SE","Amp","Amp SE","PHI","PHI SE") # format column names
            #names(mat)<-append(names(mat),nameProg, 1)
            addTable(rtf,mat,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.75,1.3,1.05,.75,.6,.7,.6,.75,.6,.6,.6,.6,.6))    #  c(.6,.1,.65,.75,.75,.7,.7,.7,.7)
          }
        #  summary of data
        if (window=="noTaper"){
          addParagraph(rtf,"\n\n        Data Summary")
        } else {
          addParagraph(rtf,"\n\n        Data Summary of filtered data")
        }

        if (Components>1){
            # changed 1-->n below, without testing this:  matS<-data.frame(matrix(cbind(ErrX[y,1,m],yVarX[y,1,m],timeX[y,1,m],hoursX[y,1,m],sPtsX[y,1,m],format(sumN[y,1:Progression_end],nsmall=PRdigits),format(sumLow[y,1:Progression_end],nsmall=PRdigits),format(sumHi[y,1:Progression_end],nsmall=PRdigits),format(sumMean[y,1:Progression_end],nsmall=PRdigits),format(sumMedian[y,1:Progression_end],nsmall=PRdigits),format(sumMode[y,1:Progression_end],nsmall=PRdigits),format(sumT[y,1:Progression_end],nsmall=PRdigits),format(sumSD[y,1:Progression_end],nsmall=PRdigits)),ncol=13))  #,dimnames=list(row_names,colName))
            # removed for loop because we only need one instance of each interval for the data summary for progressives with multiple components
            matS<-data.frame(matrix(c(Err[1,,m],yVarX[1,,m],timeX[1,,m],hoursX[1,,m],sPtsX[1,,m],format(sumN[y,1:Progression_end],nsmall=PRdigits),format(sumLow[y,1:Progression_end],nsmall=PRdigits),format(sumHi[y,1:Progression_end],nsmall=PRdigits),format(sumMean[y,1:Progression_end],nsmall=PRdigits),format(sumMedian[y,1:Progression_end],nsmall=PRdigits),format(sumMode[y,1:Progression_end],nsmall=PRdigits),format(sumT[y,1:Progression_end],nsmall=PRdigits),format(sumSD[y,1:Progression_end],digits=4,nsmall=PRdigits)),ncol=13))  #,dimnames=list(row_names,colName))
            names(matS)<-c("Err","Y","Time Pts","hours from RefDateTime","#Pts","N","Low","High","Mean","Median","Mode","dT","s.d.") # format column names
            addTable(rtf,matS,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.75,.3,1.3,1.05,.85,.5,.6,.6,.75,.75,.5,.5,.5))
          
        } else {    #  Components =1
            matS<-data.frame(matrix(cbind(Err[1,,m],yVarX[1,,m],timeX[1,,m],hoursX[1,,m],sPtsX[1,,m],format(sumN[y,1:Progression_end],nsmall=PRdigits),format(sumLow[y,1:Progression_end],nsmall=PRdigits),format(sumHi[y,1:Progression_end],nsmall=PRdigits),format(sumMean[y,1:Progression_end],nsmall=PRdigits),format(sumMedian[y,1:Progression_end],nsmall=PRdigits),format(sumMode[y,1:Progression_end],nsmall=PRdigits),format(sumT[y,1:Progression_end],nsmall=PRdigits),format(sumSD[y,1:Progression_end],digits=4,nsmall=PRdigits)),ncol=13))  #,dimnames=list(row_names,colName))

          names(matS)<-c("Err","Y","Time Pts","hours from RefDateTime","#Pts","N","Low","High","Mean","Median","Mode","dT","s.d.") # format column names
          addTable(rtf,matS,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.75,.3,1.3,1.05,.85,.5,.6,.6,.75,.75,.5,.5,.5))
        }  # End Components = 1
        if (Progression_end>1){     #  if there is more than one Interval, print final row:  full data summary 
          # non progressive and progressive!  matS<-data.frame(matrix(cbind(Err,Y,paste(StartDate,"-",EndDate),format(MyData_hours,digits=PRdigits,nsmall=PRdigits),paste(StartIndex[1],"-",tail(EndIndex,1)),format(sumN,digits=PRdigits,nsmall=PRdigits),format(sumLow[,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumHi[,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMean[,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMedian[,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMode[,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumT[,1:Progression_end],nsmall=PRdigits),format(sumSD[,Progression_end+1],digits=PRdigits,nsmall=PRdigits)),ncol=13))
          matS<-data.frame(matrix(cbind("",printYlab,paste(StartDate,"-",EndDate),format(MyData_hours,digits=PRdigits,nsmall=PRdigits),paste(StartIndex[1],"-",tail(EndIndex,1)),format(sumN[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumLow[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumHi[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMean[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMedian[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMode[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumT[y,Progression_end+1],nsmall=PRdigits),format(sumSD[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits)),ncol=13))     #not printYlab1[y]  10/6/2017
          names(matS)<-c("Err","Y","Total Time","Total Hours","Total Pts#","N","Low","High","Mean","Median","Mode","dT","s.d.")
          addTable(rtf,matS,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.75,.3,1.3,1.05,.85,.5,.6,.6,.75,.75,.5,.5,.5))
        } 
      }     # end RTF
      #####################################################################################################
      #                                                                                                   #
      #        print the Rhythmometric Summary for each Period and Y;   sumXxx are summary variables      #
      #                                   Each Y data set has a TOTALS row, and a row for each interval   #
      #                Non-progressives:  multi Y, multi component, multi-period all are done the same:   #
      #                                   (y,1+1) holds the summary variables for non-progressives        #
      #                                   (y,1) is the same as (y,1+1) so is not used                     #
      #                Progressives:  progressives have one summary for each span, and a total row        #
      #                                   (y,progression_end) holds the summary variables                 #
      #                                   (y,progression_end+1) holds the summary variables totals        #
      #                                                                                                   #
      #        Columns preceeding data are used in later data processing to re-sort and re-order          #
      #                                                                                                   #
      #####################################################################################################
      
      if (Output$Dat){    #  figure out how to write without closing; and then how to write without opening;  Time, Pts, hours, #Pts are not working
        sPtsX2<-sub(pattern="\n",replacement=" ",x=sPtsX)   # this didn't work on matrix
        jLabel<-c(1:Progression_end)
        if (Components>1){
          PRX<-array(data=multi_PR,c(i,j,m))
          printP_multi<-format(multi_P,digits=4,nsmall=PMAdigits)
          printP_multi[multi_P<.0005]<-c("<.001")
          PX<-array(data=printP_multi,c(i,j,m))
          MagX<-array(data=Magnitude,c(i,j,m))
          BathyX<-array(data=Bathyphase,c(i,j,m))
          OrthoX<-array(data=Orthophase,c(i,j,m))

          datMat<-cbind("R",Err,functionName,SubjectID[i],jLabel,yVarX,sub("\n",timeX,replacement="",fixed=TRUE),hoursX,sPtsX2,format(Cycle,nsmall=PRdigits),format(newPR,digits=2*PRdigits,nsmall=PRdigits,scientific=FALSE),printP,format(MX,digits=2*PRdigits,nsmall=PMAdigits),format(mesor_seX,digits=2*PRdigits,nsmall=PMAdigits),format(A,digits=2*PRdigits,nsmall=PMAdigits),format(amp_se,digits=2*PRdigits,nsmall=PMAdigits),format(PHI,digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE),format(phi_se,digits=2*PHIdigits,nsmall=PHIdigits),LCM,PRX,PX,MagX,OrthoX,BathyX)
          datMatNames<-c("Hdr","Err","Fn","subject","section","Y","Time Pts","hours from RefDateTime","#Pts","Period in Hrs","PR","P","Mesor","Mesor SE","Amp","Amp SE","PHI","PHI SE","Total:  LCM","PR","P","Magnitude", "Orthophase","Bathyphase")
        } else{
          datMat<-cbind("R",Err,functionName,SubjectID,jLabel,yVarX,sub("\n",timeX,replacement="",fixed=TRUE),hoursX,sPtsX2,format(Cycle,nsmall=PRdigits),format(newPR,digits=2*PRdigits,nsmall=PRdigits,scientific=FALSE),printP,format(MX,digits=2*PRdigits,nsmall=PMAdigits),format(mesor_seX,digits=2*PRdigits,nsmall=PMAdigits),format(A,digits=2*PRdigits,nsmall=PMAdigits),format(amp_se,digits=2*PRdigits,nsmall=PMAdigits),format(PHI,digits=2*PHIdigits,nsmall=PHIdigits,scientific=FALSE),format(phi_se,digits=2*PHIdigits,nsmall=PHIdigits))
          datMatNames<-c("Hdr","Err","Fn","subject","section","Y","Time Pts","hours from RefDateTime","#Pts","Period in Hrs","PR","P","Mesor","Mesor SE","Amp","Amp SE","PHI","PHI SE")
        }
    
        # Needs to have [y,,x]    call12rhcsingle.R   --  added 1 to first 5 for Yoshiko/callYW8714.R
        datMatS<-cbind("S",Err[1,,m],functionName,SubjectID[i],jLabel,yVarX[1,,m],sub("\n",timeX[1,,m],replacement="",fixed=TRUE),hoursX[1,,m],sPtsX2[1,,m],format(sumN[y,1:Progression_end],nsmall=PRdigits),format(sumLow[y,1:Progression_end],digits=PRdigits,nsmall=PRdigits),format(sumHi[y,1:Progression_end],digits=PRdigits,nsmall=PRdigits),format(sumMean[y,1:Progression_end],digits=PRdigits,nsmall=PRdigits),format(sumMedian[y,1:Progression_end],digits=PRdigits,nsmall=PRdigits),format(sumMode[y,1:Progression_end],digits=PRdigits,nsmall=PRdigits),format(sumT[y,1:Progression_end],nsmall=PRdigits),format(sumSD[y,1:Progression_end],digits=PRdigits,nsmall=PRdigits))
        datMatNamesS<-c("Hdr","Err","Fn","subject","section","Y","Time Pts","hours from RefDateTime","#Pts","N","Low","High","Mean","Median","Mode","dT","SD")
        dimnames(datMat)<-NULL
        dimnames(datMatS)<-NULL
        #eol = "\r\n" will produce Windows' line endings on a Unix-alike OS, and eol = "\r" will produce files as expected by Excel:mac 2004.
        # write Rhythmometric data to file, including header if y==1 

        if (y==1){
          write.table(datMat,file=fileoutCosX,append=FALSE,quote=FALSE,sep="\t",row.names=FALSE,col.names=datMatNames)
        } else {
          write.table(datMat,file=fileoutCosX,append=TRUE,quote=FALSE,sep="\t",row.names=FALSE,col.names=datMatNames)
        }
        #  write Data Summary Data (each Y has a TOTAL data summary, and each progressive span has a data summary)
        write.table(datMatS,file=fileoutCosX,append=TRUE,quote=FALSE,sep="\t",row.names=FALSE,col.names=datMatNamesS)
        if (Progression_end>1){     #  if there is more than one Interval, print full data summary  
          # (chgned from Y to Ycol when progressive, multi-period, multi-y, single comp)
          datMatS<-cbind("STot","",functionName,SubjectID[i],"",printYlab,paste(StartDate,"-",EndDate),format(MyData_hours,digits=PRdigits,nsmall=PRdigits),paste(StartIndex[1],"-",tail(EndIndex,1)),format(sumN[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumLow[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumHi[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMean[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMedian[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumMode[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),format(sumT[y,Progression_end+1],nsmall=PRdigits),format(sumSD[y,Progression_end+1],digits=PRdigits,nsmall=PRdigits),"\n")      #not printYlab1[y]  10/6/2017
          datMatNamesS<-c("Hdr"," ","Fn","subject"," ","Y","Total Time","Total Hours","Total Pts#","N","Low","High","Mean","Median","Mode","dT","SD"," ")
          dimnames(datMatS)<-NULL

          write.table(datMatS,file=fileoutCosX,append=TRUE,quote=FALSE,sep="\t",row.names=FALSE,col.names=datMatNamesS)
        } 
      }   # end Dat
    }   # end RTF or Dat

    if (Output$Doc){
      
      #  separation
      addNewLine(rtf,4)
      if (y==Ys_end && j==Progression_end){
        keyMsgsX<-as.data.frame(keyMsgs)
        names(keyMsgsX)<-"Key:  Err column decode."
        addTable(rtf,keyMsgsX)
        addNewLine(rtf,4)
        addPageBreak(rtf)
        addSessionInfo(rtf)
      } else {
        Err <- array(data="",c(RowCnt, Progression_end,Components))
      }
      
      done(rtf)                    # writes and closes the file
    }
  }    #    END for (y in 1:Ys_end){

closeOutput(file=fileName3,output=Output,console=Console,opar=opar)
rm(GraphSet)
}
