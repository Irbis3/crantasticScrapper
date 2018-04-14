# Section 6 macro-study #

globalwd<-"/home/bst/student/hiparker/PracticalBatch"
setwd(globalwd)
## working directory of your files ##


### Load information on GPL570 arrays ###
load("/nexsan/bst2/barcode/mmccall/barcode/GPL570/tab-GPL570.rda")
# Get arrays with more than one experiment #
x <- tab_GPL570$ExperimentID
len <- length(x)
y <- strsplit(x,';')
test <- lapply(y,length)
mult <- rep(TRUE, len)
mult[test>1] <- FALSE
# So 0's are the arrays I want to use #

# Load the actual data #
load("/nexsan/bst2/barcode/mmccall/barcode/GPL570/e.rda")

# See which data are in the expression set #
nms <- colnames(e)
arrays <- tab_GPL570$filename %in% nms


# Combine above info to get the indices of arrays that I actually want to use #
index <- rep(FALSE,len)
index[mult==TRUE & arrays==TRUE]<-TRUE
sum(index)
#[1] 16231
# nice, that's a lot of arrays we can use

# Get experiment names for each valid array #
exper <- tab_GPL570$ExperimentID[index]
arrs <- tab_GPL570$filename[index]
exper.index <- unique(exper)
exper.len <- length(exper.index)


# Get a list with the names of the arrays for each experiment #
arr.names <- list()
for(i in 1:exper.len){
	arr.names[[i]]<-arrs[exper==exper.index[i]]
}

# Limit to experiments with at least 6 arrays #
lengths <- lapply(arr.names,length)
arr.names <- arr.names[lengths>5]
exper.index <- exper.index[lengths>5]

#These seem to cause problems (in getting cel file dates)
arr.names <- arr.names[-c(77,114,165)]
exper.index <- exper.index[-c(77,114,165)]

# based on running files, know that 115, 184 also cause problems
arr.names<-arr.names[-c(115,184)]
exper.index<-exper.index[-c(115,184)]

### Now need to do cel file dates for batches! ###

# Get function #
setwd(globalwd)
source("celfileDate.R")

setwd("/nexsan/bst2/microarray/CEL/GPL570/")
lenarr<-length(arr.names)

#prep list#
x<-rep(NA,6)
dates<-list()
for(i in 1:lenarr){
	dates[[i]]<-x
}

#get celfile dates for each experiment#
for(i in 1:lenarr){
	print(i)
	for(j in 1:length(arr.names[[i]])){
		dates[[i]][j] <- celfileDate(arr.names[[i]][j])
	}
}

#prep list#
lens<-lapply(dates,length)
batches<-list()
for(i in 1:lenarr){
	batches[[i]]<-rep(NA,lens[[i]])
}

#Determine batches#
#Find median date
#make <median date+1 first batch, >median date second batch
#check to see that each batch has at least 25% of arrays.  If not, scrap experiment#

for(i in 1:lenarr){
	med<-median(dates[[i]])
	batches[[i]][dates[[i]]<(med+.25)]<-0
	batches[[i]][dates[[i]]>med]<-1
	
	if(sum(batches[[i]])/length(batches[[i]])>.75 |
	   sum(batches[[i]])/length(batches[[i]])<.25){
			batches[[i]]<-NA
	}
}

sum(is.na(batches))
# only 195 have a bad dist!! not too shabs! #
arr.names<-arr.names[!is.na(batches)]
exper.index<-exper.index[!is.na(batches)]
dates<-dates[!is.na(batches)]
batches <- batches[!is.na(batches)]


setwd(globalwd)
save(list=c("exper.index","arr.names","batches","dates"),file="Section6_multi_prelim.RData")

