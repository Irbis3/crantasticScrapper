# R

data <- read.csv("Sheet_1.csv")
by_practicum <- function(x){
	N <- 3 	
	y <- numeric(N)
	for(i in 1:N){
		y[i]  <- sum( x[data[[185]] == i], na.rm=T)
	}
	r <- hist(data[[185]], breaks=0:N, plot=FALSE)
	y <- y / r$counts

	names(y) = c("Completed", "Current", "Not yet")
	y
}

by_phd <- function(x){
	N <- 8 	
	y <- numeric(N)
	for(i in 1:N){
		y[i]  <- sum( x[data[[183]] == i], na.rm=T)
	}
	r <- hist(data[[183]], breaks=0:N, plot=FALSE)
	y <- y / r$counts

	names(y) = c("physics", "chem", "CS", "math", "engin", "biology", "inter", "other")
	y
}
phd <- hist(data[[183]], breaks=0:8, plot=FALSE)$counts
names(phd) = c("physics", "chem", "CS", "math", "engin", "biology", "inter", "other")



by_year <- function(x){
	y <- numeric(6)
	for(i in 1:6){
		y[i]  <- sum( x[data[[179]] == i], na.rm=T)
	}
	r <- hist(data[[179]], breaks=0:6, plot=FALSE)

	y <- y / r$counts
	names(y) = c("new", "first", "second", "third", "fourth", "alumni")
	y
}
years <- hist(data[[179]], breaks=0:6, plot=FALSE)$counts
names(years) = c("new", "first", "second", "third", "fourth", "alumni")

compiled <- rowSums(data[,17:20] >= 5, na.rm=T)

pdf("compiled.pdf", 14, 7)
par(mfrow=c(1,2))
barplot(by_year(compiled>=1), main="frequently use >= one compiled language")
barplot(by_phd(compiled>=1))
dev.off()



# Who uses Fortran77 ??
pdf("fortran77.pdf", 14, 7)
par(mfrow=c(1,2))
barplot(by_phd(data[,19] >= 5), main="Who uses Fortran77?" )
barplot(by_year(data[,19] >= 5), main="Who uses Fortran77?" )
dev.off()
# Fortran77 + 90
#barplot(by_phd(rowSums((data[,19:20] >= 5, na.rm=T) )), main="any Fotran")
#barplot(by_year(rowSums((data[,19:20] >= 5, na.rm=T) )), main="Fortran")

pdf("scripting.pdf", 14, 7)
par(mfrow=c(1,2) )
barplot(by_phd(data[,21] >= 5), main="Python")
#barplot(by_year(data[,21] >= 5), main="Python")
barplot(by_phd(data[,22] >= 5), main="Matlab" )
#barplot(by_year(data[,22] >= 5), main="Matlab" )
dev.off()



# Never heard of R
pdf("R.pdf")
barplot(by_phd(data[,23] == 1), main = "Never Heard of R" )
dev.off()
#barplot(by_year(data[,23] == 1) )

barplot(by_phd(data[,27] >= 2), main = "Has heard of Haskell")

# Who ever heard of netlogo?
#barplot(by_phd(data[,28] >= 2), main = "Has heard of netlogo")
#barplot(by_year(data[,28] == 2) )

pdf("multilingual.pdf")
barplot(by_phd( rowSums(data[,17:33] >= 5, na.rm=T) > 5 ), main="uses over 5 langauges a day")
dev.off()


## Use at least one parallel interface 
pdf("some_parallel.pdf", 14, 7)
par(mfrow=c(1,2) )
barplot( by_phd(rowSums( data[,35:41]>=3 , na.rm=T) >=1 ), main="Uses some parallel code" )
barplot( by_year(rowSums( data[,35:41]>=3 , na.rm=T) >= 1 ), main="Uses some parallel code" )
dev.off()

#barplot( by_phd(rowSums( data[,35:41]>=4, na.rm=T ) >= 1 ), main="regularly use at least one parallel interface" )
#barplot( by_year(rowSums( data[,35:41]>=4, na.rm=T ) >= 1) )

pdf("obsessed.pdf", 14, 7)
par(mfrow=c(1,2) )
barplot( by_phd(rowSums( data[,35:41]>=4, na.rm=T ) >= 3 ), main="regularly use at 3 or more parallel interfaces" )
barplot( by_year(rowSums( data[,35:41]>=4, na.rm=T ) >= 3) )
dev.off()


# Simple parallel

# GPU
pdf("gpu.pdf", 14, 7)
par(mfrow=c(1,2) )
barplot(by_phd(data[[13]] >=2), main="do some GPU" )
barplot(by_year(data[[13]] >=2), main="do some GPU" )
dev.off()

#barplot(by_phd(data[[13]] >=3), main="primary GPU" )
#barplot(by_year(data[[13]] >=3), main="primary GPU" )



# Scores on good practices quiz
carpentry<- rowSums(data[,82:96], na.rm=T)
#barplot(by_year(carpentry) , main="Good Practices by year (out of 16)" )
#barplot(by_phd(carpentry) , main="Good Practices by PhD degree (out of 16)" )
pdf("carpentry.pdf")
barplot(by_practicum(carpentry) , main="Good Practices by Practicum" )
dev.off()



