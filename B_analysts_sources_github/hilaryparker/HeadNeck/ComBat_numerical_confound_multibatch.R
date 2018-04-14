# 'expression_xls' is the expression index file (e.g. outputted by dChip); 'sample_info_file' is a tab-delimited text file containing the colums: Array  name, sample name, Batch, and any other covariates to be included in the modeling; 'type' currently supports two data file types 'txt' for a tab-delimited text file and 'csv' for an Excel .csv file (sometimes R handles the .csv file better, so use this if you have problems with a .txt file!); 'write' if 'T' ComBat writes adjusted data to a file, and if 'F' and ComBat outputs the adjusted data matrix if 'F' (so assign it to an object! i.e. NewData <- ComBat('my expression.xls','Sample info file.txt', write=F)); 'covariates=all' will use all of the columns in your sample info file in the modeling (except array/sample name), if you only want use a some of the columns in your sample info file, specify these columns here as a vector (you must include the Batch column in this list); 'par.prior' if 'T' uses the parametric adjustments, if 'F' uses the nonparametric adjustments--if you are unsure what to use, try the parametric adjustments (they run faster) and check the plots to see if these priors are reasonable; 'filter=value' filters the genes with absent calls in > 1-value of the samples. The defaut here (as well as in dchip) is .8. Filter if you can as the EB adjustments work better after filtering. Filter must be numeric if your expression index file contains presence/absence calls (but you can set it >1 if you don't want to filter any genes) and must be 'F' if your data doesn't have presence/absence calls; 'skip' is the number of columns that contain probe names and gene information, so 'skip=5' implies the first expression values are in column 6; 'prior.plots' if true will give prior plots with black as a kernal estimate of the empirical batch effect density and red as the parametric estimate. 

ComBat <- function(expression_xls, sample_info_file, type='txt', write=T, covariates='all', numeric.covariates=NULL, par.prior=T, filter=FALSE, skip=0, prior.plots=T, confounded=FALSE, pval_cut=.2, pct=FALSE){
	#debug: expression_xls='exp.txt'; sample_info_file='sam.txt'; type='txt'; write=T; covariates='all'; par.prior=T; filter=F; skip=0; prior.plots=T;  numeric.covariates=NULL
	#debug: setwd("~/Desktop/projects/BatchConfound/"); source("ComBat_numerical.R")
        #debug: expression_xls='dataExample1.txt'; sample_info_file='sampleInfoExample1multibatch.txt'; type='txt'; write=T; covariates='all'; par.prior=T; filter=F; skip=0; prior.plots=T; numeric.covariates=NULL;confounded=FALSE
	cat('Reading Sample Information File\n')
	saminfo <- read.table(sample_info_file, header=T, sep='\t',comment.char='')
	if(sum(substring(colnames(saminfo),1,5)=="Batch")==0){return('ERROR: Sample Information File does not have a Batch column!')}
		
	cat('Reading Expression Data File\n')
	if(type=='csv'){
		dat <- read.csv(expression_xls,header=T,as.is=T)
        #	dat <- dat[,trim.dat(dat)]  
        	colnames(dat) <- scan(expression_xls,what='character',nlines=1,sep=',',quiet=T)[1:ncol(dat)]
     		}else{
		dat <- read.table(expression_xls,header=T,comment.char='',fill=T,sep='\t', as.is=T)
		dat <- dat[,trim.dat(dat)]
		colnames(dat) <- scan(expression_xls,what='character',nlines=1,sep='\t',quiet=T)[1:ncol(dat)]
		}
	if (skip>0){
              geneinfo <- as.matrix(dat[,1:skip])
              dat <- dat[,-c(1:skip)]
            }else{geneinfo <- NULL}

        if(filter){
		ngenes <- nrow(dat)
		col <- ncol(dat)/2
		present <- apply(dat, 1, filter.absent, filter)
		dat <- dat[present, -(2*(1:col))]
		if (skip>0){geneinfo <- geneinfo[present,]}
		cat('Filtered genes absent in more than',filter,'of samples. Genes remaining:',nrow(dat),'; Genes filtered:',ngenes-nrow(dat),'\n')
		}

	if(any(apply(dat,2,mode)!='numeric')){return('ERROR: Array expression columns contain non-numeric values! (Check your .xls file for non-numeric values and if this is not the problem, make a .csv file and use the type=csv option)')}
        
	tmp <- match(colnames(dat),saminfo[,1])
	if(any(is.na(tmp))){return('ERROR: Sample Information File and Data Array Names are not the same!')}
	#tmp1 <- match(saminfo[,1],colnames(dat))
	#saminfo <- saminfo[tmp1[!is.na(tmp1)],]		
	saminfo <- saminfo[tmp,] # Bug fixed 01/04/2011

	if(any(covariates != 'all')){saminfo <- saminfo[,c(1:2,covariates)]}
	design <- design.mat(saminfo,numeric.covariates)	

        batches <- list.batch(saminfo)
        batch.vars <- unique(names(batches))
	n.batch <- length(batches)
	n.batches <- sapply(batches, length)
	n.array <- nrow(saminfo)
        if(qr(design)$rank<ncol(design)){
          if((ncol(design)>n.batch) & (qr(design[,-c(1:n.batch)])$rank<ncol(design[,-c(1:n.batch)]))){
            return('ERROR: The experimental design of your covariates is confounded! Please remove one or more covariates so that your design is no longer confounded.')
          }else{
            return("ERROR: One or more of your covariates are confounded with Batch! Please remove the confounded covariates and rerun ComBat with the 'confounded=TRUE' option")
          }
        }

	## Check for missing values
	NAs  <-  any(is.na(dat))
	if(NAs){cat(c('Found',sum(is.na(dat)),'Missing Data Values\n'),sep=' ')}

        ##Standardize Data across genes
	cat('Standardizing Data across genes\n')
	if (!NAs){B.hat <- solve(t(design)%*%design)%*%t(design)%*%t(as.matrix(dat))}else{B.hat <- apply(dat,1,Beta.NA,design)} #Standarization Model
        tmpdesign <- cbind(matrix(apply(design[,1:n.batch],2,mean),nrow(design),n.batch,byrow=T),design[,-c(1:n.batch)])  #
        stand.mean <- t(tmpdesign%*%B.hat)
	var.pooled <- apply(dat-stand.mean,1,var,na.rm=T)
	s.data <- (dat-stand.mean)/(sqrt(var.pooled)%*%t(rep(1,n.array)))
      
	##Get regression batch effect parameters
        #If number of batch variables is greater than 1, the first level of each batch variable is used as a reference level, and the rest of the mean and variance parameters (for variable 2 on) are relative to the reference.
	cat("Fitting L/S model and finding priors\n")
	batch.design <- design[,1:n.batch]
	if (!NAs){gamma.hat <- solve(t(batch.design)%*%batch.design)%*%t(batch.design)%*%t(as.matrix(s.data))}else{gamma.hat <- apply(s.data,1,Beta.NA,batch.design)}
	delta.hat <- NULL
	if (length(batch.vars)==1){
          for (i in batches){
            singlesam <- FALSE
            if(length(i)==1){
              singlesam <- TRUE; confounded <- TRUE
              delta.hat <- rbind(delta.hat,rep(1,nrow(s.data)))
            }else{
              delta.hat <- rbind(delta.hat,apply(s.data[,i], 1, var,na.rm=T))
            }
          }
        }else{ #If we have multuiple batch variables, we need to iterate the variances.
          cat("Iterating variance parameters for the mulitple batch variables\n")
          delta.hat <- old <- matrix(1,n.batch,nrow(s.data))
          maxdif <- 1
          while(maxdif>.05){
            singlesam <- FALSE
            for (i in batch.vars){
              tmpvars <- matrix(1,nrow(s.data),n.array)
              for (k in which(names(batches)!=i)){
                tmpvars[,batches[[k]]] <- tmpvars[,batches[[k]]]*delta.hat[k,]
              }
              tmp2 <- (s.data-batch.design%*%gamma.hat)^2/tmpvars
              for (m in which(names(batches)==i)){
                if(length(batches[[m]])==1){
                  singlesam <- TRUE; confounded <- TRUE
                  delta.hat[,m] <- rep(1,nrow(s.data))
                }else{
                  delta.hat[m,] <- apply(tmp2[,batches[[m]]],1,sum,na.rm=T)/(apply(!is.na(tmp2[,batches[[m]]]),1,sum)-1)
                }
            }
            maxdif <- max(abs(delta.hat-old)/old)
            old <- delta.hat
            #cat(maxdif, delta.hat[,1],'\n')
          }
        }
        }
        if(singlesam){cat("Warning: found batches with only one sample. Using 'confounded=TRUE' and only adjusting the mean for these batches\n")}
          
        
	##Find Priors --- Use only rank-invariant subset if confounded=TRUE
        if(confounded){
          cat("Using ANOVA to find rank invariant genes...")
          r.data <- apply(s.data,2,rank)

          overall.means <- apply(r.data,1,mean)
          cell.means <- solve(t(batch.design)%*%batch.design)%*%t(batch.design)%*%t(r.data)
          MSE <- apply((r.data-t(batch.design%*%cell.means))^2,1,sum)/(nrow(batch.design)-ncol(batch.design))
          MSB <- apply((batch.design%*%cell.means-rep(1,nrow(batch.design))%*%t(overall.means))^2,2,sum)/(ncol(batch.design)-1)
          Fs <- MSB/MSE
          pvals <- 1-pf(Fs,ncol(batch.design)-1,nrow(batch.design)-ncol(batch.design))
          if(pct){pval_cut <- as.numeric(quantile(pvals,1-pval_cut))}
          keep <- pvals>pval_cut
         
          cat("used ",round(100*mean(keep),1),"% of the genes for the prior\n",sep='')
          }else{keep <- rep(TRUE,nrow(s.data))}
          
	gamma.bar <- apply(gamma.hat[,keep], 1, mean)
	t2 <- apply(gamma.hat[,keep], 1, var)
	a.prior <- apply(delta.hat[,keep], 1, aprior)
	b.prior <- apply(delta.hat[,keep], 1, bprior)
        #cat(gamma.bar,t2,a.prior,b.prior,'\n')
       
	
	##Plot empirical and parametric priors

	if (prior.plots & par.prior){
                if(confounded){
		par(mfrow=c(3,2))
                hist(pvals,main="Invariant Rank P-values",xlab="p-value")
                abline(v=pval_cut,col=2,lwd=2)
                
                }else{par(mfrow=c(2,2))}
		tmp <- density(gamma.hat[1,])
		plot(tmp,  type='l', main="Density Plot")
		xx <- seq(min(tmp$x), max(tmp$x), length=100)
		lines(xx,dnorm(xx,gamma.bar[1],sqrt(t2[1])), col=2)
		qqnorm(gamma.hat[1,])	
		qqline(gamma.hat[1,], col=2)	
	
		tmp <- density(delta.hat[1,])
		invgam <- 1/rgamma(ncol(delta.hat),a.prior[1],b.prior[1])
		tmp1 <- density(invgam)
		plot(tmp,  typ='l', main="Density Plot", ylim=c(0,max(tmp$y,tmp1$y)))
		lines(tmp1, col=2)
		qqplot(delta.hat[1,], invgam, xlab="Sample Quantiles", ylab='Theoretical Quantiles')	
		lines(c(0,max(invgam)),c(0,max(invgam)),col=2)	
		title('Q-Q Plot')
	}
	
	##Find EB batch adjustments

	gamma.star <- delta.star <- NULL
	if(par.prior){
		cat("Finding parametric adjustments\n")
		for (i in 1:n.batch){
                     if(length(batches[[i]])==1){
                        gamma.star <- rbind(gamma.star,postmean(gamma.hat[i,],gamma.bar[i],1,1,t2[i]))
                        delta.star <- rbind(delta.star,rep(1,nrow(s.data)))
                     }else{
                       #tmpvars <- matrix(1,nrow(s.data),n.array)
                       #if (length(batch.vars)>1){
                       #  for (k in which(names(batches)!=names(batches)[i])){
                       #    tmpvars[,batches[[k]]] <- tmpvars[,batches[[k]]]*delta.hat[k,]
                       #  }
                       #}
			#temp <- it.sol(s.data[,batches[[i]]],gamma.hat[i,],delta.hat[i,],gamma.bar[i],t2[i],a.prior[i],b.prior[i],tmpvars[,batches[[i]]])
			temp <- it.sol(s.data[,batches[[i]]],gamma.hat[i,],delta.hat[i,],gamma.bar[i],t2[i],a.prior[i],b.prior[i])
			gamma.star <- rbind(gamma.star,temp[1,])
			delta.star <- rbind(delta.star,temp[2,])
			}
                   }
	}else{
		cat("Finding nonparametric adjustments\n")
		for (i in 1:n.batch){
			temp <- int.eprior(as.matrix(s.data[,batches[[i]]]),gamma.hat[i,keep],delta.hat[i,keep])
			gamma.star <- rbind(gamma.star,temp[1,])
			delta.star <- rbind(delta.star,temp[2,])
			}
		}
       

	### Normalize the Data ###
	cat("Adjusting the Data\n")

	#bayesdata <- s.data
        tmpvars <- matrix(1,nrow(s.data),n.array)
        for (k in 1:length(batches)){
          tmpvars[,batches[[k]]] <- tmpvars[,batches[[k]]]*delta.star[k,]
        }

        bayesdata <- (s.data-t(batch.design%*%gamma.star))/(sqrt(tmpvars))
		
	#j <- 1
	#for (i in batches){
	#	bayesdata[,i] <- (bayesdata[,i]-t(batch.design[i,]%*%gamma.star))/(sqrt(delta.star[j,])%*%t(rep(1,n.batches[j])))
	#	j <- j+1
	#	}

	bayesdata <- (bayesdata*(sqrt(var.pooled)%*%t(rep(1,n.array))))+stand.mean
	if(write){
		output_file <- paste('Adjusted',expression_xls,'.xls',sep='_')
                if(is.null(geneinfo)){outdata <- bayesdata}else{outdata <- cbind(geneinfo,bayesdata)}
                #write.table(outdata, file=output_file, sep="\t")
		write.table(outdata, file=output_file, sep="\t",row.names=FALSE,quote=FALSE)
		cat("Adjusted data saved in file:",output_file,"\n")
		}else{return(cbind(geneinfo,bayesdata))}
      }
      
# filters data based on presence/absence call
filter.absent <- function(x,pct){
	present <- T
	col <- length(x)/2
	pct.absent <- (sum(x[2*(1:col)]=="A") + sum(x[2*(1:col)]=="M"))/col
	if(pct.absent > pct){present <- F}
	present
	}

# Next two functions make the design matrix (X) from the sample info file 
build.design <- function(vec, des=NULL, start=2){
	tmp <- matrix(0,length(vec),nlevels(vec)-start+1)
	for (i in 1:ncol(tmp)){tmp[,i] <- vec==levels(vec)[i+start-1]}
	cbind(des,tmp)
	}

design.mat <- function(saminfo,numeric.covariates){
        design <- NULL
	if (!is.null(numeric.covariates)){
          numCovs <- saminfo[,numeric.covariates]
          saminfo <- saminfo[,-numeric.covariates]
        }else{numCovs <- NULL}
        tmp <- which(substring(colnames(saminfo),1,5) == 'Batch')
        if (length(tmp)>1){cat("Note: found",length(tmp),'batch variables in the Sample Information file\n')}
        for (i in 1:length(tmp)){
          if (i==1){s=1}else{s=2}
          tmp1 <- as.factor(saminfo[,tmp[i]])
          design <- build.design(tmp1,start=s,des=design)
          cat("Found",nlevels(tmp1),'batches in the', colnames(saminfo)[tmp[i]], 'column in the Sample Information file\n')
	}
        ncov <- ncol(as.matrix(saminfo[,-c(1:2,tmp)]))
	cat("Found",length(numeric.covariates),'numerical covariate(s)\n')
	cat("Found",ncov,'categorical covariate(s)\n')
	if(ncov>0){
		for (j in 1:ncov){
			tmp1 <- as.factor(as.matrix(saminfo[,-c(1:2,tmp)])[,j])
			design <- build.design(tmp1,des=design)
			}
		}
	cbind(design,numCovs)
	}

# Makes a list with elements pointing to which array belongs to which batch
list.batch <- function(saminfo){
        tmp <- which(substring(colnames(saminfo),1,5)== 'Batch')
        batches <- list()
        for (j in 1:length(tmp)){
          tmp1 <- as.factor(saminfo[,tmp[j]])
          for (i in min(2,j):nlevels(tmp1)){
            batches <- append(batches, list((1:length(tmp1))[tmp1==levels(tmp1)[i]]));
            names(batches)[length(batches)] <- colnames(saminfo)[tmp[j]]
          }
        }
	batches
	}

# Trims the data of extra columns, note your array names cannot be named 'X' or start with 'X.'
trim.dat <- function(dat){
	tmp <- strsplit(colnames(dat),'\\.')
	tr <- NULL
	for (i in 1:length(tmp)){tr <- c(tr,tmp[[i]][1]!='X')}
	tr
	}

# Following four find empirical hyper-prior values
aprior <- function(gamma.hat){m <- mean(gamma.hat); s2 <- var(gamma.hat); (2*s2+m^2)/s2}
bprior <- function(gamma.hat){m <- mean(gamma.hat); s2 <- var(gamma.hat); (m*s2+m^3)/s2}
postmean <- function(g.hat,g.bar,n,d.star,t2){(t2*n*g.hat+d.star*g.bar)/(t2*n+d.star)}
postvar <- function(sum2,n,a,b){(.5*sum2+b)/(n/2+a-1)}


# Pass in entire data set, the design matrix for the entire data, the batch means, the batch variances, priors (m, t2, a, b), columns of the data  matrix for the batch. Uses the EM to find the parametric batch adjustments

#it.sol  <- function(sdat,g.hat,d.hat,g.bar,t2,a,b,tmpvars,conv=.05){
it.sol  <- function(sdat,g.hat,d.hat,g.bar,t2,a,b,conv=.05){
	n <- apply(!is.na(sdat),1,sum)
	g.old <- g.hat
	d.old <- d.hat
	change <- 1
	count <- 0
	while(change>conv){
		g.new <- postmean(g.hat,g.bar,n,d.old,t2)
		#sum2 <- apply((sdat-g.new%*%t(rep(1,ncol(sdat))))^2/tmpvars, 1, sum,na.rm=T)
		#d.new <- postvar(sum2,n,a,b)
		d.new <- postvar(d.hat*(n-1),n,a,b)
		change <- max(abs(g.new-g.old)/g.old,abs(d.new-d.old)/d.old)
		g.old <- g.new
		d.old <- d.new
		count <- count+1
                #cat(count, change, g.new[1],d.new[1],'\n')
		}
	#cat("This batch took", count, "iterations until convergence\n")
	adjust <- rbind(g.new, d.new)
	rownames(adjust) <- c("g.star","d.star")
	adjust
	}

               


#likelihood function used below
L <- function(x,g.hat,d.hat){prod(dnorm(x,g.hat,sqrt(d.hat)))}

# Monte Carlo integration function to find the nonparametric adjustments
int.eprior <- function(sdat,g.hat,d.hat){
 	g.star <- d.star <- NULL
	r <- nrow(sdat)
	for(i in 1:r){
		g <- g.hat[-i]
		d <- d.hat[-i]		
		x <- sdat[i,!is.na(sdat[i,])]
		n <- length(x)
		j <- numeric(n)+1
		dat <- matrix(as.numeric(x),length(g),n,byrow=T)
		resid2 <- (dat-g)^2
		sum2 <- resid2%*%j
		LH <- 1/(2*pi*d)^(n/2)*exp(-sum2/(2*d))
		LH[LH=="NaN"]<-0
		g.star <- c(g.star,sum(g*LH)/sum(LH))
		d.star <- c(d.star,sum(d*LH)/sum(LH))
		#if(i%%1000==0){cat(i,'\n')}
		}
	adjust <- rbind(g.star,d.star)
	rownames(adjust) <- c("g.star","d.star")
	adjust	
	} 

#fits the L/S model in the presence of missing data values

Beta.NA <- function(y,X){
	des<-X[!is.na(y),]
	y1<-y[!is.na(y)]
	B <- solve(t(des)%*%des)%*%t(des)%*%y1
	B
	}

