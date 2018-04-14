########################################################################################################
########################################################################################################
####                                   PATHChange Package
########################################################################################################
########################################################################################################
####################################  PATHChange function  #############################################
########################################################################################################

PATHChange <- function(path, MeanData, writeCSV, writeRDS, destDIR){
  p.value<-list()
  for(j in 1:length(MeanData)){
    k=0
    result<-matrix(0,nrow=length(path), ncol=5, 
                   dimnames = list(NULL, c("Pathway","Activity", "Bootstrap", "Fisher", "Wilcoxon" )))
    repeat{
      k=k+1
      Genes.path<-merge(MeanData[[j]],path[[k]],by.x=c("Symbol"), by.y=c("ApprovedSymbol"))[,-4]
      
      ####################################################################################################
      ###                                        BOOTSTRAP
      ####################################################################################################
      nBoot <- 10000
      sample.e <- matrix(0, dim(path[[k]])[1], nBoot)
      sample.c <- matrix(0, dim(path[[k]])[1], nBoot)
      sample <- array(0,dim=c(dim(path[[k]])[1], 3, nBoot))
      for (i in 1:nBoot){
        sample[,,i] <- as.matrix(MeanData[[j]][sample(nrow(MeanData[[j]]), dim(path[[k]])[1], replace=TRUE), ])
        sample.e [,i] <- as.numeric(sample[1:dim(path[[k]])[1],2,i])
        sample.c [,i] <- as.numeric(sample[1:dim(path[[k]])[1],3,i])
      }
      
      exp <- Genes.path$Experiment
      ctrl <- Genes.path$Control
      activ <- function(exp,ctrl){                                                # Activity expression
        nboot <- sum(exp)/(sum(exp)+sum(ctrl))
        nfisher <- exp/(exp+ctrl)
        return(c(nboot, nfisher))
      }
      n.1 <- as.matrix(activ(exp,ctrl))
      
      a <- matrix(0,dim(sample.e)[1]+1,nBoot)
      for(i in 1:nBoot) {
        a[,i] <- activ(sample.e[,i],sample.c[,i])
      }
      
      activity <- sum(a[1,]>n.1[1,])/nBoot
      
      #####################################################################################################
      ###                                       Wilcoxon test
      #####################################################################################################
      
      Wilcoxon <- wilcox.test(exp, ctrl, paired=TRUE)$p.value
      
      #####################################################################################################
      ###                                        Fisher test
      #####################################################################################################
      
      Genes.out.path <- data.frame(Symbol=setdiff(MeanData[[j]][,1], Genes.path[,1]))
      Genes.out.path <- merge(MeanData[[j]],Genes.out.path,by.x=c("Symbol"), by.y=c("Symbol"))
      
      exp <- Genes.out.path$Experiment
      ctrl <- Genes.out.path$Control
      
      Out.Pathway <- activ(exp,ctrl)
      
      Path.decrease.n <- sum(n.1[-1]<0.5); Path.increase.n <- sum(n.1[-1]>0.5)
      OutPath.decrease.n <- sum(Out.Pathway[-1]<0.5); OutPath.increase.n <- sum(Out.Pathway[-1]>0.5)
      
      cont.table.Act <- matrix(c(Path.increase.n, OutPath.increase.n, Path.decrease.n, OutPath.decrease.n),
                               2,2,dimnames = list(c("In Pathway", "Out Pathway"),
                                                   c("Increase Expression", "Decrease Expression")))
      
      Fisher <- fisher.test(cont.table.Act)$p.value
      
      
      result[k,]<-c(as.character(unique(path[[k]]$Pathway)),n.1[1], activity, Fisher, Wilcoxon)
      if(k==length(path)) break;
    }
    p.valFDR <- cbind(result[,c(1,2)],apply(result[,-c(1,2)], 2, function(x) p.adjust(x, method="BH")))
    p.value[j]<-list(p.valFDR)
    if(writeCSV==TRUE){write.csv(p.value[j], file = paste(destDIR, paste(names(MeanData)[j], ".csv"), sep="/"), row.names=FALSE)}
  }
  list.save(p.value, file.path(tempdir(),"pValue.rds"))
  if(writeRDS==TRUE){list.save(p.value, paste(destDIR,"pValue.rds", sep="/"))}
  return(p.value)
}