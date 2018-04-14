# Recall that for each of the 100 iterations of build/test, you are
# creating a new set of probes that are determined to be batch-affected
# for rma preprocessing and frma preprocessing, for each of the two studies.
#
# So each study has a matrix with 100 rows (one for each iteration) and columns for each probe.
# Need to compare within a study consistency versus between study consistency.
#
#
#


compare<-function(){
load("Section6_GSE2034_RMA_badps.RData")
load("Section6_GSE2034_fRMA_badps.RData")

x<-matrix(nrow=100,ncol=22283)



### SETTING UP RESULTS ###
rma.2034.1<-x
rma.2034.2<-x
rma.2034.3<-x
rma.2034.4<-x
rma.2034.5<-x
rma.2034.6<-x

rma.2034<-list(rma.2034.1,
			   rma.2034.2,
			   rma.2034.3,
			   rma.2034.4,
			   rma.2034.5,
			   rma.2034.6)

frma.2034.1<-x
frma.2034.2<-x
frma.2034.3<-x
frma.2034.4<-x
frma.2034.5<-x
frma.2034.6<-x

frma.2034<-list(frma.2034.1,
			   frma.2034.2,
			   frma.2034.3,
			   frma.2034.4,
			   frma.2034.5,
			   frma.2034.6)

for(j in 1:100){
	rma.2034[[1]][j,]<-rma.bad.ps.1[[j]]
	rma.2034[[2]][j,]<-rma.bad.ps.2[[j]]
	rma.2034[[3]][j,]<-rma.bad.ps.3[[j]]
	rma.2034[[4]][j,]<-rma.bad.ps.4[[j]]
	rma.2034[[5]][j,]<-rma.bad.ps.5[[j]]
	rma.2034[[6]][j,]<-rma.bad.ps.6[[j]]
	
	frma.2034[[1]][j,]<-frma.bad.ps.1[[j]]
	frma.2034[[2]][j,]<-frma.bad.ps.2[[j]]
	frma.2034[[3]][j,]<-frma.bad.ps.3[[j]]
	frma.2034[[4]][j,]<-frma.bad.ps.4[[j]]
	frma.2034[[5]][j,]<-frma.bad.ps.5[[j]]
	frma.2034[[6]][j,]<-frma.bad.ps.6[[j]]
}

rma.2034.sums<-matrix(nrow=6,ncol=22283)
frma.2034.sums<-matrix(nrow=6,ncol=22283)
for(i in 1:6){
	rma.2034.sums[i,]<-colSums(rma.2034[[i]])
	frma.2034.sums[i,]<-colSums(frma.2034[[i]])
}


load("Section6_GSE2603_RMA_badps.RData")
load("Section6_GSE2603_fRMA_badps.RData")

rma.2603.1<-x
rma.2603.2<-x
rma.2603.3<-x
rma.2603.4<-x
rma.2603.5<-x
rma.2603.6<-x

rma.2603<-list(rma.2603.1,
			   rma.2603.2,
			   rma.2603.3,
			   rma.2603.4,
			   rma.2603.5,
			   rma.2603.6)

frma.2603.1<-x
frma.2603.2<-x
frma.2603.3<-x
frma.2603.4<-x
frma.2603.5<-x
frma.2603.6<-x

frma.2603<-list(frma.2603.1,
			   frma.2603.2,
			   frma.2603.3,
			   frma.2603.4,
			   frma.2603.5,
			   frma.2603.6)

for(j in 1:100){
	rma.2603[[1]][j,]<-rma.bad.ps.1[[j]]
	rma.2603[[2]][j,]<-rma.bad.ps.2[[j]]
	rma.2603[[3]][j,]<-rma.bad.ps.3[[j]]
	rma.2603[[4]][j,]<-rma.bad.ps.4[[j]]
	rma.2603[[5]][j,]<-rma.bad.ps.5[[j]]
	rma.2603[[6]][j,]<-rma.bad.ps.6[[j]]
	
	frma.2603[[1]][j,]<-frma.bad.ps.1[[j]]
	frma.2603[[2]][j,]<-frma.bad.ps.2[[j]]
	frma.2603[[3]][j,]<-frma.bad.ps.3[[j]]
	frma.2603[[4]][j,]<-frma.bad.ps.4[[j]]
	frma.2603[[5]][j,]<-frma.bad.ps.5[[j]]
	frma.2603[[6]][j,]<-frma.bad.ps.6[[j]]
}

rma.2603.sums<-matrix(nrow=6,ncol=22283)
frma.2603.sums<-matrix(nrow=6,ncol=22283)
for(i in 1:6){
	rma.2603.sums[i,]<-colSums(rma.2603[[i]])
	frma.2603.sums[i,]<-colSums(frma.2603[[i]])
}


# Calculate the number of probes in common between
# all pairs of columns withn a preprocessing method / algorithm
# Report the median number, Q1, Q3
# 
# Four objects:  Lists with 6 matrices - matrices are 100 x 22283
#	rma.2034
#	frma.2034
#	rma.2603
#	frma.2603

fun<-function(mat){
	Q1<-rep(0,6)
	Q3<-rep(0,6)
	medians<-rep(0,6)
	for(h in 1:6){
		temp<-mat[[h]]
		temp.res<-rep(0,10000)
		k<-1
		for(i in 1:100){
			for(j in 1:100){
				temp.res[k]<-round(sum(temp[i,]==temp[j,] & temp[i,]==1)*100 / length(temp[i,]))
				k<-k+1
			}
		}
		tmp<-summary(temp.res)
		Q1[h]<-tmp[2]
		medians[h]<-tmp[3]
		Q3[h]<-tmp[5]
		pcts<-c("10%","20%","30%","40%","50%","60%")
	}
	res<-cbind(Q1,medians,Q3)
	rownames(res)<-pcts
	library(xtable)
	print(xtable(res,digits=0))
}

fun(rma.2034)
fun(frma.2034)
fun(rma.2603)
fun(frma.2603)



rma.2034.inds<-matrix(0,nrow=6,ncol=22283)
frma.2034.inds<-matrix(0,nrow=6,ncol=22283)
rma.2603.inds<-matrix(0,nrow=6,ncol=22283)
frma.2603.inds<-matrix(0,nrow=6,ncol=22283)

rma.res<-c()
frma.res<-c()
for(i in 1:6){
	rma.2034.inds[i,(rma.2034.sums[i,]>50)]<-1
	frma.2034.inds[i,(frma.2034.sums[i,]>50)]<-1
	rma.2603.inds[i,(rma.2603.sums[i,]>50)]<-1
	frma.2603.inds[i,(frma.2603.sums[i,]>50)]<-1

	rma.res[i]<-sum(rma.2034.inds[i,]==rma.2603.inds[i,] & rma.2034.inds[i,]==1)/
				length(rma.2034.inds[i,])
	frma.res[i]<-sum(frma.2034.inds[i,]==frma.2603.inds[i,] & frma.2034.inds[i,]==1)/
				length(frma.2034.inds[i,])
}

pcts<-c("10%","20%","30%","40%","50%","60%")
print(xtable(cbind(pcts,round(rma.res*100),round(frma.res*100))))



}