## Function for separating data into build/test groups. ##
# frac is the fraction you want to be as build - 2/3 works well for keeping
# the size of the test and build groups roughly the same
# grp must be a list separated by batch #

sepdata<-function(grp,frac=(2/3)){

	#Figure out which arrays belong to each group#
	ind1<-which(grp==1)
	ind0<-which(grp==0)

	#Figure out how many arrays for each group#	
	length(ind1)->len1
	length(ind0)->len0
		
	#Create a unique ordering of arrays from each group#
	index1<-sample(ind1,len1,replace=FALSE)
	index0<-sample(ind0,len0,replace=FALSE)

	#Will take first 2/3 of arrays from each group as build groups#
	buildlen1<-round((frac)*len1)
	buildlen0<-round((frac)*len0)
	build1<-index1[1:buildlen1]
	test1<-index1[(buildlen1+1):len1]
	build0<-index0[1:buildlen0]
	test0<-index0[(buildlen0+1):len0]

		#Create a unique index that identifies which arrays go into which group#
		# 1 = build 1
		# 2 = test 1
		# 3 = build 0
		# 4 = test 0
		
	index<-rep(0,length(grp))
	index[build1]<-1
	index[test1]<-2
	index[build0]<-3
	index[test0]<-4
	return(index)
}