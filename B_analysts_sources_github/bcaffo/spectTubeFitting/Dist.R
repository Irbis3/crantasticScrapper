#############################################################
# Aug 29 2011
# Jeff Goldsmith
#
# this file contains a function to find the euclidean dist
# between two points
#############################################################

dist2=function(pt1, pt2){
	if(length(pt1)==2){
		ret=sqrt((pt1[1]-pt2[1])^2+(pt1[2]-pt2[2])^2)
	}

	if(length(pt1)==3){
		ret=sqrt((pt1[1]-pt2[1])^2+(pt1[2]-pt2[2])^2+(pt1[3]-pt2[3])^2)
	}
	ret
}


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################