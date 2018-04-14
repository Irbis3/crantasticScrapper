`mloglik` <-
function(x,alpha,Y,Z){
#minus log prob of graph
	n <- nrow(Y) - nrow(Z)
	p <- ncol(Z)
	xx <- matrix(x,nrow=n,ncol=p)
	ZZ = rbind(Z,xx)
	distance <- distz(ZZ)
	eta <- alpha - distance
	diag(eta) <- 0
	- sum(Y * eta - log(1 + exp(eta)))
}

