

example <- function(){
	data(primate_data)
	# Drop duplicate genuses (should average instead).  
	nuc_names <- nuc$tip.label
	dups <- duplicated(traits[['Genus']])
	trait_names <- as.character(traits$Genus[!dups])
	x <- traits$log_brain.weight[!dups]

	# Put names on data and drop taxa that aren't in both tree data and trait data
	names(x) <- trait_names
	compare <- treedata(nuc, x)

	# Do the ancestral state reconstruction and make a plot
	out <- ace(compare$data, compare$phy)
	plot_cts_ancestor(compare$phy, compare$data, out)
}

plot_cts_ancestor <- function(phy, data, ancestral){
# A function to plot current and ancestral states using continuous colormap
# Args: 
#		phy: an ape-style tree (object of class phylo)
#		data: named data-set (as passed to ace function)
#		ancestral: Output of ace function
# Returns:
#		colormapped, labeled plot.  

	plot(phy) # just to get treelength 
	treelen <- max(axisPhylo() )
	plot(phy, cex=1, x.lim=1.3*treelen, edge.width=2)
	mycol <- function(x){
		tmp = (x - min(data)) /max(x-min(data)) 
		rgb(blue = 1-tmp, green=0, red = tmp )
	}
	nodelabels(pch=19, col=mycol(ancestral$ace), cex=1.5  )
	tiplabels(pch=19, col=mycol(data), cex=1.5, adj=0.5) # add tip color code
}

