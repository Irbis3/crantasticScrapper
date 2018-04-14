

generate_plots <- function(plot_item) {
	ptitle <- sprintf("(%s,%s), sA=%s, F=2",unique(plot_item$a), unique(plot_item$b), unique(plot_item$sA))
	ggplot(plot_item, aes(cv, mstar, colour= as.character(corr))) + facet_grid(~corr)   + geom_point(size =5 ,alpha=1/2, aes(shape=type)) + opts(title=ptitle)
}

# generated the juvshape curves
# -----------------------------
j_curves <- function(id) {
	dt <- split_data[[id]]
	ids <- working_js[sim_id %in% dt[type=="juvshape",]$sim_id,which=T]
	dataa <- ldply(as.list(ids), function(x) {
		if(is.data.frame(t3_juvshape[[x]]$data))
		return(t3_juvshape[[x]]$data) })
	dataa$a <- unique(dt$a)
	dataa$b <- unique(dt$b)
	dataa$sA <- unique(dt$sA)
	dataa$corr <- NA
	return(dataa)
}

# generated the corr curves
# --------------------------
c_curves <- function(id) {
	dt <- split_data[[id]]
	ids <- working_corr[sim_id %in% dt[type=="corr",]$sim_id,which=T]
	dataa <- ldply(as.list(ids), function(x) {
		if(is.data.frame(t3_corr[[x]]$data))
		return(t3_corr[[x]]$data) })
	dataa$a <- unique(dt$a)
	dataa$b <- unique(dt$b)
	dataa$sA <- unique(dt$sA)
	dataa$corr <- unique(dt$corr)
	return(dataa)
}

# generated the simple and vd curves
# ----------------------------------
o_curves <- function(id) {
	dt <- split_data[[id]]
	id1 <- working_simple[sim_id %in% dt[type=="simple",]$sim_id,which=T]
	id2 <- working_vd[sim_id %in% dt[type=="vd",]$sim_id,which=T]
	id1 <- min(id1)
	id2 <- min(id2)
	dataa <- rbind(t3_vd[[id1]]$data, t3_simple[[id2]]$data)
	dataa$a <- unique(dt$a)
	dataa$b <- unique(dt$b)
	dataa$sA <- unique(dt$sA)
	dataa$corr <- NA
	return(dataa)
}
# ----------------------------------
tplot <- function(dtt) {
	ptitle <- sprintf("(%s,%s), sA=%s, F=2",unique(dtt$a), unique(dtt$b), unique(dtt$sA))
	ggplot(dtt, aes(m, lambda, colour = type)) + geom_point(size=3.2) + opts(title=ptitle)
}
# ----------------------------------
tplot.cv <- function(dtt, cutoff = NULL) {
	ptitle <- sprintf("(%s,%s), sA=%s, F=2",unique(dtt$a), unique(dtt$b), unique(dtt$sA))
	if(is.null(cutoff)) {
	ggplot(dtt, aes(m, lambda, colour = cv)) + geom_point(size=3.2) + opts(title=ptitle) } else {
ggplot(subset(dtt, cv > cutoff), aes(m, lambda, colour = cv)) + geom_point(size=3.2) + opts(title=ptitle)
	}
}


tplot.cv.corr <- function(dtt, cutoff = NULL) {
	ptitle <- sprintf("(%s,%s), sA=%s, F=2",unique(dtt$a), unique(dtt$b), unique(dtt$sA))
	if(is.null(cutoff)) {
	ggplot(dtt, aes(m, lambda, colour = cv)) + geom_point(size=3.2) + opts(title=ptitle) } else {
		ggplot(subset(dtt, cv > cutoff), aes(m, lambda, colour = cv)) + geom_point(size=3.2) + opts(title=ptitle)

	}
}



gen_plot <- function(p) {
	return(grid.arrange(tplot(o_curves(p)), tplot.cv(j_curves(p)),tplot.cv.corr(c_curves(p))))
}

gen_curve <- function(p) {
	ggplot(split_data[[p]], aes(cv, mstar, colour = type)) + geom_point(size=3.2) + facet_wrap(~corr) + scale_colour_brewer("type",palette="Set1")
}

