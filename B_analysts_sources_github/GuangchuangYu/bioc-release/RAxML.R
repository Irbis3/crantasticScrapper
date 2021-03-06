##' parse RAxML bootstrapping analysis output
##'
##'
##' @title read.raxml
##' @param file RAxML bootstrapping analysis output
##' @return raxml object
##' @export
##' @examples
##' raxml_file <- system.file("extdata/RAxML", "RAxML_bipartitionsBranchLabels.H3", package="treeio")
##' read.raxml(raxml_file)
##' @author Guangchuang Yu
read.raxml <- function(file) {
    tree.text <- readLines(file)
    tree_text <- gsub('(:[0-9\\.eE+\\-]+)\\[(\\d+)\\]', '\\@\\2\\1', tree.text)
    phylo <- read.tree(text=tree_text)
    if(any(grepl('@', phylo$node.label))) {
        bootstrap <- as.numeric(gsub("[^@]*@(\\d+)", "\\1", phylo$node.label))
        phylo$node.label <- gsub("@\\d+", "", phylo$node.label)
    }

    if (all(phylo$node.label == "")) {
        phylo$node.label <- NULL
    }

    bootstrap <- data.frame(node = Ntip(phylo) + 1:phylo$Nnode,
                            bootstrap = bootstrap)

    ## new("raxml",
    ##     file      = file,
    ##     fields    = "bootstrap",
    ##     treetext  = tree.text,
    ##     phylo     = phylo,
    ##     bootstrap = bootstrap
    ##     )

    new("treedata",
        file = filename(file),
        treetext = tree.text,
        phylo = phylo,
        data = bootstrap
        )
}


## ##' @rdname groupOTU-methods
## ##' @exportMethod groupOTU
## setMethod("groupOTU", signature(object="raxml"),
##           function(object, focus, group_name="group") {
##               groupOTU_(object, focus, group_name)
##           }
##           )

## ##' @rdname groupClade-methods
## ##' @exportMethod groupClade
## setMethod("groupClade", signature(object="raxml"),
##           function(object, node, group_name="group") {
##               groupClade_(object, node, group_name)
##           })

## ##' @rdname scale_color-methods
## ##' @exportMethod scale_color
## setMethod("scale_color", signature(object="raxml"),
##           function(object, by="bootstrap", ...) {
##               scale_color_(object, by, ...)
##           })


## ##' @rdname gzoom-methods
## ##' @exportMethod gzoom
## setMethod("gzoom", signature(object="raxml"),
##           function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
##               gzoom.phylo(get.tree(object), focus, subtree, widths)
##           })

