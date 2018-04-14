myplot_formal <-
function (x, xlab = "Map position (cM)", ylab = "LOD", mgp = c(1.6,
    0.2, 0), ...)
{
    if (!any(class(x) == "testpleio.1vs2"))
        stop("Input should have class \"testpleio.1vs2\".")
    LOD1 <- x$LOD1
    LOD2 <- x$LOD2
    LODdiff <- x$LODdiff
    maxPOS <- x$maxPOS
    maxLOD <- x$maxLOD
    Group <- x$Group
    pvalue <- x$pvalue
    map <- x$map
    map.marker <- x$map.marker
    rg <- range(map)
    ylim <- c(0, max(maxLOD, LOD1, LOD2, na.rm = TRUE)*1.05)
    grayplot(y = LOD1, x = map, type = "l", ylim = ylim, xlim = rg,
             xaxt = "n", xlab = xlab, ylab = ylab, mgp = mgp, lwd=2,
             yat=pretty(ylim), yaxs="i", ...)
    rug(map.marker, ticksize = -0.01)
    points(maxPOS, maxLOD, col = c("slateblue", "violetred")[Group], pch = 20)
    points(x = attr(LODdiff, "LOD1pos"), y = attr(LODdiff, "LOD1lod"),
        col = "black", pch = 17)
    points(y = c(2, 2), x = attr(LODdiff, "LOD2pos"), col = c("slateblue",
        "violetred"), pch = 17)
    ind <- arrayInd(which.max(LOD2), .dim = dim(LOD2))
    lines(map, LOD2[, ind[2]], col = "slateblue", lwd=2)
    lines(map, LOD2[ind[1], ], col = "violetred", lwd=2)
}


myplot_formal_trace <-
function (x, xlab = "i.cut", ylab = "LODdiff", mgp = c(1.6, 0.2,
    0), ...)
{
    object <- x
    if (!any(class(object) == "testpleio.1vs2"))
        stop("Input should have class \"testpleio.1vs2\".")
    t <- object$LODdiff.trace
    t[t < 0] <- 0
    ylim <- c(0, max(t, na.rm = TRUE)*1.05)
    grayplot(1:length(t), t, type = "n", xlab = xlab, ylab = ylab,
             ylim = ylim, yaxs="i", mgp = mgp,
             xlim=c(0.5, length(t)+0.5), xaxs="i",  ...)
    n.max <- which.max(t)
    v.max <- max(t, na.rm = TRUE)
    abline(v=n.max, lwd=2, col="violetred")
    lines(seq(along=t), t, type="o", pch=21, bg="slateblue", cex=0.8)
}
