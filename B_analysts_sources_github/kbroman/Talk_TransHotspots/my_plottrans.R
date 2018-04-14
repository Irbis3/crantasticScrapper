my_plottrans.LDA <-
function (Y, geno, nonrecomb, max.p = 100, ...)
{
    suppressMessages(require(MASS)) || stop("the required package 'MASS' is not installed. ")
    Y <- Y[, 1:min(max.p, ncol(Y))]
    fit <- lda(Y[nonrecomb, ], grouping = geno[nonrecomb])
    pred <- predict(fit, Y[-nonrecomb, ])
    sca <- fit$scaling
    y.lda <- Y %*% sca
    orange <- rgb(230, 159, 0, maxColorValue = 256)
    green <- rgb(27, 159, 120, maxColorValue = 256)
    blue <- rgb(123, 104, 238, maxColorValue = 256)
    yellow <- rgb(255, 255, 0, maxColorValue = 256)
    genecolor <- c(blue, orange, green, yellow)
    Class <- geno
    Class[-nonrecomb] <- 4
    xlim <- range(y.lda[, 1])
    ylim <- range(y.lda[, 2])
    px <- pretty(xlim)
    py <- pretty(ylim)
    grayplot(x = y.lda[, 1], y = y.lda[, 2], pch = 21, xat = px,
        yat = py, col = "black", bg = genecolor[Class], hlines = py,
        vlines = px, xaxt = "n", yaxt = "n", xaxs = "r", yaxs = "r",
        xlim = xlim, ylim = ylim, xlab = "Linear Discriminant 1",
        ylab = "Linear Discriminant 2", mgp = c(1.6, 0.2, 0), cex = 0.8,
        ...)
}

my_plottrans.PCA <-
function (Y, geno, nonrecomb, max.p = 100, ...)
{
    Y <- Y[, 1:min(max.p, ncol(Y))]
    PC <- predict(princomp(Y))[, 1:2]
    blue <- rgb(123, 104, 238, maxColorValue = 256)
    orange <- rgb(230, 159, 0, maxColorValue = 256)
    green <- rgb(27, 159, 120, maxColorValue = 256)
    yellow <- rgb(255, 255, 0, maxColorValue = 256)
    genecolor <- c(blue, orange, green, yellow)
    Class <- geno
    Class[-nonrecomb] <- 4
    xlim <- range(PC[, 1])
    ylim <- range(PC[, 2])
    px <- pretty(xlim)
    py <- pretty(ylim)
    grayplot(x = PC[, 1], y = PC[, 2], pch = 21, xat = px, yat = py,
        col = "black", bg = genecolor[Class], hlines = py, vlines = px,
        xaxt = "n", yaxt = "n", xaxs = "r", yaxs = "r", xlim = xlim,
        ylim = ylim, xlab = "Principal Component 1", ylab = "Principal Component 2",
        mgp = c(1.9, 0.2, 0), cex = 0.8, ...)
}
