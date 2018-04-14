GWsignif <- function (pvalues, files = NULL, readFun = read.table, header = FALSE, 
		ntest.genome, K = 5, alpha = 0.05, plot.it = TRUE) 
{
ngroup <- K
if (is.null(files)) {
	pvalues <- as.matrix(pvalues)
	ntest.region <- ncol(pvalues)
 	if (missing(ntest.genome)) ntest.genome <- ntest.region
	ngroup <- min(ngroup, floor(1 + log(ntest.region)/log(4)))
	m1 <- ceiling(ntest.region/(2^(ngroup - 1)))
	stopifnot(ngroup > 1)

	minp <- matrix(NA, nrow(pvalues), 2^(ngroup - 1))

	nregion <- floor(ncol(pvalues)/m1)
	stopifnot(nregion > 0)
	testj <- 0
	for (j in 1:nregion) {
		testj <- testj + 1
		minp[, testj] <- apply(pvalues[, (1 + (j - 1) * m1):(j * m1)], 1, min, na.rm = TRUE)
		}
	if (ncol(pvalues) - nregion * m1 > 0) {
		testj <- testj + 1
		minp[, testj] <- apply(pvalues[, (ncol(pvalues) - m1 + 1):ncol(pvalues), drop = FALSE], 1, min, na.rm = TRUE)
		}

} else {
	ntest.region <- 0
	for (f in files) {
		pvalues <- readFun(f, header = header, nrows = 2)
		ntest.region <- ntest.region + ncol(pvalues)
		}
 	if (missing(ntest.genome)) ntest.genome <- ntest.region
	ngroup <- min(ngroup, floor(1 + log(ntest.region)/log(4)))
	m1 <- ceiling(ntest.region/(2^(ngroup - 1)))
	stopifnot(ngroup > 1)

	pvalues <- readFun(f, header = header)
	minp <- matrix(NA, nrow(pvalues), 2^(ngroup - 1))

	pval0 <- NULL
	testj <- 0
	for (f in files) {
		pvalues <- as.matrix(readFun(f, header = header))
		pvalues <- cbind(pval0, pvalues)
		nregion <- floor(ncol(pvalues)/m1)
		if (nregion > 0) {
            for (j in 1:nregion) {
			testj <- testj + 1
			minp[, testj] <- apply(pvalues[, (1 + (j - 1) * m1):(j * m1)], 1, min, na.rm = TRUE)
			}
			}
		if (ncol(pvalues) - nregion * m1 > 0) {
			pval0 <- pvalues[, (1 + nregion * m1):ncol(pvalues), drop = FALSE]
			} else {
			pval0 <- NULL
			}
		}

	if (ncol(pvalues) - nregion * m1 > 0) {
		testj <- testj + 1
		minp[, testj] <- apply(pvalues[, max(1, ncol(pvalues) - m1 + 1):ncol(pvalues), drop = FALSE], 1, min, na.rm = TRUE)
    		}

}

qminp <- list()
qminp[[1]] <- apply(minp, 2, quantile, prob = alpha)
k <- 2
while (k < ngroup) {
	minp <- pmin(minp[, seq(1, ncol(minp), by = 2)], minp[, seq(2, ncol(minp), by = 2)])
	qminp[[k]] <- apply(minp, 2, quantile, prob = alpha)
	k <- k + 1
	}
minp <- pmin(minp[, 1], minp[, 2])
qminp[[k]] <- quantile(minp, prob = alpha)

ntest <- c(m1 * 2^(0:(ngroup - 2)), ntest.region)
x <- -log10(alpha/ntest)
logqminp <- lapply(qminp, log10)
y <- -unlist(lapply(logqminp, mean))
ysd <- unlist(lapply(logqminp, sd))
mlogq <- cbind(ntest = ntest, bonf = x, mean = y, sd = ysd)
fit <- lm(y ~ x)
xgw <- -log10(alpha/ntest.genome)
ygw <- predict(fit, new = data.frame(x = xgw))
signifgw <- as.numeric(10^(-ygw))
cat(paste0("\nThe number of tests in a large genome-wide region of interest: ntest.genome=", 
	ntest.genome, ".\nThe significance threshold in the large genome-wide region: GWsignif.threshold=", 
	signif(signifgw), ".\n"))

if (plot.it) {
	qk <- unlist(qminp)
	nk <- NULL
	for (k in 1:ngroup) nk <- c(nk, rep(ntest[k], 2^(ngroup - k)))
		xk <- -log10(alpha/nk)
		yk <- -log10(qk)
		plot(xk, yk, xlim = range(c(x, xk, xgw)), 
			ylim = range(c(y, y + ysd, y - ysd, yk, ygw), na.rm = TRUE), col = "gray", 
            	cex = 0.8, xlab = paste0("-log10(FWER / number of tests), FWER=", alpha), ylab = "-log10(significance threshold)")
		abline(0, 1, col = "gray")
        	points(x, y, pch = 16, col = "blue")
        	points(xgw, ygw, pch = 8, col = "red", cex = 1.2)
        	ypre <- predict(fit)
        	lines(c(x, xgw), c(ypre, ygw), lty = "dashed")
        	co <- round(coefficients(fit), digits = 4)
        	lege = paste("y = ", co[1], " + ", co[2], "x", sep = "")
        	legend("topleft", lege, lty = "dashed", lwd = 1, cex = 1, 
            text.col = "black", merge = TRUE, bg = "gray90")
        	cat("\n--------------\nIn the figure:\nGray circles represent sub-regions significance thresholds.\nBlue dots are the mean of -log10(significance thresholds) in the sub-regions of same size.\nRed star represents the extrapolated significance threshold in a large genome-wide region of interest.\nThe dashed line is fit to the blue solid dots. The grey line is the line of equality, y=x.     \n\n")
		}

list(qminp = qminp, mlogq = mlogq, alpha = alpha, ntest.region = ntest.region, 
	ntest.genome = ntest.genome, GWsignif.threshold = signifgw)
}

