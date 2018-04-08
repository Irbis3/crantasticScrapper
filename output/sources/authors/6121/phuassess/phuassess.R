phuassess <- function(used, avail, hnames = FALSE, exactperm = TRUE, nrperm = 1e+05,
    alpha = 0.05) {

    used <- as.matrix(used)
    avail <- as.matrix(avail)

    if (ncol(used) != ncol(avail)) {
        stop("the two matrices should have the same dimensions")
    }

    if (nrow(used) != nrow(avail)) {

        stop("the two matrices should have the same dimensions")
    }

    if (!all(colnames(used) == colnames(avail)))
        stop("the two matrices should have the same habitat names")

    if (hnames == FALSE) {
        colnames(used) <- paste("Habitat ", 1:ncol(used), sep = "")
        colnames(avail) <- paste("Habitat ", 1:ncol(avail), sep = "")
    }

    diffm <- used - avail
    nh <- ncol(used)
    na <- nrow(used)
    nperm <- ifelse(exactperm == TRUE, 2^na, nrperm)

    if (exactperm == TRUE && na > 31)
        stop("number of animals exceeding 31 - only a sample of permutations can be performed")

    r <- rep(0, na)
    s <- rep(-100, na)
    classif <- rep("PROPORTIONALLY USED", nh)
    pvalue.overall <- 0
    noss <- colSums(diffm != 0)
    npos <- colSums(diffm > 0)

    if (any(noss == 0))
        stop("Analysis cannot be performed: zero observations for performing sign test")
    if (any(noss <= 5))
        warning("Warning: less than six observations for performing sign test")


    f <- npos/noss
    t <- pmax(npos, noss - npos)
    mint <- ifelse(noss%%2 == 0, noss/2, (noss + 1)/2)
    margpvalue <- matrix(0, 1, nh, byrow = FALSE, dimnames = list(c("marginalpvalue"),
        colnames(used)))

    for (i in 1:nh) {
        margpvalue[i] <- sum(dbinom(t[i]:noss[i], noss[i], 0.5))
        margpvalue[i] <- ifelse(t[i] != mint[i], 2 * margpvalue[i], 1)
        if (margpvalue[i] <= alpha) {
            classif[i] <- ifelse(f[i] >= 0.5, "PREFERRED", "AVOIDED")
        }
    }

    minp <- min(margpvalue)
    h.preferred <- names(f[which(classif == "PREFERRED")])
    h.avoided <- names(f[which(classif == "AVOIDED")])
    h.proport <- names(f[which(classif == "PROPORTIONALLY USED")])

    f.preferred <- subset(f, classif == "PREFERRED")
    f.avoided <- subset(f, classif == "AVOIDED")
    f.proport <- subset(f, classif == "PROPORTIONALLY USED")

    noss.preferred <- subset(noss, classif == "PREFERRED")
    npos.preferred <- subset(npos, classif == "PREFERRED")
    noss.avoided <- subset(noss, classif == "AVOIDED")
    npos.avoided <- subset(npos, classif == "AVOIDED")

    t.preferred <- abs((npos.preferred %*% t(noss.preferred)) - t(npos.preferred %*%
        t(noss.preferred)))
    t.avoided <- abs((npos.avoided %*% t(noss.avoided)) - t(npos.avoided %*% t(noss.avoided)))

    pvalue.overall <- 0
    pvalue.preferred <- rep(0, length(f.preferred))
    pvalue.avoided <- rep(0, length(f.avoided))

    nh.preferred <- sum(classif == "PREFERRED")
    nh.avoided <- sum(classif == "AVOIDED")

    for (i in 1:nperm) {

        if (exactperm == TRUE) {

            for (j in 1:na) {
                s[j] <- as.integer(((i - 1)/(2^(na - j))))%%2
                r[j] <- ifelse(s[j] == 0, -1, 1)
            }

        } else {
            r <- ifelse(runif(na) < 0.5, -1, 1)
        }

        diffm.p <- diffm * r
        npos.p <- colSums(diffm.p > 0)
        f.p <- npos.p/noss
        t.p <- pmax(npos.p, noss - npos.p)
        mint.p <- ifelse(noss%%2 == 0, noss/2, (noss + 1)/2)
        margpvalue.p <- matrix(0, 1, nh)

        for (i in 1:nh) {
            margpvalue.p[i] <- sum(dbinom(t.p[i]:noss[i], noss[i], 0.5))
            margpvalue.p[i] <- ifelse(t.p[i] != mint.p[i], 2 * margpvalue.p[i], 1)
        }

        minp.p <- min(margpvalue.p)
        if (minp >= minp.p)
            pvalue.overall <- pvalue.overall + 1

        if (nh.preferred > 1) {
            npos.preferred.p <- subset(npos.p, classif == "PREFERRED")
            t.preferred.p <- abs((npos.preferred.p %*% t(noss.preferred)) - t(npos.preferred.p %*%
                t(noss.preferred)))
            pvalue.preferred <- ifelse((t.preferred.p >= t.preferred), pvalue.preferred +
                1, pvalue.preferred)
        }

        if (nh.avoided > 1) {
            npos.avoided.p <- subset(npos.p, classif == "AVOIDED")
            t.avoided.p <- abs((npos.avoided.p %*% t(noss.avoided)) - t(npos.avoided.p %*%
                t(noss.avoided)))
            pvalue.avoided <- ifelse((t.avoided.p >= t.avoided), pvalue.avoided +
                1, pvalue.avoided)
        }
    }

    pvalue.overall <- pvalue.overall/nperm
    if (nh.preferred > 1)
        pvalue.preferred <- pvalue.preferred/(nperm * 1)
    if (nh.avoided > 1)
        pvalue.avoided <- pvalue.avoided/(nperm * 1)

    f.out <- c(sort(as.vector(f.preferred), decreasing = T), sort(as.vector(f.proport),
        decreasing = T), sort(as.vector(f.avoided), decreasing = T))

    h.preferred <- names(sort(f.preferred, decreasing = T))
    h.proport <- names(sort(f.proport, decreasing = T))
    h.avoided <- names(sort(f.avoided, decreasing = T))

    habtype.out <- c(h.preferred, h.proport, h.avoided)
    matrpvalue.out <- diag(margpvalue[, habtype.out])
    rownames(matrpvalue.out) <- habtype.out
    colnames(matrpvalue.out) <- habtype.out
    matrpvalue.out[-which(matrpvalue.out %in% diag(matrpvalue.out))] <- "-"
    matrpvalue.out

    pvalue.preferred <- as.matrix(pvalue.preferred)
    rownames(pvalue.preferred) <- colnames(pvalue.preferred)
    if (nh.preferred > 1) {
        pvalue.preferred <- pvalue.preferred[h.preferred, h.preferred]
        matrpvalue.out[h.preferred, h.preferred][which(upper.tri(matrpvalue.out[h.preferred,
            h.preferred]))] <- pvalue.preferred[which(upper.tri(pvalue.preferred))]
    }

    pvalue.avoided <- as.matrix(pvalue.avoided)
    rownames(pvalue.avoided) <- colnames(pvalue.avoided)

    if (nh.avoided > 1) {
        pvalue.avoided <- pvalue.avoided[h.avoided, h.avoided]
        matrpvalue.out[h.avoided, h.avoided][which(upper.tri(matrpvalue.out[h.avoided,
            h.avoided]))] <- pvalue.avoided[which(upper.tri(pvalue.avoided))]
    }

    classif.out <- c(rep("preferred", nh.preferred), rep("proportionally used", nh - (nh.preferred +
        nh.avoided)), rep("avoided", nh.avoided))

    f.out <- format(round(f.out, digits = 4), nsmall = 4)
    matrpvalue.out[-which(matrpvalue.out == "-")] <- format(round(as.numeric(matrpvalue.out[-which(matrpvalue.out ==
        "-")]), 4), nsmall = 4)
    ordering <- data.frame(cbind(habtype.out, f.out, matrpvalue.out, classif.out),
        row.names = NULL)
    colnames(ordering) <- c("Habitat Type", "f", habtype.out, "Decision")
    type.perm <- ifelse(exactperm == TRUE, "exact permutation distribution", "estimated permutation distribution")
    return(list(used = used, avail = avail, pvalue.overall = format(round(pvalue.overall,
        digits = 5), nsmall = 5), ordering = ordering, noss = noss, npos = npos,
        type.perm = type.perm, nperm = nperm, alpha = alpha))

}
