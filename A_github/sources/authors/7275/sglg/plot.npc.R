#'Plotting a natural cubic splines or P-splines.
#'
#'plotnpc displays a graph of a fitted nonparametric effect, either natural cubic spline or P-spline, from an object of class sglg.
#'
#' @param fit an object of the class sglg. This object is returned from the call to glg(), sglg(), survglg() or ssurvglg().

#' @references Eilers P.H.C. and Marx B.D. (1996). Flexible smoothing with B-splines and penalties. Statistical Science. 11, 89-121.
#' @references Wood, S. (2006). Additive generalized models: An R introduction. Chapman and Hall.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>, G. Paula and L. Vanegas.
#' @import graphics

plot.npc <- function(fit) {
    if (fit$semi == FALSE) {
        stop("Sorry, for this kind of model it is not available this option.")
    }
    npc <- fit$npc
    y <- fit$y
    X <- fit$X
    p <- fit$p
    Knot <- fit$Knot
    betas <- fit$mu[1:p]
    as <- fit$mu[(p + 1):(p + Knot)]
    lambda <- fit$lambda
    rord <- fit$rord
    rdev <- fit$rdev
    y_est <- fit$y_est2
    scovar <- fit$scovar
    t_npc <- as.numeric(levels(factor(as.matrix(npc))))
    N_t <- deBoor2(t_npc, Knot)$N
    g_t <- N_t %*% as
    scovarred <- scovar[(p + 1):(p + Knot), (p + 1):(p + Knot)]
    Var <- N_t %*% (scovarred %*% t(N_t))
    nval <- diag(Var)
    nste <- sqrt(nval)
    n1 <- length(nste)
    percentil <- 0.025/n1
    quantil <- abs(qnorm(percentil))
    upper_g_t <- g_t + quantil * nste
    lower_g_t <- g_t - quantil * nste
    
    cloud <- y - X %*% betas
    xrange <- range(t_npc)
    yrange <- range(c(lower_g_t, upper_g_t))
    plot(as.matrix(npc), cloud, ylim = yrange, col = 3, pch = 20, xlab = colnames(npc), 
        ylab = "g(x)", main = "Simultaneous 95% confidence intervals")
    
    f <- splinefun(t_npc, g_t, method = "natural")
    ls(envir = environment(f))
    splinecoef <- get("z", envir = environment(f))
    values <- curve(f, min(t_npc), max(t_npc), col = "black", add = TRUE)
    
    uf <- splinefun(t_npc, upper_g_t, method = "natural")
    ls(envir <- environment(uf))
    splinecoef <- get("z", envir = environment(uf))
    values <- curve(uf, min(t_npc), max(t_npc), col = "red", add = TRUE)
    
    lf <- splinefun(t_npc, lower_g_t, method = "natural")
    ls(envir <- environment(lf))
    splinecoef <- get("z", envir = environment(lf))
    values <- curve(lf, min(t_npc), max(t_npc), col = "red", add = TRUE)
}
