#'plot
#'
#'plot produces the graph of the goodness-of-fit statistic which is based on the statistic Y.
#'Also produces a graph of the deviance-type residuals versus the fitted values for the location model.
#'Under the presence of a right-censored sample,
#'the function plot() produces a graph of the survival function of the error distribution.
#'
#' @param x an object of the class sglg. This object is returned from the call to glg(), sglg(), survglg() or ssurvglg().
#' @param ... other arguments.

# @references Carlos A. Cardozo, G. Paula and L. Vanegas.
# Semi-parametric generalized log-gamma regression models.  In
# preparation.  @references Carlos A.  Cardozo, G. Paula and L.  Vanegas.
# Semi-parametric accelerated failure time models with generalized
# log-gamma erros: Censored case. In preparation.  @author Carlos Alberto
# Cardozo Delgado <cardozorpackages@gmail.com>, G.  Paula and L.
# Vanegas.
#' @export
plot.sglg <- function(x, ...) {
    par(mfrow = c(1, 2))
    
    lambda <- x$lambda
    rord <- x$rord
    rdev <- x$rdev
    y_est <- x$y_est
    
    if (x$censored == FALSE) {
        
        Fs <- ploggamma(rord, lambda = lambda)
        equantil <- qnorm(Fs)
        diff <- qqnorm(equantil, main = "Overall goodness-of-fit", cex = 0.3, 
            lwd = 3, xlab = "Quantiles of N(0,1)", ylab = "Overall residuals")
        abline(0, 1, col = 2)
        
        C <- rep(3, length(y_est))
        graphics::plot(y_est, rdev, main = "Deviance residuals", xlab = "Fitted values", 
            ylab = "Deviance-type residuals", ylim = c(-3.2, 3.2), pch = 20)
        abline(h = C, col = 2)
        abline(h = -C, col = 2)
    }
    
    if (x$censored == TRUE) {
        
        delta <- x$delta
        # par(mfrow = c(1, 3))
        
        C <- rep(3, length(y_est))
        plot(y_est, rdev, main = "Deviance residuals", xlab = "Fitted values", 
            ylab = "Deviance-type residuals", ylim = c(-3.2, 3.2), pch = 20)
        abline(h = C, col = 2)
        abline(h = -C, col = 2)
        
        ekm <- survfit(Surv(exp(rord), 1 - delta) ~ 1)
        # ftimes <- as.numeric(unlist(as.vector(summary(ekm)[2])))
        surv <- as.numeric(unlist(as.vector(summary(ekm)[6])))
        Fkm <- 1 - surv
        # plot(ftimes, surv, xlab = 'Multiplicative error', ylab = 'Survival
        # values', main = 'Survival function', type = 'l', pch = 20)
        
        res <- sort((rord * (1 - delta))[delta == 0])
        Fs <- ploggamma(res, lambda = lambda)
        r_q <- qnorm(Fs)
        diff <- abs(r_q - qnorm(Fkm))
        output <- mean(diff[-length(diff)])
        qqnorm(r_q, xlab = "Quantiles of N(0,1)", ylab = "Overall residuals", 
            main = "Overall goodness-of-fit", pch = 20)
        qqline(r_q, col = 2)
    }
}
