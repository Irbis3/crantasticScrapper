#' summary.sglg
#'
#' summary.sglg extracts displays the summary of the fitted model including parameter estimates, associated (approximated) standard errors and goodness-of-fit statistics from a model from an object of class 'sglg'.
#' @param object an object of the class sglg. This object is returned from the call to glg(), sglg(), survglg() or ssurvglg() function.
#' @param ... other arguments.
#' @export
summary.sglg <- function(object, ...) {
    if (object$semi == FALSE & object$censored == FALSE) {
        cat(" -------------------------------------------------------------- ")
        cat("\n Semi-parametric generalized log-gamma regression model \n")
        cat(" --------------------------------------------------------------\n")
        cat(" Sample size: ", length(object$y_est))
        cat(" Censored: ")
        print(object$censored)
        cat("\n ------------------------ Location model ---------------------- \n\n")
        
        cat(" ---------- Parametric component ----------\n\n")
        p <- object$p
        Estimate <- object$mu
        StdErr <- object$st_error[1:p]
        tval <- Estimate/StdErr
        p.value <- object$p.values[1:p]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames(object$X)
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat("\n -------------------- Scale parameter -------------------- \n\n")
        Estimate <- object$sigma
        StdErr <- object$st_error[p + 1]
        tval <- Estimate/StdErr
        p.value <- object$p.values[p + 1]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames("sigma")
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat("\n -------------------- Shape parameter -------------------- \n\n")
        Estimate <- object$lambda
        StdErr <- object$st_error[p + 2]
        tval <- Estimate/StdErr
        p.value <- object$p.values[p + 2]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames("lambda")
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = TRUE, tst.ind = c(2, 3))
        cat(" ------------------------------------------------------------\n\n")
        cat(" Deviance: ", round(object$deviance, digits = 2), "\n\n")
        cat(" ------ Goodness-of-fit ------\n\n")
        cat(" Overall statistic: ", round(object$goodnessoffit, digits = 3), 
            "\n\n")
        cat(" ------ Penalized Log-likelihood ------\n\n")
        cat(" Log-lik: ", round(object$llglg, digits = 2), "\n\n")
        cat(" ------ Information criterion ------\n\n")
        cat(" AIC: ", round(object$AIC, digits = 2), "\n")
        cat(" BIC: ", round(object$BIC, digits = 2), "\n\n")
        cat(" --------------------------------------------------------------\n")
    }
    if (object$semi == FALSE & object$censored == TRUE) {
        cat(" -------------------------------------------------------------- ")
        cat("\n Semi-parametric generalized log-gamma regression model \n")
        cat(" --------------------------------------------------------------\n")
        cat(" Sample size: ", length(object$y_est))
        cat(" Censored: ")
        print(object$censored)
        cat(" Percentage of censored observations: ")
        print(object$per.censo)
        cat("\n ------------------------ Location model ---------------------- \n\n")
        
        cat(" ---------- Parametric component ----------\n\n")
        p <- object$p
        Estimate <- object$mu
        StdErr <- object$st_error[1:p]
        tval <- Estimate/StdErr
        p.value <- object$p.values[1:p]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames(object$X)
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat("\n -------------------- Scale parameter -------------------- \n\n")
        Estimate <- object$sigma
        StdErr <- object$st_error[p + 1]
        tval <- Estimate/StdErr
        p.value <- object$p.values[p + 1]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames("sigma")
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat(" ------------------------------------------------------------\n\n")
        cat(" Shape: ", round(object$lambda, digits = 2), "\n\n")
        cat(" Deviance: ", round(object$deviance, digits = 2), "\n\n")
        cat(" ------ Goodness-of-fit ------\n\n")
        cat(" Overall statistic: ", round(object$goodnessoffit, digits = 3), 
            "\n\n")
        cat(" ------ Penalized Log-likelihood ------\n\n")
        cat(" Log-lik: ", round(object$llglg, digits = 2), "\n\n")
        cat(" ------ Information criterion ------\n\n")
        cat(" AIC: ", round(object$AIC, digits = 2), "\n")
        cat(" BIC: ", round(object$BIC, digits = 2), "\n\n")
        cat(" --------------------------------------------------------------\n")
    }
    if (object$semi == TRUE & object$censored == FALSE) {
        cat(" -------------------------------------------------------------- ")
        cat("\n Semi-parametric generalized log-gamma regression model \n")
        cat(" --------------------------------------------------------------\n")
        cat(" Sample size: ", length(object$y_est))
        cat(" Censored: ")
        print(object$censored)
        cat("\n ------------------------ Location model ---------------------- \n\n")
        
        cat(" ---------- Parametric component ----------\n\n")
        p <- object$p
        Estimate <- object$mu[1:p]
        StdErr <- object$st_error[1:p]
        tval <- Estimate/StdErr
        p.value <- object$p.values[1:p]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames(object$X)
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat(" ---------- Non-parametric component ----------\n\n")
        Knot <- object$Knot
        Smoothp <- object$alpha
        d.f <- object$d.f.npc
        BasisD <- Knot
        table <- cbind(Smoothp, BasisD, d.f)
        colnames(table) <- c("Smooth parameter", "Basis dimension", "d.f")
        rownames(table) <- colnames(object$npc)
        printCoefmat(table)
        cat("\n -------------------- Scale parameter -------------------- \n\n")
        Estimate <- object$sigma
        StdErr <- object$st_error[p + Knot + 1]
        tval <- Estimate/StdErr
        p.value <- object$p.values[p + Knot + 1]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames("sigma")
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat("\n -------------------- Shape parameter -------------------- \n\n")
        Estimate <- object$lambda
        StdErr <- object$st_error[p + Knot + 2]
        tval <- Estimate/StdErr
        p.value <- object$p.values[p + Knot + 2]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames("lambda")
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = TRUE, tst.ind = c(2, 3))
        cat(" ------------------------------------------------------------\n\n")
        cat(" Deviance: ", round(object$deviance, digits = 2), "\n\n")
        cat(" ------ Goodness-of-fit ------\n\n")
        cat(" Overall statistic: ", round(object$goodnessoffit, digits = 3), 
            "\n\n")
        cat(" ------ Penalized Log-likelihood ------\n\n")
        cat(" Log-lik: ", round(object$llglg, digits = 2), "\n\n")
        cat(" ------ Information criterion ------\n\n")
        cat(" AIC: ", round(object$AIC, digits = 2), "\n")
        cat(" BIC: ", round(object$BIC, digits = 2), "\n\n")
        cat(" --------------------------------------------------------------\n")
    }
    if (object$semi == TRUE & object$censored == TRUE) {
        cat(" -------------------------------------------------------------- ")
        cat("\n Semi-parametric generalized log-gamma regression model \n")
        cat(" --------------------------------------------------------------\n")
        cat(" Sample size: ", length(object$y_est))
        cat(" Censored: ")
        print(object$censored)
        cat("\n ------------------------ Location model ---------------------- \n\n")
        
        cat(" ---------- Parametric component ----------\n\n")
        p <- object$p
        Estimate <- object$mu[1:p]
        StdErr <- object$st_error[1:p]
        tval <- Estimate/StdErr
        p.value <- object$p.values[1:p]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames(object$X)
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat(" ---------- Non-parametric component ----------\n\n")
        Knot <- object$Knot
        Smoothp <- object$alpha
        d.f <- object$d.f.npc
        BasisD <- Knot
        table <- cbind(Smoothp, BasisD, d.f)
        colnames(table) <- c("Smooth parameter", "Basis dimension", "d.f")
        rownames(table) <- colnames(object$npc)
        printCoefmat(table)
        cat("\n -------------------- Scale parameter -------------------- \n\n")
        Estimate <- object$sigma
        StdErr <- object$st_error[p + Knot + 1]
        tval <- Estimate/StdErr
        p.value <- object$p.values[p + Knot + 1]
        table <- cbind(Estimate, StdErr, tval, p.value)
        colnames(table) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
        rownames(table) <- colnames("sigma")
        printCoefmat(table, P.values = TRUE, has.Pvalue = TRUE, digits = 5, 
            signif.legend = FALSE, tst.ind = c(2, 3))
        cat(" ------------------------------------------------------------\n\n")
        cat(" Shape: ", round(object$lambda, digits = 2), "\n\n")
        cat(" Deviance: ", round(object$deviance, digits = 2), "\n\n")
        cat(" ------ Goodness-of-fit ------\n\n")
        cat(" Overall statistic: ", round(object$goodnessoffit, digits = 3), 
            "\n\n")
        cat(" ------ Penalized Log-likelihood ------\n\n")
        cat(" Log-lik: ", round(object$llglg, digits = 2), "\n\n")
        cat(" ------ Information criterion ------\n\n")
        cat(" AIC: ", round(object$AIC, digits = 2), "\n")
        cat(" BIC: ", round(object$BIC, digits = 2), "\n\n")
        cat(" --------------------------------------------------------------\n")
    }
    
}
