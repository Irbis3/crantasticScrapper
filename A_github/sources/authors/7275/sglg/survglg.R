#'Fitting linear generalized log-gamma regression models under the presence of right censored data.
#'
#'\code{survglg} is used to fit a multiple linear regression model in which the response variable is continuous, strictly positive, asymmetric and there are right censored observations.
#'In this setup, the location parameter of the logarithm of the response variable is modeled by a linear model of the parametes.
#'
#' @param formula a symbolic description of the systematic component of the model to be fitted. See details for further information.
#' @param data an optional data frame, list containing the variables in the model.
#' @param shape an optional value for the shape parameter of the model.
#' @param Tolerance an optional positive value, which represents the convergence criterion. Default value is 1e-04.
#' @param Maxiter an optional positive integer giving the maximal number of iterations for the estimating process. Default value is 1e03.
#' @return mu a vector of parameter estimates asociated with the location parameter.
#' @return sigma estimate of the scale parameter associated with the model.
#' @return lambda estimate of the shape parameter associated with the model.
#' @return interval estimate of a 95\% confidence interval for each estimate parameters associated with the model.
#' @return Deviance the deviance associated with the model.
#' @references Carlos A. Cardozo, G. Paula and L. Vanegas. Semi-parametric accelerated failure time models with generalized log-gamma erros. In preparation.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>, G. Paula and L. Vanegas.
#' @examples
#' rows  <- 240
#' columns <- 2
#' t_beta  <- c(0.5, 2)
#' t_sigma <- 1
#' t_lambda <- 1
#' set.seed(8142031)
#' library(ssym)
#' x1 <- rbinom(rows, 1, 0.5)
#' x2 <- runif(columns, 0, 1)
#' X <- cbind(x1,x2)
#' s         <- t_sigma^2
#' a         <- 1/s
#' t_ini1    <- exp(X %*% t_beta) * rgamma(rows, scale = s, shape = a)
#' cens.time <- rweibull(rows, 0.3, 14)
#' delta1     <- ifelse(t_ini1 > cens.time, 1, 0)
#' obst1 <- t_ini1
#' for (i in 1:rows) {
#' if (delta1[i] == 1) {
#'    obst1[i] <- cens.time[i]
#'   }
#' }
#' data.example <- data.frame(obst1,delta1,X)
#' fit3 <- survglg(Surv(log(obst1),delta1) ~ x1 + x2 - 1, data=data.example,shape=0.9)
#' summary(fit3)
#' plot(fit3)
#' @import Formula
#' @import survival
#' @import ssym
#' @import methods
#' @export survglg
survglg = function(formula, data, shape, Maxiter, Tolerance) {
    if (missingArg(formula)) {
        stop("The formula argument is missing.")
    }
    if (missingArg(data)) {
        stop("The data argument is missing.")
    }

    if (missingArg(Tolerance))
        Tolerance <- 1e-04
    if (missingArg(Maxiter))
        Maxiter <- 1000
    if (missingArg(shape))
        shape <- 1

    if (class(data) == "list")
        data <- as.data.frame(data)

    data <- model.frame(formula, data = data)

    X <- model.matrix(formula, data = data)
    p <- ncol(X)

    Y <- cbind(data[, 1][, 1], data[, 1][, 2])
    Delta <- as.factor(Y[, 2])
    y <- Y[, 1][Delta == 0]
    XX <- X[Delta == 0, ]
    Delta <- as.numeric(as.vector(Delta))
    per.censo <- 100 * mean(Delta)
    datus <- data.frame(y, XX)

    formula2 <- formula
    formula2 <- Formula(formula2)
    formula2 <- formula(formula2, lhs = 0)
    formula2 <- update(formula2, y ~ .)

    #################################################################################################################################################

    ############################################################################################################################################################

    fit0 <- glg(formula2, data = datus)
    beta0 <- fit0$mu
    sigma0 <- fit0$sigma
    lambda0 <- shape

    n <- nrow(X)

    # Some fixed matrizes

    I_n <- diag(1, n)

    # Defining mu function

    mu <- function(bet) {
        output <- X %*% bet
        return(output)
    }

    ### First step: The residuals

    eps <- function(bet, sigm) {
        epsilon <- (Y[, 1] - mu(bet))/sigm
        return(epsilon)
    }

    ### Second step: The matrix D

    S <- function(y, lambd) {
        s <- pgamma((1/lambd^2) * exp(lambd * y), 1/lambd^2, lower.tail = FALSE)
        return(s)
    }

    ### Third step: The matrix Wd

    aa <- function(bet, sigm, lambd) {
        epsilon <- eps(bet, sigm)
        output <- (1/lambd^2) * exp(lambd * epsilon)
        return(output)
    }

    ## Final step: Score functions

    U_beta <- function(bet, sigm, lambd) {
        epsil <- eps(bet, sigm)
        aaa <- aa(bet, sigm, lambd)
        bb <- (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S(epsil,
            lambd))
        output <- matrix(0, p, 1)
        for (j in 1:p) {
            output[j] <- sum(X[, j] * ((1 - Delta) * (1/(lambd * sigm)) *
                (exp(lambd * epsil) - 1) + Delta * (lambd/sigm) * bb))
        }
        return(output)
    }

    U_sigma <- function(bet, sigm, lambd) {
        epsil <- eps(bet, sigm)
        aaa <- aa(bet, sigm, lambd)
        bb <- (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S(epsil,
            lambd))
        part1 <- (1/sigm) * (-1 - (1/lambd) * (epsil - epsil * exp(lambd *
            epsil)))
        part2 <- (lambd/sigm) * epsil * bb
        output <- sum((1 - Delta) * part1 + Delta * part2)
        return(output)
    }

    U_theta <- function(bet, sigm, lambd) {
        output <- matrix(1, p + 1, 1)
        output[1:p] <- U_beta(bet, sigm, lambd)
        output[p + 1] <- U_sigma(bet, sigm, lambd)
        return(output)
    }

    # Observational Fisher Matrix

    I_beta <- function(bet, sigm, lambd) {
        epsil <- eps(bet, sigm)
        aaa <- aa(bet, sigm, lambd)
        bb <- (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S(epsil,
            lambd))
        output <- matrix(0, p, p)
        for (l in 1:p) {
            for (j in 1:p) {
                output[l, j] <- sum(X[, l] * X[, j] * ((1 - Delta) * (-1/sigm^2) *
                  exp(lambd * epsil) + Delta * ((lambd/sigm)^2) * bb * (aaa -
                  1/lambd^2 - bb)))
            }
        }
        return(output)
    }

    I_sigma <- function(bet, sigm, lambd) {
        epsil <- eps(bet, sigm)
        part1 <- (1/sigm^2) * (1 + (2/lambd) * epsil * (1 - exp(lambd * epsil)) -
            (epsil^2) * exp(lambd * epsil))
        aaa <- aa(bet, sigm, lambd)
        bb <- (aaa^(1/lambd^2) * exp(-aaa))/S(epsil, lambd)
        b <- aaa - (1/lambd^2) - bb/gamma(1/lambd^2)
        part2 <- ((lambd * epsil * bb)/(sigm^2)) * (epsil * b - 2/lambd)
        output <- sum((1 - Delta) * part1 + Delta * part2)
        return(output)
    }

    I_sigbet <- function(bet, sigm, lambd) {
        epsil <- eps(bet, sigm)
        expep <- exp(lambd * epsil)
        H <- matrix(0, p, 1)
        for (k in 1:p) {
            H[k] <- sum((1 - Delta) * X[, k] * (1 - expep - lambd * epsil *
                expep))
        }
        part1 <- (1/(lambd * (sigm^2))) * H
        part2 <- 0 * H
        aaa <- aa(bet, sigm, lambd)
        bb <- (aaa^(1/lambd^2)) * exp(-aaa)
        deno <- gamma(1/lambd^2) * S(epsil, lambd)
        part21 <- -1/lambd + epsil * (aaa - (1/lambd^2) - bb/deno)
        for (k in 1:p) {
            H[k] <- sum(Delta * X[, k] * ((((lambd/sigm)^2) * bb/deno) *
                part21))
        }
        part2 <- H
        output <- part1 + part2
        return(output)
    }

    I_tetha <- function(bet, sigm, lambd) {
        output <- matrix(0, p + 1, p + 1)
        output[1:p, 1:p] <- I_beta(bet, sigm, lambd)
        output[(p + 1), (p + 1)] <- I_sigma(bet, sigm, lambd)
        output[1:p, (p + 1)] <- I_sigbet(bet, sigm, lambd)
        output[(p + 1), 1:p] <- t(output[1:p, (p + 1)])
        return(output)
    }

    ## LOG-LIKELIHOOD

    c_l <- function(lambd) {
        invlambdos <- 1/lambd^2
        c <- abs(lambd)/gamma(invlambdos)
        output <- c * (invlambdos^invlambdos)
        return(output)
    }

    loglikglg <- function(bet, sigm, lambd) {
        epsil <- eps(bet, sigm)
        output <- sum(Delta * log(S(epsil, lambd)) + (1 - Delta) * (log(c_l(lambd)/sigm) +
            (1/lambd) * epsil - (1/lambd^2) * exp(lambd * epsil)))
        return(output)
    }

    newpar <- function(bet, sigm, lambd) {
        output <- matrix(0, p + 1, 2)
        output[, 1] <- c(bet, sigm)
        new <- output[, 1]
        l <- 2
        output[, l] <- output[, (l - 1)] - solve(I_tetha(output[1:p, (l -
            1)], output[(p + 1), (l - 1)], lambd)) %*% U_theta(output[1:p,
            (l - 1)], output[(p + 1), (l - 1)], lambd)
        llglg <- loglikglg(output[1:p, 2], output[p + 1, 2], lambd)
        condition <- llglg - loglikglg(new[1:p], new[p + 1], lambd)

        M <- 2
        while (condition < 0 & M < Maxiter) {
            output[, 2] <- 0.99 * output[, 2] + 0.01 * output[, 1]
            llglg <- loglikglg(output[1:p, 2], output[p + 1, 2], output[p +
                2, 2])
            condition <- llglg - loglikglg(new[1:p], new[p + 1], new[p +
                2])
            M <- M + 1
        }
        if (condition < 0) {
            return(new)
        }

        llglg <- loglikglg(output[1:p, 2], output[p + 1, 2], lambd)
        condition <- llglg - loglikglg(new[1:p], new[p + 1], lambd)
        if (condition > 0) {
            new <- output[, 2]
        }

        return(new)
    }

    gfit <- function(resid, lambd) {
        ekm <- survival::survfit(Surv(exp(resid), 1 - Delta) ~ 1)
        surv <- as.numeric(unlist(as.vector(summary(ekm)[6])))
        Fkm <- 1 - surv

        res <- sort((resid * (1 - Delta))[Delta == 0])
        Fs <- robustloggamma::ploggamma(res, lambda = lambd)
        r_q <- qnorm(Fs)

        diff <- abs(r_q - qnorm(Fkm))
        output <- mean(diff[-length(diff)])
        msurv <- 1 - Fs
        return(list(stat = output, msurv = msurv))
    }

    ## THE MAIN FUNCTION

    conv <- FALSE
    condition <- 1
    iter <- 1
    l <- 1

    optimum <- function(bet, sigm, lambd) {
        new <- matrix(0, p + 1, Maxiter)
        l <- 1
        new[, 1] <- c(beta0, sigma0)
        output <- new[, 1]

        l <- 2
        new[, l] <- newpar(new[1:p, (l - 1)], new[(p + 1), (l - 1)], lambd)

        llglg <- loglikglg(new[1:p, l], new[(p + 1), l], lambd)
        condition <- llglg - loglikglg(output[1:p], output[p + 1], lambd)

        if (condition > 0) {
            output <- new[, l]
        }
        while (condition > Tolerance & l < Maxiter) {
            l <- l + 1
            new[, l] <- newpar(new[1:p, (l - 1)], new[(p + 1), (l - 1)],
                lambd)
            llglg <- loglikglg(new[1:p, l], new[(p + 1), l], lambd)

            condition <- llglg - loglikglg(output[1:p], output[(p + 1)],
                lambd)

            if (condition > 0) {
                output <- new[, l]
            }
        }

        if (l <= Maxiter) {
            return(list(est = output, cond = condition, conv = TRUE, iter = l))
        }
        if (l >= Maxiter) {
            stop("The convergence was not successful.")
        }
    }

    output <- optimum(beta0, sigma0, lambda0)
    if (output$conv == TRUE) {
        conv <- output$conv
        iter <- output$iter
        condition <- output$cond
        output <- output$est

        llglg <- loglikglg(output[1:p], output[(p + 1)], lambda0)
        aic <- -2 * llglg + 2 * (p + 1)
        bic <- -2 * llglg + log(n) * (p + 1)
        aic2 <- aic + 2 * sum(y)
        scores <- U_theta(output[1:p], output[p + 1], lambda0)
        covar <- matrix(0, p + 1, p + 1)
        covar <- I_tetha(output[1:p], output[p + 1], lambda0)
        inter <- matrix(0, p + 1, 2)
        pval <- 0 * output
        ste <- 0 * output
        zs <- 0 * output
        scovar <- solve(-covar)
        val <- diag(scovar)
        if (min(val) > 0) {
            ste <- sqrt(val)
            inter[, 1] <- as.matrix(output - 1.96 * ste)
            inter[, 2] <- as.matrix(output + 1.96 * ste)
            zs <- abs(output/ste)
            pval <- 1 - (pnorm(zs) - pnorm(-zs))
        }

        y_est <- X %*% output[1:p]
        ordresidual <- eps(output[1:p], output[p + 1])
        sgn <- sign(Y[, 1] - y_est)
        outputp <- lambda0
        dev <- sgn * sqrt(2) * ((1 - Delta) * ((1/outputp^2) * exp(outputp *
            ordresidual) - (1/outputp) * ordresidual - (1/outputp)^2)^(0.5) +
            Delta * (-log(S(ordresidual, outputp))))
        devian <- sum(dev^2)
        part2 <- ((output[p + 1])/outputp) * (digamma((1/outputp)^2) - log((1/outputp)^2))
        y_est <- y_est + part2
        good_fit <- gfit(ordresidual, outputp)
        msurv <- good_fit$msurv
        good_fit <- good_fit$stat
        output <- list(formula = formula, size = n, per.censo = per.censo,
            p = p, mu = output[1:p], sigma = output[p + 1], lambda = lambda0,
            y = Y[, 1], delta = Delta, X_bar = X, y_est = y_est, rord = ordresidual,
            rdev = dev, deviance = devian, modelsurv=msurv, survItheta = scovar, scores = scores,
            goodnessoffit = good_fit, llglg = llglg, AIC = aic, BIC = bic,
            AIC2 = aic2, st_error = ste, z_values = zs, p.values = pval,
            interval = inter, convergence = conv, condition = condition,
            Iterations = iter, semi = FALSE, censored = TRUE)
        class(output) = "sglg"
        return(output)
    }

    if (output$conv == FALSE) {
        # print('The optimization was not successful.')
        return(0)
    }
}
