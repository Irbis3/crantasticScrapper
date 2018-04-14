#'Fitting semi-parametric generalized log-gamma regression models under the presence of right censored data.
#'
#'\code{ssurvglg} is used to fit a semi-parametric regression model in which the response variable is continuous, strictly positive, asymmetric and there are right censored observations.
#'In this setup, the location parameter of the logarithm of the variable is explicitly modeled by semi-parametric functions, whose nonparametric components may be approximated by
#'natural cubic splines or cubic P-splines.
#'
#' @param formula a symbolic description of the systematic component of the model to be fitted. See details for further information.
#' @param npc a data frame with potential nonparametric variables of the systematic part of the model to be fitted.
#' @param basis a name of the cubic spline basis to be used in the model. Supported basis include deBoor and Gu basis
#'  which are a B-spline basis and a natural cubic spline basis, respectively.
#' @param data an optional data frame, list containing the variables in the model.
#' @param shape an optional value for the shape parameter of the model.
#' @param Tolerance an optional positive value, which represents the convergence criterion. Default value is 1e-04.
#' @param Maxiter an optional positive integer giving the maximal number of iterations for the estimating process. Default value is 1e03.

#' @return mu a vector of parameter estimates asociated with the location parameter.
#' @return sigma estimate of the scale parameter associated with the model.
#' @return lambda estimate of the shape parameter associated with the model.
#' @return interval estimate of a 95\% confidence interval for each estimate parameters associated with the model.
#' @return Deviance the deviance associated with the model.

#' @references Carlos A. Cardozo, G. Paula and L. Vanegas. Semi-parametric accelerated failure time models with generalized log-gamma erros: Censored case. In preparation.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>, G. Paula and L. Vanegas.
#' @examples
#' rows    <- 150
#' columns <- 2

#' t_beta  <- c(0.5, 2)
#' t_sigma <- 0.75
#' t_lambda <- 1
#' set.seed(8142030)
#' library(ssym)
#' x1 <- rbinom(rows, 1, 0.5)
#' x2 <- runif(rows, 0, 1)
#' X <- cbind(x1,x2)

#' t_knot1 <- 6
#' ts1 <- seq(0, 1, length = t_knot1)
#' t_g1 <- 0.4 * sin(pi * ts1)

#' BasisN <- function(n, knot) {
#' N <- matrix(0, n, knot)
#' m <- n/knot
#' block <- rep(1,m)
#' for (i in 1:knot) {
#'    l <- (i - 1) * m + 1
#'  r <- i * m
#'   N[l:r, i] <- block }
#' return(N)
#'  }

#' s_N1 <- BasisN(rows, length(ts1))
#' x3 <- s_N1 %*% ts1
#' colnames(x3) <- 'x3'
#' sys       <- X %*% t_beta + s_N1%*%t_g1
#' t_ini1    <- exp(sys) * rweibull(rows,1/t_sigma,1)
#' cens.time <- rweibull(rows, 1.5, 14)

#' delta     <- ifelse(t_ini1 > cens.time, 1, 0)
#' obst1 = t_ini1
#' for(i in 1:rows) {
#'    if (delta[i] == 1) {
#'        obst1[i] = cens.time[i]
#'       }
#' }
#' data.example <- data.frame(obst1, delta, X, x3)
#' fit4  <- ssurvglg(Surv(log(obst1),delta)~ x1 + x2 - 1, npc=x3, data=data.example, shape=0.8)
#' @import ssym
#' @import robustloggamma
#' @import methods
#' @export ssurvglg
#'
ssurvglg = function(formula, npc, basis, data, shape, Maxiter, Tolerance) {
    if (missingArg(formula)) {
        stop("The formula argument is missing.")
    }
    if (missingArg(data)) {
        stop("The data argument is missing.")
    }

    if (missingArg(Tolerance))
        Tolerance = 1e-04
    if (missingArg(Maxiter))
        Maxiter = 1000
    if (missingArg(shape))
        shape <- 1

    if (missingArg(basis))
        basis = rep("deBoor", dim(npc)[2])

    if (class(data) == "list")
        data <- as.data.frame(data)

    data1 <- model.frame(formula, data = data)
    X <- model.matrix(formula, data1)
    Y <- model.response(data1)
    per.censo <- 100 * mean(Y[, 2])
    delta <- Y[, 2]
    p = ncol(X)
    n = nrow(X)

    intknt = function(x) {
        op1 = floor(n^(1/3)) + 3
        op2 = length(as.numeric(levels(factor(x))))
        knt = min(op1, op2)
        if (knt < 3) {
            stop("This covariate has not at least three different values.")
        }
        return(knt)
    }

    k <- dim(npc)[2]
    XX = cbind(X, npc)
    Knot = matrix(0, k, 1)
    for (i in 1:k) {
        Knot[i] = intknt(XX[, (p + i)])
    }
    Knot = as.numeric(Knot)

    npoutput <- deBoor2(npc, Knot)
    N <- npoutput$N
    K <- npoutput$K

    g0 = function(knot) {
        g = 0 * 1:knot
        return(g)
    }

    ##################################################################################################################################################

    # Initial values
    formula2 <- formula

    formula21 <- as.character(formula(Formula(formula2), rhs = 0))[2]
    formula21 <- sub("delta", "1 - delta", formula21)
    formula21 <- paste(formula21, "~ ")

    formula22 <- paste(" ncs(", colnames(npc), sep = "")
    formula22 <- paste(formula22, ")", sep = "")

    formula2 <- paste(formula21, formula22)

    formula2 <- formula(formula2)
    formula23 <- as.character(formula(Formula(formula), lhs = 0))[2]
    formula23 <- paste(".~. + ", formula23)
    formula23 <- formula(formula23)

    formula2 <- update(formula2, formula23)

    fit0 = ssym.l2(formula2, data = data, family = "Normal")
    beta0 = fit0$theta.mu[1:p]
    g0s = g0(Knot)
    sigma0 = exp(fit0$theta.phi)
    lambda0 = shape
    alpha0 = fit0$lambdas.mu

    ###############################################################################################################################################################

    # Some fixed matrizes

    I_n = diag(1, n)

    # Defining mu function

    mu = function(bet, g) {
        output = X %*% bet + N %*% g
        return(output)
    }

    ### First step: The residuals

    eps = function(bet, g, sigm) {
        epsilon = (Y[, 1] - mu(bet, g))/sigm
        return(epsilon)
    }

    ### Second step: The matrix D

    S = function(t, lambd) {
        s = pgamma((1/lambd^2) * exp(lambd * t), 1/lambd^2, lower.tail = FALSE)
        return(s)
    }

    ### Third step: The matrix Wd

    aa = function(bet, g, sigm, lambd) {
        epsilon = eps(bet, g, sigm)
        output = (1/lambd^2) * exp(lambd * epsilon)
        return(output)
    }

    ## Score function

    U_beta = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        output = matrix(0, p, 1)
        for (j in 1:p) {
            output[j] = sum(X[, j] * ((1 - delta) * (1/(lambd * sigm)) *
                (exp(lambd * epsil) - 1) + delta * (lambd/sigm) * bb))
        }
        return(output)
    }

    U_g = function(bet, g, sigm, lambd, alph) {
        epsil = eps(bet, g, sigm)
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        output = matrix(0, p, 1)
        for (j in 1:Knot) {
            output[j] = sum(N[, j] * ((1 - delta) * (1/(lambd * sigm)) *
                (exp(lambd * epsil) - 1) + delta * (lambd/sigm) * bb))
        }
        output = output - alph * K %*% g
        return(output)
    }

    U_sigma = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        part1 = (1/sigm) * (-1 - (1/lambd) * (epsil - epsil * exp(lambd *
            epsil)))
        part2 = (lambd/sigm) * epsil * bb
        output = sum((1 - delta) * part1 + delta * part2)
        return(output)
    }

    U_theta = function(bet, g, sigm, lambd, alph) {
        output = matrix(1, p + Knot + 1, 1)
        output[1:p] = U_beta(bet, g, sigm, lambd)
        output[(p + 1):(p + Knot)] = U_g(bet, g, sigm, lambd, alph)
        output[p + Knot + 1] = U_sigma(bet, g, sigm, lambd)
        return(output)
    }

    # Observational Fisher Matrix

    I_beta = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        output = matrix(0, p, p)
        for (l in 1:p) {
            for (j in 1:p) {
                output[l, j] = sum(X[, l] * X[, j] * ((1 - delta) * (-1/sigm^2) *
                  exp(lambd * epsil) + delta * ((lambd/sigm)^2) * bb * (aaa -
                  1/lambd^2 - bb)))
            }
        }
        return(output)
    }

    I_g = function(bet, g, sigm, lambd, alph) {
        epsil = eps(bet, g, sigm)
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        output = matrix(0, Knot, Knot)
        for (l in 1:Knot) {
            for (j in 1:Knot) {
                output[l, j] = sum(N[, l] * N[, j] * ((1 - delta) * (-1/sigm^2) *
                  exp(lambd * epsil) + delta * ((lambd/sigm)^2) * bb * (aaa -
                  1/lambd^2 - bb)))
            }
        }
        output = output - alph * K
        return(output)
    }

    I_sigma = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        part1 = (1/sigm^2) * (1 + (2/lambd) * epsil * (1 - exp(lambd * epsil)) -
            (epsil^2) * exp(lambd * epsil))
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        b = aaa - (1/lambd^2) - bb/gamma(1/lambd^2)
        part2 = ((lambd * epsil * bb)/(sigm^2)) * (epsil * b - 2/lambd)
        output = sum((1 - delta) * part1 + delta * part2)
        return(output)
    }

    I_betg = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        aaa = aa(bet, g, sigm, lambd)
        S = S(epsil, lambd)
        bb = (aaa^(1/lambd^2) * exp(-aaa))/(gamma(1/lambd^2) * S)
        output = matrix(0, p, Knot)
        for (l in 1:p) {
            for (j in 1:Knot) {
                output[l, j] = sum(X[, l] * N[, j] * ((1 - delta) * (-1/sigm^2) *
                  exp(lambd * epsil) + delta * ((lambd/sigm)^2) * bb * (aaa -
                  1/lambd^2 - bb)))
            }
        }
        return(output)
    }

    I_betsig = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        expep = exp(lambd * epsil)
        H = matrix(0, p, 1)
        for (k in 1:p) {
            H[k] = sum((1 - delta) * X[, k] * (1 - expep - lambd * epsil *
                expep))
        }
        part1 = (1/(lambd * (sigm^2))) * H
        part2 = 0 * H
        aaa = aa(bet, g, sigm, lambd)
        bb = (aaa^(1/lambd^2)) * exp(-aaa)
        S = S(epsil, lambd)
        deno = gamma(1/lambd^2) * S
        part21 = -1/lambd + epsil * (aaa - (1/lambd^2) - bb/deno)
        for (k in 1:p) {
            H[k] = sum(delta * X[, k] * ((((lambd/sigm)^2) * bb/deno) * part21))
        }
        part2 = H
        output = part1 + part2
        return(output)
    }

    I_gsig = function(bet, g, sigm, lambd) {
        epsil = eps(bet, g, sigm)
        expep = exp(lambd * epsil)
        H = matrix(0, Knot, 1)
        for (k in 1:Knot) {
            H[k] = sum((1 - delta) * N[, k] * (1 - expep - lambd * epsil *
                expep))
        }
        part1 = (1/(lambd * (sigm^2))) * H
        part2 = 0 * H
        aaa = aa(bet, g, sigm, lambd)
        bb = (aaa^(1/lambd^2)) * exp(-aaa)
        S = S(epsil, lambd)
        deno = gamma(1/lambd^2) * S
        part21 = -1/lambd + epsil * (aaa - (1/lambd^2) - bb/deno)
        for (k in 1:Knot) {
            H[k] = sum(delta * N[, k] * ((((lambd/sigm)^2) * bb/deno) * part21))
        }
        part2 = H
        output = part1 + part2
        return(output)
    }

    I_tetha = function(bet, g, sigm, lambd, alph) {
        output = matrix(0, p + Knot + 1, p + Knot + 1)

        output[1:p, 1:p] = I_beta(bet, g, sigm, lambd)
        output[1:p, (p + 1):(p + Knot)] = I_betg(bet, g, sigm, lambd)
        output[1:p, (p + Knot + 1)] = I_betsig(bet, g, sigm, lambd)

        output[(p + 1):(p + Knot), 1:p] = t(output[1:p, (p + 1):(p + Knot)])
        output[(p + 1):(p + Knot), (p + 1):(p + Knot)] = I_g(bet, g, sigm,
            lambd, alph)
        output[(p + 1):(p + Knot), p + Knot + 1] = I_gsig(bet, g, sigm, lambd)

        output[(p + Knot + 1), 1:p] = t(output[1:p, (p + Knot + 1)])
        output[(p + Knot + 1), (p + 1):(p + Knot)] = t(output[(p + 1):(p +
            Knot), p + Knot + 1])
        output[(p + Knot + 1), (p + Knot + 1)] = I_sigma(bet, g, sigm, lambd)

        return(output)
    }

    ## LOG-LIKELIHOOD

    c_l = function(lambd) {
        invlambdos = 1/lambd^2
        c = abs(lambd)/gamma(invlambdos)
        output = c * (invlambdos^invlambdos)
        return(output)
    }

    loglikglg = function(bet, g, sigm, lambd, alph) {
        epsil = eps(bet, g, sigm)
        S = S(epsil, lambd)
        output1 = sum(delta * log(S) + (1 - delta) * (log(c_l(lambd)/sigm) +
            (1/lambd) * epsil - (1/lambd^2) * exp(lambd * epsil)))
        output2 = -(alph/2) * g %*% K %*% g
        output = output1 + output2
        return(output)
    }

    newpar = function(bet, g, sigm, lambd, alph) {
        output = matrix(0, p + Knot + 1, 2)
        output[, 1] = c(bet, g, sigm)
        new = output[, 1]
        l = 2
        output[, l] = output[, (l - 1)] - solve(I_tetha(output[1:p, (l -
            1)], output[(p + 1):(p + Knot), (l - 1)], output[(p + Knot +
            1), (l - 1)], lambd, alph)) %*% U_theta(output[1:p, (l - 1)],
            output[(p + 1):(p + Knot), (l - 1)], output[(p + Knot + 1), (l -
                1)], lambd, alph)
        llglg = loglikglg(output[1:p, 2], output[(p + 1):(p + Knot), 2],
            output[p + Knot + 1, 2], lambd, alph)
        condition = llglg - loglikglg(new[1:p], new[(p + 1):(p + Knot)],
            new[p + Knot + 1], lambd, alph)

        if (condition == "NaN") {
            norm = as.numeric(sqrt(t(U_theta(output[1:p, (l - 1)], output[(p +
                1):(p + Knot), (l - 1)], output[(p + Knot + 1), (l - 1)],
                lambd, alph)) %*% U_theta(output[1:p, (l - 1)], output[(p +
                1):(p + Knot), (l - 1)], output[(p + Knot + 1), (l - 1)],
                lambd, alph)))
            output[, l] = output[, (l - 1)] - (1/norm) * solve(I_tetha(output[1:p,
                (l - 1)], output[(p + 1):(p + Knot), (l - 1)], output[(p +
                Knot + 1), (l - 1)], lambd, alph)) %*% U_theta(output[1:p,
                (l - 1)], output[(p + 1):(p + Knot), (l - 1)], output[(p +
                Knot + 1), (l - 1)], lambd, alph)
            llglg = loglikglg(output[1:p, 2], output[(p + 1):(p + Knot),
                2], output[p + Knot + 1, 2], lambd, alph)
            condition = llglg - loglikglg(new[1:p], new[(p + 1):(p + Knot)],
                new[p + Knot + 1], lambd, alph)
        }

        if (condition < 0) {
            norm = as.numeric(sqrt(t(U_theta(output[1:p, (l - 1)], output[(p +
                1):(p + Knot), (l - 1)], output[(p + Knot + 1), (l - 1)],
                lambd, alph)) %*% U_theta(output[1:p, (l - 1)], output[(p +
                1):(p + Knot), (l - 1)], output[(p + Knot + 1), (l - 1)],
                lambd, alph)))
            output[, l] = output[, (l - 1)] - (1/norm) * solve(I_tetha(output[1:p,
                (l - 1)], output[(p + 1):(p + Knot), (l - 1)], output[(p +
                Knot + 1), (l - 1)], lambd, alph)) %*% U_theta(output[1:p,
                (l - 1)], output[(p + 1):(p + Knot), (l - 1)], output[(p +
                Knot + 1), (l - 1)], lambd, alph)
            llglg = loglikglg(output[1:p, 2], output[(p + 1):(p + Knot),
                2], output[p + Knot + 1, 2], lambd, alph)
            condition = llglg - loglikglg(new[1:p], new[(p + 1):(p + Knot)],
                new[p + Knot + 1], lambd, alph)
        }

        if (condition > 0) {
            new = output[, 2]
        }
        return(new)
    }

    gfit = function(resid, lambd) {
        ekm = survfit(Surv(exp(resid), 1 - delta) ~ 1)
        surv = as.numeric(unlist(as.vector(summary(ekm)[6])))
        Fkm = 1 - surv

        res = sort((resid * (1 - delta))[delta == 0])
        Fs = robustloggamma::ploggamma(res, lambda = lambd)
        r_q = qnorm(Fs)

        diff = abs(r_q - qnorm(Fkm))
        output = mean(diff[-length(diff)])
        msurv <- 1 - Fs
        return(list(stat=output,msurv = msurv))
    }

    ## THE MAIN FUNCTION

    conv = FALSE
    condition = 1
    iter = 1
    l = 1

    optimum = function(bet, g, sigm, lambd, alph) {
        new = matrix(0, p + Knot + 1, Maxiter)
        new[, 1] = c(bet, g, sigm)
        output = new[, 1]

        l = 2
        new[, l] = newpar(new[1:p, (l - 1)], new[(p + 1):(p + Knot), (l -
            1)], new[(p + Knot + 1), (l - 1)], lambd, alph)

        llglg = loglikglg(new[1:p, l], new[(p + 1):(p + Knot), l], new[(p +
            Knot + 1), l], lambd, alph)
        condition = llglg - loglikglg(output[1:p], output[(p + 1):(p + Knot)],
            output[p + Knot + 1], lambd, alph)

        if (condition > 0) {
            output = new[, l]
        }

        while (condition > Tolerance & l < Maxiter) {
            l = l + 1
            new[, l] = newpar(new[1:p, (l - 1)], new[(p + 1):(p + Knot),
                (l - 1)], new[(p + Knot + 1), (l - 1)], lambd, alph)
            llglg = loglikglg(new[1:p, l], new[(p + 1):(p + Knot), l], new[(p +
                Knot + 1), l], lambd, alph)

            condition = llglg - loglikglg(output[1:p], output[(p + 1):(p +
                Knot)], output[(p + Knot + 1)], lambd, alph)

            if (condition > 0) {
                output = new[, l]
                llglg = loglikglg(output[1:p], output[(p + 1):(p + Knot)],
                  output[(p + Knot + 1)], lambd, alph)
            }
        }
        if (l < Maxiter) {
            return(list(est = output, llglg = llglg, cond = condition, conv = TRUE,
                iter = l))
        }
        if (l >= Maxiter) {
            stop("The convergence was not successful.")
        }
    }

    edf = function(bet, g, sigm, lambd, alph) {
        output = sum(diag((1/sigm^2) * t(N) %*% N %*% solve(-I_g(bet, g,
            sigm, lambd, alph))))
        return(output)
    }

    Conv = FALSE
    num.iter = 1

    masterf = function(alph) {
        news = optimum(beta0, g0s, sigma0, lambda0, alph)
        if (news$iter < Maxiter) {
            Conv = news$conv
            num.iter = news$iter
            cond = news$cond
            llglg = news$llglg
            df = edf(news$est[1:p], news$est[(p + 1):(p + Knot)], news$est[(p +
                Knot + 1)], lambda0, alph)
            aic = -2 * llglg + 2 * (p + df + 1)
            bic = -2 * llglg + log(n) * (p + df + 1)
            return(list(est = news$est, df = df, llglg = llglg, AIC = aic,
                BIC = bic, Conv = Conv, iter = num.iter, cond = cond))
        }
        if (news$iter >= Maxiter) {
            Conv = FALSE
            return(list(Conv = Conv))
            stop("")
        }
    }

    AIC_p = function(alph) {
        tetha = masterf(alph)
        output = tetha$AIC
        output = round(output, digits = 3)
        return(output)
    }

    opt_alph = function(alph) {
        out = optimize(AIC_p, c(0, alph + 2), tol = 0.001)
        return(c(out$minimum, out$objective))
    }

    total_optimum = function(start) {
        output0 <- opt_alph(start)
        output1 <- output0[1:k]
        output2 <- output0[k + 1]
        output3 <- masterf(output1)
        if (output3$Conv == FALSE) {
            print("The optimization was not successful.")
            return(0)
        }
        df <- output3$df
        d.f.npc <- df - p
        output <- output3$est
        llglg <- output3$llglg
        aic <- output3$AIC
        bic <- output3$BIC
        scores <- U_theta(output[1:p], output[(p + 1):(p + Knot)], output[p +
            Knot + 1], lambda0, output1)
        covar <- I_tetha(output[1:p], output[(p + 1):(p + Knot)], output[p +
            Knot + 1], lambda0, output1)
        inter <- matrix(0, p + Knot + 1, 2)
        scovar <- solve(-covar)
        val <- diag(scovar)
        # if (min(val) > 0) {
        ste <- sqrt(val)
        inter[, 1] <- as.matrix(output - 1.96 * ste)
        inter[, 2] <- as.matrix(output + 1.96 * ste)
        zs <- abs(output/ste)
        pval <- 1 - (pnorm(zs) - pnorm(-zs))
        pval2 <- pval[-((p + 1):(p + Knot))]
        as <- output[(p + 1):(p + Knot)]
        # }
        y_est <- X %*% output[1:p] + N %*% output[(p + 1):(p + Knot)]
        ordresidual <- eps(output[1:p], output[(p + 1):(p + Knot)], output[p +
            Knot + 1])
        sgn <- sign(Y[, 1] - y_est)
        outputp <- lambda0
        dev <- sgn * sqrt(2) * ((1 - delta) * ((1/outputp^2) * exp(outputp *
            ordresidual) - (1/outputp) * ordresidual - (1/outputp)^2)^(0.5) +
            delta * (-log(S(ordresidual, outputp))))
        devian <- sum(dev^2)
        part2 <- ((output[p + 1])/outputp) * (digamma((1/outputp)^2) - log((1/outputp)^2))
        y_est <- y_est + part2
        good_fit <- gfit(ordresidual, outputp)
        msurv <- good_fit$msurv
        output <- list(formula = formula, npc = npc, size = n, per.cens = per.censo,
            mu = output[1:(p + Knot)], sigma = output[p + Knot + 1], lambda = lambda0,
            alpha = output1, edf = df, d.f.npc = d.f.npc, y = Y[, 1], delta = delta,
            X = X, N = N, p = p, Knot = Knot, y_est = y_est, rord = ordresidual,
            rdev = dev, scores = scores, deviance = devian,  modelsurv=msurv, goodnessoffit = good_fit$stat,
            llglg = llglg, AIC = aic, BIC = bic, scovar = scovar, st_error = ste,
            p.values = pval2, convergence = output3$Conv, condition = output3$cond,
            Iterations = output3$iter, semi = TRUE, censored = TRUE)
        return(output)
    }
    output = total_optimum(alpha0)
    class(output) = "sglg"
    return(output)
}

