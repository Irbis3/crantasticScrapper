#'Fitting semi-parametric generalized log-gamma regression models
#'
#'\code{sglg} is used to fit a semi-parametric regression model suitable for analysis of data sets in which the response variable is continuous, strictly positive, and asymmetric.
#'In this setup, the location parameter of the response variable is explicitly modeled by semi-parametric functions, whose nonparametric components may be approximated by
#'natural cubic splines or cubic P-splines.
#'
#' @param formula a symbolic description of the systematic component of the model to be fitted. See details for further information.
#' @param npc a data frame with potential nonparametric variables of the systematic part of the model to be fitted.
#' @param basis a name of the cubic spline basis to be used in the model. Supported basis include deBoor and Gu basis
#'  which are a B-spline basis and a natural cubic spline basis, respectively.
#' @param data an optional data frame, list containing the variables in the model.
#' @param shape an optional value for the shape parameter of the error distribution of a generalized log-gamma distribution. Default value is 1.
#' @param method There are two possibles algorithms to estimate the parameters. The default algorithm is 'FS' Fisher-Scoring,
#' the other option is 'GSFS' an adequate combination between the block matrix version of non-linear Gauss-Seidel algorithm and Fisher-Scoring algorithm.
#' @param Tolerance an optional positive value, which represents the convergence criterion. Default value is 1e-04.
#' @param Maxiter an optional positive integer giving the maximal number of iterations for the estimating process. Default value is 1e03.

#' @return mu a vector of parameter estimates asociated with the location parameter.
#' @return sigma estimate of the scale parameter associated with the model.
#' @return lambda estimate of the shape parameter associated with the model.
#' @return interval estimate of a 95\% confidence interval for each estimate parameters associated with the model.
#' @return Deviance the deviance associated with the model.

#' @references Carlos A. Cardozo, G. Paula and L. Vanegas. Semi-parametric generalized log-gamma regression models. In preparation.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>, G. Paula and L. Vanegas.
#' @examples
#' rows <- 175 # Number of observations
#' columns <- 2 # Number of parametric components
#' library(ssym)
#' t_beta  <- c(0.5, 2)
#' t_sigma <- 1
#' t_lambda <- 1
#' t_knot1 <- 7
#' ts1 <- seq(0, 1, length = t_knot1)
#' t_g1 <- 0.4 * sin(pi * ts1)
#'
#' BasisN <- function(n, knot) {
#'           N <- matrix(0, n, knot)
#'           m <- n/knot
#'           block <- matrix(1, m, 1)
#'           for (i in 1:knot) {
#'           l <- (i - 1) * m + 1
#'           r <- i * m
#'           N[l:r, i] <- block }
#'           return(N)
#'           }
#' s_N1 <- BasisN(rows, length(ts1))
#' x3 <- s_N1 %*% ts1
#' colnames(x3) <- 'x3'
#' set.seed(8142031)
#' x1 <- rbinom(rows, 1, 0.5)
#' x2 <- runif(rows, 0, 1)
#' X <- cbind(x1,x2)
#' error <- rglg(rows, 0, 1, t_lambda)
#' y1 <- X %*%t_beta + + s_N1 %*% t_g1 + t_sigma * error
#' data.example <- data.frame(y1,X,x3)
#' fit1 <- sglg(y1 ~ x1 + x2 - 1, npc=x3, method='FS',data=data.example)
#' fit2 <- sglg(y1 ~ x1 + x2 - 1, npc=x3, method='GSFS',data=data.example)
#' @import ssym
#' @import robustloggamma
#' @import methods
#' @export sglg

sglg = function(formula, npc, basis, data, shape, method, Tolerance, Maxiter) {
    if (missingArg(formula)) {
        stop("The formula argument is missing.")
    }
    if (missingArg(npc)) {
        stop("This kind of model need at least one non-parametric component.")
    }
    if (missingArg(data)) {
        stop("The data argument is missing.")
    }
    
    if (missingArg(method)) 
        method <- "FS"
    if (missingArg(shape)) 
        shape <- 1
    if (missingArg(Tolerance)) 
        Tolerance <- 1e-04
    if (missingArg(Maxiter)) 
        Maxiter <- 1000
    
    if (missingArg(basis)) 
        basis <- rep("deBoor", dim(npc)[2])
    
    if (class(data) == "list") 
        data <- as.data.frame(data)
    
    data1 <- model.frame(formula, data = data)
    X <- model.matrix(formula, data = data1)
    y <- model.response(data1)
    
    p <- ncol(X)
    n <- nrow(X)
    op1 <- floor(n^(1/3)) + 3
    
    intknt <- function(x) {
        op2 <- length(as.numeric(levels(factor(x))))
        knt <- min(op1, op2)
        if (knt < 3) {
            stop("This covariate has not at least three different values.")
        }
        return(knt)
    }
    
    k <- dim(npc)[2]
    XX <- cbind(X, npc)
    
    Knot <- matrix(0, k, 1)
    for (i in 1:k) {
        Knot[i] <- intknt(XX[, (p + i)])
    }
    
    Knot <- as.numeric(Knot)
    Tknot <- sum(Knot)
    N <- X
    K <- matrix(0, p + Tknot, p + Tknot)
    
    for (j in 1:k) {
        output <- deBoor2(npc[, j], Knot[j])
        N <- cbind(N, output$N)
        Knot1 <- c(0, Knot)
        l <- p + 1 + sum(Knot1[1:j])
        r <- p + sum(Knot[1:j])
        K[l:r, l:r] <- output$K
    }
    
    g0 <- function(knot) {
        g <- rep(0, knot)
        return(g)
    }
    
    ############################################################################################################ 
    formula2 <- formula
    for (j in 1:k) {
        formul <- paste(".~. + ", colnames(npc)[j])
        formul <- as.formula(formul)
        formula2 <- update(formula2, formul)
    }
    ############################################################################################################################################################ 
    
    # Initial values
    
    fit0 <- glg(formula2, shape = shape, data = data)
    beta0 <- fit0$mu[1:p]
    g0s <- g0(Tknot)
    sigma0 <- fit0$sigma
    lambda0 <- fit0$lambda
    
    formula3 <- formula
    formul <- paste("ncs(", colnames(npc), sep = "")
    formul <- paste(formul, ")", sep = "")
    formul <- paste(".~. + ", formul, sep = "")
    for (j in 1:k) {
        formula3 <- update(formula3, formul[j])
    }
    
    fit00 <- ssym::ssym.l(formula3, data = data, family = "Normal")
    alpha0 <- fit00$lambdas.mu
    
    Ident <- diag(1, n)
    Ones <- matrix(1, n, 1)
    
    ## THE FISHER INFORMATION MATRIX
    
    M_bar <- function(alph) {
        output <- rep(0, p)
        for (j in 1:k) {
            output <- c(output, rep(alph[j], Knot[j]))
        }
        output <- diag(output)
        output <- K %*% output
        return(output)
    }
    
    I_gammas <- function(sigm, alph) {
        output <- (1/(sigm^2)) * t(N) %*% N
        output <- output + M_bar(alph)
        return(output)
    }
    
    u_lambda <- function(lambd) {
        invlamb <- 1/lambd^2
        output <- (1/lambd) * (digamma(1 + invlamb) - log(invlamb))
        return(output)
    }
    
    I_gammassigma <- function(sigm, lambd) {
        output <- (1/sigm^2) * u_lambda(lambd) * t(N) %*% Ones
        return(output)
    }
    
    I_gammaslambda <- function(sigm, lambd) {
        output <- -(sigm/lambd) * I_gammassigma(sigm, lambd)
        return(output)
    }
    
    ### Defining mu function
    
    mu <- function(bet, g) {
        bet <- as.matrix(bet)
        g <- as.matrix(g)
        g_bar <- rbind(bet, g)
        output <- N %*% g_bar
        return(output)
    }
    
    ### First step: The residuals
    
    eps <- function(bet, g, sigm) {
        epsilon <- (y - mu(bet, g))/sigm
        return(epsilon)
    }
    
    ### Second step: The matrix D
    
    D <- function(epsil, lambd) {
        w <- as.vector(exp(lambd * epsil))
        D_s <- diag(w, n, n)
        return(D_s)
    }
    
    ### Third step: The matrix W
    
    W <- function(bet, g, sigm, lambd) {
        output <- Ident - D(eps(bet, g, sigm), lambd)
        return(output)
    }
    
    ## The score functions
    
    U_sigma <- function(bet, g, sigm, lambd) {
        output <- -(1/sigm) * n - (1/(lambd * sigm)) * t(Ones) %*% W(bet, 
            g, sigm, lambd) %*% eps(bet, g, sigm)
        return(output)
    }
    
    U_lambda <- function(bet, g, sigm, lambd) {
        invlamb <- 1/lambd
        eta_lambd <- (invlamb) * (1 + 2 * (invlamb^2) * (digamma(invlamb^2) + 
            2 * log(abs(lambd)) - 1))
        Ds <- D(eps(bet, g, sigm), lambd)
        epsilons <- eps(bet, g, sigm)
        output <- n * eta_lambd - (invlamb^2) * t(Ones) %*% epsilons + (2 * 
            invlamb^3) * t(Ones) %*% Ds %*% Ones - (invlamb^2) * t(Ones) %*% 
            Ds %*% epsilons
        return(output)
    }
    
    U_sl <- function(bet, g, sigm, lambd) {
        output <- c(U_sigma(bet, g, sigm, lambd), U_lambda(bet, g, sigm, 
            lambd))
        return(output)
    }
    
    U_gammas <- function(bet, g, sigm, lambd, alph) {
        output <- (-1/(sigm * lambd)) * t(N) %*% W(bet, g, sigm, lambd) %*% 
            Ones - M_bar(alph) %*% c(bet, g)
        return(output)
    }
    
    U_theta <- function(bet, g, sigm, lambd, alph) {
        output <- U_gammas(bet, g, sigm, lambd, alph)
        output <- c(output, U_sl(bet, g, sigm, lambd))
        return(output)
    }
    
    ## Estimating sigma and lambda
    
    v_lambda <- function(lambd) {
        invlamb <- 1/lambd^2
        output <- invlamb * trigamma(1 + invlamb) + u_lambda(lambd)^2
        return(output)
    }
    
    # K_1_lambda and K_2_lambda functions
    
    K_1 <- function(lambda) {
        invlamb2 <- 1/lambda^2
        part1 <- 4 * (1 + digamma(1 + invlamb2) - digamma(invlamb2) - invlamb2 * 
            trigamma(invlamb2))
        part2 <- trigamma(1 + invlamb2) + (digamma(1 + invlamb2) - log(invlamb2))^2
        output <- 1 - invlamb2 * (part1 - part2)
        return(output)
    }
    
    K_2 <- function(lambda) {
        invlamb <- 1/lambda
        invlamb2 <- invlamb^2
        output <- invlamb * ((digamma(1 + invlamb2) - digamma(invlamb2)) - 
            trigamma(1 + invlamb2) - (digamma(1 + invlamb2) - log(invlamb2))^2)
        return(output)
    }
    
    ## Defining the components of the FIM
    
    I_22 <- function(sigm, lambd) {
        output <- (n/(sigm^2)) * (1 + v_lambda(lambd))
        return(output)
    }
    
    I_23 <- function(sigm, lambd) {
        output <- (n/(sigm * lambd^2)) * K_2(lambd)
        return(output)
    }
    
    I_33 <- function(sigm, lambd) {
        output <- (n/(lambd^2)) * K_1(lambd)
        return(output)
    }
    
    I_sl <- function(sigm, lambd) {
        output <- matrix(c(I_22(sigm, lambd), I_23(sigm, lambd), I_23(sigm, 
            lambd), I_33(sigm, lambd)), 2, 2)
        return(output)
    }
    
    
    I_theta <- function(sigm, lambd, alph) {
        I_gs <- I_gammassigma(sigm, lambd)
        I_gl <- I_gammaslambda(sigm, lambd)
        output <- cbind(I_gammas(sigm, alph), I_gs, I_gl)
        output1 <- cbind(I_gs, I_gl)
        output1 <- cbind(t(output1), I_sl(sigm, lambd))
        output <- rbind(output, output1)
        return(output)
    }
    
    ## LOG-LIKELIHOOD
    
    c_l <- function(lambd) {
        invlambdos <- 1/lambd^2
        c <- abs(lambd)/gamma(invlambdos)
        output <- c * (invlambdos^invlambdos)
        return(output)
    }
    
    loglikglg <- function(bet, g, sigm, lambd, alph) {
        epsilon <- eps(bet, g, sigm)
        part1 <- n * log(c_l(lambd)/sigm) + (1/lambd) * t(Ones) %*% epsilon - 
            (1/lambd^2) * t(Ones) %*% D(epsilon, lambd) %*% Ones
        part2 <- -0.5 * t(c(bet, g)) %*% M_bar(alph) %*% c(bet, g)
        output <- part1 + part2
        return(output)
    }
    
    newpar <- function(bet, g, sigm, lambd, alph) {
        output <- matrix(0, p + Tknot + 2, Maxiter)
        output[, 1] <- c(bet, g, sigm, lambd)
        new <- output[, 1]
        llglg <- loglikglg(new[1:p], new[(p + 1):(p + Tknot)], new[p + Tknot + 
            1], new[p + Tknot + 2], alph)
        
        if (method == "FS") {
            output[, 2] <- output[, 1] + solve(I_theta(sigm, lambd, alph)) %*% 
                U_theta(bet, g, sigm, lambd, alph)
            condition <- loglikglg(output[1:p, 2], output[(p + 1):(p + Tknot), 
                2], output[p + Tknot + 1, 2], output[p + Tknot + 2, 2], alph) - 
                llglg
            if (condition > 0) {
                new <- output[, 2]
            }
            condition <- 1
            l <- 2
            while (condition > Tolerance & l < Maxiter) {
                l <- l + 1
                output[, l] <- output[, (l - 1)] + solve(I_theta(output[(p + 
                  Tknot + 1), (l - 1)], output[(p + Tknot + 2), (l - 1)], 
                  alph)) %*% U_theta(output[1:p, (l - 1)], output[(p + 1):(p + 
                  Tknot), (l - 1)], output[(p + Tknot + 1), (l - 1)], output[(p + 
                  Tknot + 2), (l - 1)], alph)
                llglg <- loglikglg(new[1:p], new[(p + 1):(p + Tknot)], new[p + 
                  Tknot + 1], new[p + Tknot + 2], alph)
                condition <- loglikglg(output[1:p, l], output[(p + 1):(p + 
                  Tknot), l], output[p + Tknot + 1, l], output[p + Tknot + 
                  2, l], alph) - llglg
                if (condition > 0) {
                  new <- output[, l]
                  llglg <- loglikglg(new[1:p], new[(p + 1):(p + Tknot)], 
                    new[p + Tknot + 1], new[p + Tknot + 2], alph)
                }
            }
        }
        
        if (method == "GSFS") {
            ps <- c(p, Knot, 2)
            b <- I_theta(sigm, lambd, alph) %*% output[, 1] + U_theta(bet, 
                g, sigm, lambd, alph)
            output[, 2] <- blockgs(I_theta(sigm, lambd, alph), b, output[, 
                1], ps)$x
            condition <- loglikglg(output[1:p, 2], output[(p + 1):(p + Tknot), 
                2], output[p + Tknot + 1, 2], output[p + Tknot + 2, 2], alph) - 
                llglg
            if (condition > 0) {
                new <- output[, 2]
            }
            condition <- 1
            l <- 2
            while (condition > Tolerance & l < Maxiter) {
                l <- l + 1
                b <- I_theta(output[(p + Tknot + 1), (l - 1)], output[(p + 
                  Tknot + 2), (l - 1)], alph) %*% output[, (l - 1)] + U_theta(output[1:p, 
                  (l - 1)], output[(p + 1):(p + Tknot), (l - 1)], output[(p + 
                  Tknot + 1), (l - 1)], output[(p + Tknot + 2), (l - 1)], 
                  alph)
                output[, l] <- blockgs(I_theta(output[(p + Tknot + 1), (l - 
                  1)], output[(p + Tknot + 2), (l - 1)], alph), b, output[, 
                  (l - 1)], ps)$x
                # output[, l] <- output[, (l - 1)] + solve(I_theta(output[(p + Tknot +
                # 1), (l - 1)], output[(p + Tknot + 2), (l - 1)], alph)) %*%
                # U_theta(output[1:p, (l - 1)], output[(p + 1):(p + Tknot), (l - 1)],
                # output[(p + Tknot + 1), (l - 1)], output[(p + Tknot + 2), (l - 1)],
                # alph)
                llglg <- loglikglg(new[1:p], new[(p + 1):(p + Tknot)], new[p + 
                  Tknot + 1], new[p + Tknot + 2], alph)
                condition <- loglikglg(output[1:p, l], output[(p + 1):(p + 
                  Tknot), l], output[p + Tknot + 1, l], output[p + Tknot + 
                  2, l], alph) - llglg
                if (condition > 0) {
                  new <- output[, l]
                  llglg <- loglikglg(new[1:p], new[(p + 1):(p + Tknot)], 
                    new[p + Tknot + 1], new[p + Tknot + 2], alph)
                }
            }
        }
        return(list(est = new, ll = llglg, cond = condition, iter = l))
    }
    
    ## Effective degree freedom - EDF
    
    inv_root_A <- function(A) {
        e <- eigen(A)
        V <- e$vectors
        output <- solve(V %*% sqrt(diag(e$values)) %*% t(V))
        return(output)
    }
    
    edf <- function(sigm, alph) {
        inv_root_tNN <- inv_root_A(t(N) %*% N)
        output <- diag(1, p + sum(Knot)) + (sigm^2) * inv_root_tNN %*% M_bar(alph) %*% 
            inv_root_tNN
        output <- sum(diag(solve(output)))
        return(output)
    }
    
    Conv <- FALSE
    num.iter <- 1
    masterf <- function(alph) {
        news <- newpar(beta0, g0s, sigma0, lambda0, alph)
        if (news$iter < Maxiter) {
            Conv <- TRUE
            num.iter <- news$iter
            cond <- news$cond
            llglg <- news$ll
        }
        df <- edf(news$est[(p + Tknot + 1)], alph)
        aic <- -2 * llglg + 2 * (df + 2)
        bic <- -2 * llglg + log(n) * (df + 2)
        
        return(list(est = news$est, df = df, llglg = llglg, AIC = aic, BIC = bic, 
            Conv = Conv, iter = num.iter, cond = cond))
    }
    
    AIC_p <- function(alph) {
        output <- masterf(alph)$AIC
        output <- round(output, digits = 4)
        return(output)
    }
    
    opt_alph <- function(alph) {
        out <- optimize(AIC_p, c(0, alph + 2), tol = 0.001)
        return(c(out$minimum, out$objective))
    }
    
    gfit <- function(resid, lambd) {
        Fs <- ploggamma(resid, lambda = lambd)
        equantil <- qnorm(Fs)
        diff <- qqnorm(equantil, plot.it = FALSE)
        output <- mean(abs(diff$x - diff$y))
        return(output)
    }
    
    total_optimum <- function(start) {
        output0 <- opt_alph(start)
        output1 <- output0[1:k]
        output2 <- output0[k + 1]
        output3 <- masterf(output1)
        df <- output3$df
        dfnpc <- df - p
        output <- output3$est
        llglg <- output3$llglg
        aic <- output3$AIC
        bic <- output3$BIC
        scores <- U_theta(output[1:p], output[(p + 1):(p + Tknot)], output[p + 
            sum(Knot) + 1], output[p + Tknot + 2], output1)
        covar <- I_theta(output[p + Tknot + 1], output[p + Tknot + 2], output1)
        inter <- matrix(0, p + Tknot + 2, 2)
        scovar <- solve(covar)
        val <- diag(scovar)
        if (min(val) > 0) {
            ste <- sqrt(val)
            inter[, 1] <- as.matrix(output - 1.96 * ste)
            inter[, 2] <- as.matrix(output + 1.96 * ste)
            zs <- abs(output/ste)
            pval <- 1 - (pnorm(zs) - pnorm(-zs))
            pval2 <- pval[-((p + 1):(p + Tknot))]
            as <- output[(p + 1):(p + Tknot)]
        }
        
        y_est <- N %*% output[1:(p + Tknot)]
        ordresidual <- eps(output[1:p], output[(p + 1):(p + Tknot)], output[p + 
            Tknot + 1])
        sgn <- sign(y - y_est)
        dev <- sgn * sqrt(2) * ((1/output[p + Tknot + 2]^2) * exp(output[p + 
            Tknot + 2] * ordresidual) - (1/output[p + Tknot + 2]) * ordresidual - 
            (1/output[p + Tknot + 2])^2)^(0.5)
        Devian <- sum(dev^2)
        
        good_fit <- gfit(ordresidual, output[p + Tknot + 2])
        part2 <- ((output[p + Tknot + 1])/(output[p + Tknot + 2])) * (digamma((1/output[p + 
            Tknot + 2])^2) - log((1/output[p + Tknot + 2])^2))
        y_est2 <- y_est + part2
        
        return(list(formula = formula, npc = npc, size = n, mu = output[1:(p + 
            Tknot)], sigma = output[p + Tknot + 1], lambda = output[p + Tknot + 
            2], y = y, X = X, p = p, N = N, Knot = Knot, rord = ordresidual, 
            rdev = dev, interval = inter, llglg = llglg, AIC = output2, BIC = output3$BIC, 
            scores = scores, Itheta = covar, scovar = scovar, st_error = ste, 
            Z_values = zs, p.values = pval, alpha = output1, d.f.model = df, 
            d.f.npc = dfnpc, deviance = Devian, goodnessoffit = good_fit, 
            convergence = output3$Conv, condition = output3$cond, iterations = output3$iter, 
            semi = TRUE, censored = FALSE, y_est2 = y_est))
    }
    output <- total_optimum(alpha0)
    class(output) <- "sglg"
    return(output)
}
