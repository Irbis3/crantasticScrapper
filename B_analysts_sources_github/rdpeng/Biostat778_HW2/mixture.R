mixture <- function(y, method = c("newton", "EM"), maxit = NULL, tol = 1e-8,
                    param0 = NULL) {
        method <- match.arg(method)
        switch(method,
               newton = {
                       if(is.null(maxit))
                               maxit <- 100
                       newton.mix(y, param0, maxit, tol)
               },
               EM = {
                       if(is.null(maxit))
                               maxit <- 500
                       em.mix(y, param0, maxit, tol)
               })
}

newton.mix <- function(y, param0, maxit, tol) {
        if(is.null(param0)) {
                set.seed(10)
                param0 <- c(lambda = 0.5, m1 = sample(y, 1),
                            m2 = sample(y, 1), s1 = sd(y)/2, s2 = sd(y)/2)
        }
        d <- drv(y, param0, gradfun)
        ll0 <- d$likelihood
        grad <- d$gradient
        hess <- d$hessian
        alpha <- 1
        delta0 <- Inf
        for(i in seq_len(maxit)) {
                param <- drop(solve(hess, hess %*% param0 - grad * alpha))
                d <- drv(y, param, gradfun)
                ll <- d$likelihood
                ## delta <- abs(ll - ll0)
                delta <- sqrt(sum((param - param0)^2))
                cat(i, "delta:", delta, "\tLL:", ll, "\n")
                if(delta > delta0) {
                        alpha <- 0.95 * alpha
                        message(sprintf("resetting step length: %f", alpha))
                        next
                }
                if(delta < tol) {
                        convergence <- 0
                        break
                }
                else {
                        ll0 <- ll
                        grad <- d$gradient
                        hess <- d$hessian
                        delta0 <- delta
                }
        }
        if(i == maxit && delta >= tol)
                convergence <- 1
        browser()
        vcov <- -solve(hess)
        list(mle = param, stderr = sqrt(diag(vcov)), convergence = convergence)
}

gradfun <- deriv3(~ log(lambda * dnorm((y - m1)/s1)/s1 + (1-lambda) * dnorm((y-m2)/s2)/s2), c("lambda", "m1", "m2", "s1", "s2"), c("y", "lambda", "m1", "m2", "s1", "s2"))

drv <- function(y, param, gradfun) {
        g <- gradfun(y, param[1], param[2], param[3], param[4], param[5])
        list(likelihood = sum(g),
             gradient = colSums(attr(g, "gradient")),
             hessian = colSums(attr(g, "hessian"), dims = 1))
}

em.mix <- function(y, param0, maxit, tol) {
        if(is.null(param0)) {
                set.seed(10)
                param0 <- c(lambda = 0.5, m1 = sample(y, 1),
                            m2 = sample(y, 1), s1 = sd(y)/2, s2 = sd(y)/2)
        }
        for(i in seq_len(maxit)) {
                plist <- as.list(param0)
                z <- with(plist, (lambda * dnorm(y, m1, s1)) / (lambda*dnorm(y,m1,s1)+(1-lambda)*dnorm(y,m2,s2)))
                lambda <- mean(z)
                m1 <- weighted.mean(y, z)
                m2 <- weighted.mean(y, 1 - z)
                s1 <- with(plist, sqrt(sum(z * (y - m1)*(y - m1)) / sum(z)))
                s2 <- with(plist, sqrt(sum((1-z) * (y-m2)*(y-m2)) / (sum(1-z))))
                param <- c(lambda = lambda, m1 = m1, m2 = m2, s1 = s1, s2 = s2)
                delta <- sqrt(sum((param - param0)^2))
                if(delta < tol) {
                        convergence <- 0
                        break
                }
                else {
                        param0 <- param
                }
        }
        if(i == maxit && delta >= tol)
                convergence <- 1
        stderr <- compute.stderr(y, param)
        list(mle = param, stderr = stderr, convergence = convergence,
             iterations = i)
}

## Compute standard errors using Louis's method
compute.stderr <- function(y, param) {
        plist <- as.list(param)
        z <- with(plist, (lambda * dnorm(y, m1, s1)) / (lambda*dnorm(y,m1,s1)+(1-lambda)*dnorm(y,m2,s2)))
        S <- with(plist, {
                cbind(lambda = z / lambda - (1 - z) / (1 - lambda),
                      m1 = z * (y - m1) / s1^2,
                      m2 = (1 - z) * (y - m2) / s2^2,
                      s1 = z * (-1/s1 + (y - m1)^2 / s1^3),
                      s2 = (1 - z) * (-1/s2 + (y - m2)^2 / s2^3))
        })
        I <- crossprod(S)
        H <- solve(I)
        sqrt(diag(H))
}
