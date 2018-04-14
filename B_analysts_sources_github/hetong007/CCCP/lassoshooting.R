soft.thred = function(x, tau) {
    sign(x)*max(abs(x)-tau, 0)
}

# Normal Lasso with Coordinate Descent
lasso.cd = function(x, y, lambda, max.step = 10000, thred = 0.001, verbose = FALSE) {
    n = nrow(x)
    p = ncol(x)
    if (length(y) != n)
        stop("Wrong length of y and x.")
    
    beta = rep(0, p)
    txx = colSums(x^2)
    xni.sum = rep(0, n)
    
    step = 1
    current.diff = Inf
    while (step <= max.step && thred < current.diff) {
        current.diff = 0
        for (i in 1:p) {
            up = t(x[,i,drop = FALSE]) %*% (y - xni.sum + x[,i]*beta[i])
            new.val = soft.thred(up/txx[i], lambda*n/txx[i])[1,1]
            
            val.diff = new.val - beta[i]
            current.diff = current.diff + abs(val.diff)
            
            beta[i] = new.val
            xni.sum = xni.sum + x[,i]*val.diff
        }
        if (verbose)
            cat(step, current.diff, '\n')
        step = step + 1
    }
    
    return(beta)
}

scad.loss = function(beta, lambda, gamma) {
    theta = abs(beta)
    if (theta <= lambda) {
        return(lambda*theta)
    }
    if (theta > gamma*lambda) {
        res = lambda^2*(gamma^2-1)/2/(gamma-1)
        return(res)
    }
    res = (lambda*theta*gamma-0.5*(theta^2+lambda^2))/(gamma-1)
    return(res)
}

lasso.cd.cccp = function(x, y, lambda, beta.init,
                         max.step = 10000, thred = 0.001, verbose = FALSE) {
    n = nrow(x)
    p = ncol(x)
    if (length(y) != n)
        stop("Wrong length of y and x.")
    
    beta = beta.init
    txx = colSums(x^2)
    xni.sum = colSums(t(x) * beta)
    param.a = 3.7
    
    step = 1
    current.loss = Inf
    last.loss = Inf
    loss.diff = Inf
    while (step <= max.step && thred < loss.diff) {
        #if (step > 100)
            #browser()
        last.loss = current.loss
        current.loss = 0
        for (i in 1:p) {
            up = t(x[,i,drop = FALSE]) %*% (y - xni.sum + x[,i]*beta[i])
            if (abs(beta[i]) <= lambda) {
                lambda.spec = lambda
            } else {
                lambda.spec = max(param.a*lambda-abs(beta[i]), 0)/(param.a-1)
            }
            
            new.val = soft.thred(up/txx[i], lambda.spec*n/txx[i])[1,1]
            
            val.diff = new.val - beta[i]
            beta[i] = new.val
            xni.sum = xni.sum + x[,i]*val.diff
            
            val.loss = scad.loss(beta[i], lambda, param.a)
            current.loss = current.loss + val.loss
        }
        
        current.loss = current.loss + mean((y-x %*% beta)^2)/2
        if (verbose) {
            cat(step, last.loss - current.loss, '\n')
        }
        
        loss.diff = abs(current.loss - last.loss)
        step = step + 1
    }
    
    return(beta)
}

fscad = function(z, lambda, gamma) {
    zz = abs(z)
    
    if (zz <= 2*lambda) {
        res = soft.thred(z, lambda)
    } else if (2*lambda < zz && zz <= gamma * lambda) {
        res = soft.thred(z, gamma*lambda/(gamma-1))/(1-1/(gamma-1))
    } else {
        res = z
    }
    
    return(res)
}

# Assume x is already standarized to be diag(t(x) %*% x) == rep(n, n)
lasso.cd.ncv = function(x, y, lambda, beta.init,
                        max.step = 10000, thred = 0.001, verbose = FALSE) {
    n = nrow(x)
    p = ncol(x)
    if (length(y) != n)
        stop("Wrong length of y and x.")
    
    beta = beta.init
    txx = colSums(x^2)
    param.a = 3.7
    r = y - x %*% beta
    
    step = 1
    current.diff = Inf
    while (step <= max.step && thred < current.diff) {
        current.diff = 0
        for (i in 1:p) {
            z = mean(x[,i]*r) + beta[i]
            
            new.val = fscad(z, lambda, param.a)
            
            val.diff = new.val - beta[i]
            r = r - val.diff * x[,i]
            beta[i] = new.val
            
            current.diff = current.diff + val.diff
        }
        
        if (verbose) {
            cat(step, current.diff, '\n')
        }
        
        step = step + 1
    }
    
    return(beta)
}

cccp.solve = function(x, y, lambda, method = "ncv", 
                      max.step = 10000, thred = 0.001,
                      verbose = FALSE) {
    n = nrow(x)
    if (n <= 0)
        stop("Wrong input X")
    
    bhat_1 = lasso.cd(x, y, lambda/log(n), max.step, thred, verbose)
    if (method == "cccp") {
        bhat_res = lasso.cd.cccp(x, y, lambda, bhat_1, max.step, thred, verbose)
    } else if (method == "ncv") {
        bhat_res = lasso.cd.ncv(x, y, lambda, bhat_1, max.step, thred, verbose)
    } else {
        stop("Wrong parameter `method`")
    }
    return(list(bhat_lasso = bhat_1, bhat_res = bhat_res))
}

hbic.calc = function(x, y, lambda, beta) {
    n = nrow(x)
    p = ncol(x)
    if (n <= 1)
        stop("Wrong Input")
    
    res = log(mean((y-x %*% beta)^2)) + 
        sum(beta != 0) * log(log(n)) * log(p) / n
    return(res)
}

# helper function to calculate MSE of beta
mse = function(beta, bhat) {
    sum((beta-bhat)^2)
}

true.pos = function(beta, bhat) {
    sum((bhat != 0) * (beta != 0))
}

false.pos = function(beta, bhat) {
    sum((bhat != 0) * (beta == 0))
}

HBIC = function(x, y, lambda.vec, method = "cccp", beta,
                max.step = 10000, thred = 0.001, verbose = FALSE) {
    len = length(lambda.vec)
    
    lasso_bhat = list()

    final_bhat = list()
    hbic.val = rep(0, len)
    mse.val = rep(0, len)
    loss.val = rep(0, len)
    tp.val = rep(0, len)
    fp.val = rep(0, len)
    
    cat('  ld\tHBIC\t\t MSE\t\t SCAD\t\t TP\t FP\n')
    
    for (i in 1:len) {
        lambda = lambda.vec[i]
        cat(i, lambda, '\t')
        
        res = cccp.solve(x, y, lambda, method, max.step, thred, verbose)
        bhat_lasso = res$bhat_lasso
        bhat = res$bhat_res
        
        hbic.val[i] = hbic.calc(x, y, lambda, bhat)
        mse.val[i] = mse(bhat, beta)
        loss.val[i] = mean((Y-X %*% bhat)^2)/2 +
            sum(sapply(bhat, scad.loss, lambda, 3.7))
        tp.val[i] = true.pos(beta, bhat)
        fp.val[i] = false.pos(beta, bhat)
        
        cat(hbic.val[i], '\t', mse.val[i], '\t', loss.val[i], '\t',
            tp.val[i], '\t', fp.val[i], '\n')
        
        lasso_bhat[[i]] = bhat_lasso
        final_bhat[[i]] = bhat
    }
    
    return(list(bhat = res,
                hbic.val = hbic.val, mse.val = mse.val, loss.val = loss.val,
                tp.val = tp.val, fp.val = fp.val))
}

rmse = function(y, pred) {
    sqrt(mean((y-pred)^2))
}

cv.cutfold = function(n, k) {
    ind = sample(n)
    m = n %/% k
    res = vector(k, mode='list')
    for (i in 1:k) {
        res[[i]] = ind[((i-1)*m+1):(i*m)]
        i = i + 1
    }
    L = m*k+1
    i = 1
    while (L <= n) {
        res[[i]] = c(res[[i]],ind[L])
        i = i+1
        L = L + 1
    }
    
    return(res)
}

lasso.cv = function(X, Y, lambda.vec, beta, nfold = 5, seed = NULL) {
    m = length(lambda.vec)
    n = nrow(X)
    rmse.val = rep(0, m)
    mse.val = rep(0, m)
    bhat.list = list()
    tp.val = rep(0, m)
    fp.val = rep(0, m)
    
    if (!is.null(seed))
        set.seed(seed)
    
    for (i in 1:m) {
        lambda = lambda.vec[i]
        cat(i,lambda,'\t\t')
        cv.ind = cv.cutfold(n, nfold)
        rmse.vec = rep(0, nfold)
        
        for (k in 1:nfold) {
            cat(k,' ')
            test = cv.ind[[k]]
            train = setdiff(1:n, test)
            bhat = lasso.cd(X[train,], Y[train], lambda)
            # browser()
            rmse.vec[k] = rmse(X[test,] %*% bhat, Y[test])
        }
        bhat = lasso.cd(X, Y, lambda)
        rmse.val[i] = mean(rmse.vec)
        mse.val[i] = mse(beta, bhat)
        bhat.list[[i]] = bhat
        tp.val[i] = true.pos(beta, bhat)
        fp.val[i] = false.pos(beta, bhat)
        
        cat(mse.val[i], '\t', rmse.val[i], '\n')
    }
    
    return(list(bhat = bhat.list, rmse = rmse.val, mse = mse.val, lambda = lambda.vec,
                tp.val = tp.val, fp.val = fp.val))
}
