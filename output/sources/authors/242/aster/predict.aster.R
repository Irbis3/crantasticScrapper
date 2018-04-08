
predict.aster <- function(object, x, root, modmat, amat,
    parm.type = c("mean.value", "canonical"),
    model.type = c("unconditional", "conditional"),
    se.fit = FALSE, info = c("expected", "observed"),
    info.tol = sqrt(.Machine$double.eps), newcoef = NULL,
    gradient = se.fit, ...)
{
    parm.type <- match.arg(parm.type)
    model.type <- match.arg(model.type)
    info <- match.arg(info)

    if (! object$converged)
        stop("aster model fit not converged")

    if (missing(modmat)) {
        ##### "predict" observed data #####
        modmat <- object$modmat
        x <- object$x
        root <- object$root
    }

    setfam(object$famlist)

    stopifnot(is.logical(se.fit))
    stopifnot(length(se.fit) == 1)
    stopifnot(is.logical(gradient))
    stopifnot(length(gradient) == 1)

    stopifnot(all(dim(modmat)[2:3] == dim(object$modmat)[2:3]))
    stopifnot(all(dimnames(modmat)[[2]] == dimnames(object$modmat)[[2]]))
    stopifnot(all(dimnames(modmat)[[3]] == dimnames(object$modmat)[[3]]))
    if (parm.type == "mean.value") {
        stopifnot(dim(root) == dim(modmat)[1:2])
        if (model.type == "conditional") {
            stopifnot(dim(x) == dim(modmat)[1:2])
        }
    }

    if (parm.type == "mean.value") {
        if (missing(root))
            stop("parm.type == \"mean.value\" and root missing\n")
        if (model.type == "conditional") {
            if (missing(x))
                stop("parm.type == \"mean.value\" && model.type == \"conditional\" and x missing\n")
        }
    }

    if (! missing(amat)) {
        if (is.array(amat)) {
            if (length(dim(amat)) != 3)
                stop("amat is array but not 3-dimensional")
            if (! all(dim(amat)[1:2] == dim(modmat)[1:2]))
                stop("amat is array but dimensions 1 and 2 do not match modmat")
        } else {
            if (is.matrix(amat)) {
                if (dim(amat)[1] != prod(dim(modmat)[1:2]))
                    stop("amat is matrix but first dimension does not match dimensions 1 and 2 of modmat")
            } else {
                stop("amat is neither array nor matrix")
            }
        }
    }

    nind <- dim(modmat)[1]
    nnode <- dim(modmat)[2]
    ncoef <- dim(modmat)[3]
    if (ncoef != length(object$coefficients))
        stop("object$coefficients does not match dim(modmat)[3]")

    if (se.fit) {
        if (info == "expected")
            infomat <- object$fisher
        else
            infomat <- object$hessian

        fred <- eigen(infomat, symmetric = TRUE)
        sally <- fred$values < max(fred$values) * info.tol
        if (any(sally)) {
            cat("apparent null eigenvectors of information matrix\n")
            cat("directions of recession or constancy of log likelihood\n")
            print(zapsmall(fred$vectors[ , sally]))
            stop("cannot compute standard errors")
        }
    }

    beta <- object$coefficients
    if (! is.null(newcoef)) {
        stopifnot(is.numeric(newcoef))
        stopifnot(is.finite(newcoef))
        stopifnot(length(newcoef) == length(object$coefficients))
        beta <- newcoef
    }

    eta <- .C(C_aster_mat_vec_mult,
        nrow = as.integer(nind * nnode),
        ncol = as.integer(ncoef),
        a = as.double(modmat),
        b = as.double(beta),
        c = matrix(as.double(0), nind, nnode))$c

    origin <- object$origin
    stopifnot(is.numeric(origin))
    stopifnot(all(is.finite(origin)))
    stopifnot(is.matrix(origin))
    stopifnot(ncol(origin) == nnode)
    origin.row <- origin[1, ]
    origin <- matrix(origin.row, nrow = nind, ncol = nnode, byrow = TRUE)
    eta <- eta + origin

    ##### now we do 8 cases #####

    pred <- object$pred
    fam <- object$fam

    if (parm.type == "canonical") {
        if (model.type == object$type) {
            zeta <- eta
            if (se.fit | gradient)
                gradmat <- matrix(modmat, ncol = ncoef)
        } else {
            if (model.type == "unconditional") {
                ##### object$type == "conditional" #####
                zeta <- .C(C_aster_theta2phi,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    theta = as.double(eta),
                    phi = matrix(as.double(0), nind, nnode))$phi
                if (se.fit | gradient)
                    gradmat <- .C(C_aster_D_beta2theta2phi,
                        nind = as.integer(nind),
                        nnode = as.integer(nnode),
                        ncoef = as.integer(ncoef),
                        pred = as.integer(pred),
                        fam = as.integer(fam),
                        theta = as.double(eta),
                        modmat = as.double(modmat),
                        gradmat = matrix(as.double(0), nind * nnode, ncoef)
                        )$gradmat
            }
            if (model.type == "conditional") {
                ##### object$type == "unconditional" #####
                zeta <- .C(C_aster_phi2theta,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    phi = as.double(eta),
                    theta = matrix(as.double(0), nind, nnode))$theta
                if (se.fit | gradient)
                    gradmat <- .C(C_aster_D_beta2phi2theta,
                        nind = as.integer(nind),
                        nnode = as.integer(nnode),
                        ncoef = as.integer(ncoef),
                        pred = as.integer(pred),
                        fam = as.integer(fam),
                        theta = as.double(zeta),
                        modmat = as.double(modmat),
                        gradmat = matrix(as.double(0), nind * nnode, ncoef)
                        )$gradmat
            }
        }
    } else {
        ##### parm.type == "mean.value" #####
        if (model.type == "conditional" && object$type == "conditional") {
            ctau <- .C(C_aster_theta2ctau,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                theta = as.double(eta),
                ctau = matrix(as.double(0), nind, nnode))$ctau
            xpred <- .C(C_aster_xpred,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                x = as.double(x),
                root = as.double(root),
                xpred = double(nind * nnode))$xpred
            zeta <- xpred * ctau
            if (se.fit | gradient) {
                grad.ctau <- .C(C_aster_theta2whatsis,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    deriv = as.integer(2),
                    theta = as.double(eta),
                    result = double(nind * nnode))$result
                gradmat <- matrix(modmat, ncol = ncoef)
                gradmat <- sweep(gradmat, 1, xpred * grad.ctau, "*")
            }
        }
        if (model.type == "unconditional" && object$type == "unconditional") {
            theta <- .C(C_aster_phi2theta,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                phi = as.double(eta), 
                theta = matrix(as.double(0), nind, nnode))$theta
            ctau <- .C(C_aster_theta2ctau,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                theta = as.double(theta),
                ctau = matrix(as.double(0), nind, nnode))$ctau
            zeta <- .C(C_aster_ctau2tau,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                root = as.double(root),
                ctau = as.double(ctau),
                tau = matrix(as.double(0), nind, nnode))$tau
            if (se.fit | gradient)
                gradmat <- .C(C_aster_D_beta2phi2tau,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    ncoef = as.integer(ncoef),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    beta = as.double(beta),
                    root = as.double(root),
                    origin = as.double(origin),
                    modmat = as.double(modmat),
                    gradmat = matrix(as.double(0), nind * nnode, ncoef))$gradmat
        }
        if (model.type == "conditional" && object$type == "unconditional") {
            theta <- .C(C_aster_phi2theta,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                phi = as.double(eta), 
                theta = matrix(as.double(0), nind, nnode))$theta
            ctau <- .C(C_aster_theta2ctau,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                theta = as.double(theta),
                ctau = matrix(as.double(0), nind, nnode))$ctau
            xpred <- .C(C_aster_xpred,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                x = as.double(x),
                root = as.double(root),
                xpred = matrix(as.double(0), nind, nnode))$xpred
            zeta <- xpred * ctau
            if (se.fit | gradient) {
                gradmat <- .C(C_aster_D_beta2phi2theta,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    ncoef = as.integer(ncoef),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    theta = as.double(theta),
                    modmat = as.double(modmat),
                    gradmat = matrix(as.double(0), nind * nnode, ncoef))$gradmat
                grad.ctau <- .C(C_aster_theta2whatsis,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    deriv = as.integer(2),
                    theta = as.double(theta),
                    result = double(nind * nnode))$result
                deltheta2xi <- as.numeric(xpred) * grad.ctau
                gradmat <- sweep(gradmat, 1, deltheta2xi, "*")
            }
        }

        if (model.type == "unconditional" && object$type == "conditional") {
            ctau <- .C(C_aster_theta2ctau,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                theta = as.double(eta),
                ctau = matrix(as.double(0), nind, nnode))$ctau
            zeta <- .C(C_aster_ctau2tau,
                nind = as.integer(nind),
                nnode = as.integer(nnode),
                pred = as.integer(pred),
                fam = as.integer(fam),
                root = as.double(root),
                ctau = as.double(ctau),
                tau = matrix(as.double(0), nind, nnode))$tau
            if (se.fit | gradient)
                gradmat <- .C(C_aster_D_beta2theta2tau,
                    nind = as.integer(nind),
                    nnode = as.integer(nnode),
                    ncoef = as.integer(ncoef),
                    pred = as.integer(pred),
                    fam = as.integer(fam),
                    beta = as.double(beta),
                    root = as.double(root),
                    modmat = as.double(modmat),
                    gradmat = matrix(as.double(0), nind * nnode, ncoef))$gradmat
        }
    }

    clearfam()

    result <- as.double(zeta)
    if (! missing(amat)) {
        amat <- matrix(amat, nrow = nind * nnode)
        result <- as.numeric(t(amat) %*% result)
        if (se.fit | gradient)
            gradmat <- t(amat) %*% gradmat
    }
    if (! (se.fit | gradient)) {
        return(result)
    } else if (se.fit) {
        fred <- .C(C_aster_diag_mat_mat_mat_mult,
            nrow = nrow(gradmat),
            ncol = ncol(gradmat),
            a = as.double(gradmat),
            b = as.double(solve(infomat)),
            c = double(nrow(gradmat)))$c
        return(list(fit = result, se.fit = sqrt(as.numeric(fred)),
            gradient = gradmat))
    } else {
        # se.fit == FALSE & gradient == TRUE
        return(list(fit = result, gradient = gradmat))
    }
}

predict.aster.formula <- function(object, newdata, varvar, idvar, root, amat,
    parm.type = c("mean.value", "canonical"),
    model.type = c("unconditional", "conditional"),
    se.fit = FALSE, info = c("expected", "observed"),
    info.tol = sqrt(.Machine$double.eps), newcoef = NULL,
    gradient = se.fit, ...)
{
    parm.type <- match.arg(parm.type)
    model.type <- match.arg(model.type)
    info <- match.arg(info)

    if (missing(newdata)) {
        class(object) <- "aster"
        return(NextMethod("predict"))
    }

    tt <- object$terms
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("newdata", "varvar", "idvar", "root"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf$formula <- tt
    mf$xlev <- object$xlevels
    mf$data <- mf$newdata
    mf$newdata <- NULL
    mf <- eval.parent(mf)

    mt <- attr(mf, "terms")
    x <- model.response(mf, "numeric")
    if (is.empty.model(mt)) {
        stop("empty model")
    } else {
        modmat <- model.matrix(mt, mf)
    }

    varvar <- mf[["(varvar)"]]
    idvar <- mf[["(idvar)"]]
    root <- mf[["(root)"]]

    nind <- length(unique(idvar))
    nnode <- length(unique(varvar))
    if (nind * nnode != length(varvar))
        stop("nrow(data) not nind * nnode")
    varvarmat <- matrix(as.vector(varvar), nind, nnode)
    idvarmat <- matrix(as.vector(idvar), nind, nnode)
    foo <- apply(varvarmat, 2, function(x) length(unique(x)))
    bar <- apply(idvarmat, 1, function(x) length(unique(x)))
    if (! (all(foo == 1) & all(bar == 1)))
        stop("data not nind by nnode matrix with rows individuals and columns variables")
    varlab <- varvarmat[1, ]
    idlab <- idvarmat[ , 1]
    if (all(idlab == seq(along = idlab)))
        idlab <- NULL

    if (! is.numeric(x))
        stop("response not numeric")
    if (length(x) != nind * nnode)
        stop("response not nind by nnode matrix with rows individuals and columns variables")
    x <- matrix(x, nind, nnode)
    dimnames(x) <- list(idlab, varlab)

    if (! is.numeric(root))
        stop("root not numeric")
    if (length(root) != nind * nnode)
        stop("root not nind by nnode matrix with rows individuals and columns variables")
    root <- matrix(root, nind, nnode)
    dimnames(root) <- list(idlab, varlab)

    if (! is.numeric(root))
        stop("root not numeric")
    if (length(root) != nind * nnode)
        stop("root not nind by nnode matrix with rows individuals and columns variables")
    root <- matrix(root, nind, nnode)
    dimnames(root) <- list(idlab, varlab)

    if (! is.numeric(modmat))
        stop("model matrix not numeric")
    if (! is.matrix(modmat))
        stop("model matrix not matrix")
    if (nrow(modmat) != nind * nnode)
        stop("nrow of model matrix not nind * nnode")
    
    coeflab <- dimnames(modmat)[[2]]
    objcoeflab <- names(object$coefficients)
    if (! all(is.element(objcoeflab, coeflab)))
        stop("regression coefficients do not match in object and new model matrix")
    inies <- is.element(coeflab, objcoeflab)
    modmat <- modmat[ , inies]
    coeflab <- dimnames(modmat)[[2]]
    ncoef <- length(coeflab)

    modmat <- array(as.numeric(modmat), c(nind, nnode, ncoef))
    dimnames(modmat) <- list(idlab, varlab, coeflab)

    if (missing(amat)) {
        foo <- predict.aster(object, x, root, modmat,
            parm.type = parm.type, model.type = model.type,
            se.fit = se.fit, info = info, info.tol = info.tol,
            newcoef = newcoef, gradient = gradient, ...)
    } else {
        foo <- predict.aster(object, x, root, modmat, amat,
            parm.type = parm.type, model.type = model.type,
            se.fit = se.fit, info = info, info.tol = info.tol,
            newcoef = newcoef, gradient = gradient, ...)
    }
    if (is.list(foo)) {
        foo$modmat <- modmat
    }
    return(foo)
}

