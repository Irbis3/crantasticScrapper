
transformSaturated <- function(parm, data,
    from = c("theta", "phi", "xi", "mu"), to = c("theta", "phi", "xi", "mu"),
    differential, model.type = c("unconditional", "conditional"),
    tolerance = 8 * .Machine$double.eps)
{
    from <- match.arg(from)
    to <- match.arg(to)
    model.type <- match.arg(model.type)
    stopifnot(inherits(data, "asterdata"))
    validasterdata(data)
    stopifnot(is.atomic(parm))
    stopifnot(is.numeric(parm))
    stopifnot(is.finite(parm))
    stopifnot(length(parm) == length(data))
    stopifnot(is.atomic(tolerance))
    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > 0)
    fam.set.tolerance(tolerance)
    fam.clear()
    for (i in seq(along = data$families))
        fam.set(data$families[[i]])
    result <- NULL
    if (missing(differential)) {
        if (from == to)
            result <- as.vector(parm)
        if (from == "theta" && to == "phi") {
            if (! is.validThetaNoSetNoClear(data, parm))
                stop("invalid theta vector")
            out <- .C(C_aster_theta_to_phi,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(parm),
                dtheta = double(0),
                phi = double(length(parm)),
                dphi = double(0))
            result <- out$phi
        }
        if (from == "phi" && to == "theta") {
            out <- .C(C_aster_phi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                phi = as.double(parm),
                dphi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            result <- out$theta
            if (! is.validThetaNoSetNoClear(data, result))
                stop("phi vector maps to invalid theta vector")
        }
        if (from == "theta" && to == "xi") {
            if (! is.validThetaNoSetNoClear(data, parm))
                stop("invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(parm),
                dtheta = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            result <- out$xi
        }
        if (from == "xi" && to == "theta") {
            if (! is.validXiNoSetNoClear(data, parm))
                stop("invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(parm),
                dxi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            result <- out$theta
        }
        if (from == "xi" && to == "mu") {
            if (! is.validXiNoSetNoClear(data, parm))
                stop("invalid xi vector")
            out <- .C(C_aster_xi_to_mu,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                xi = as.double(parm),
                dxi = double(0),
                mu = double(length(parm)),
                dmu = double(0))
            result <- out$mu
        }
        if (from == "mu" && to == "xi") {
            out <- .C(C_aster_mu_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                mu = as.double(parm),
                dmu = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            if (! is.validXiNoSetNoClear(data, out$xi))
                stop("mu vector maps to invalid xi vector")
            result <- out$xi
        }
        if (from == "theta" && to == "mu") {
            if (! is.validThetaNoSetNoClear(data, parm))
                stop("invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(parm),
                dtheta = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            out <- .C(C_aster_xi_to_mu,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                xi = out$xi,
                dxi = double(0),
                mu = double(length(parm)),
                dmu = double(0))
            result <- out$mu
        }
        if (from == "xi" && to == "phi") {
            if (! is.validXiNoSetNoClear(data, parm))
                stop("invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(parm),
                dxi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            out <- .C(C_aster_theta_to_phi,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = out$theta,
                dtheta = double(0),
                phi = double(length(parm)),
                dphi = double(0))
            result <- out$phi
        }
        if (from == "phi" && to == "xi") {
            out <- .C(C_aster_phi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                phi = as.double(parm),
                dphi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            if (! is.validThetaNoSetNoClear(data, out$theta))
                stop("phi vector maps to invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(out$theta),
                dtheta = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            result <- out$xi
        }
        if (from == "phi" && to == "mu") {
            out <- .C(C_aster_phi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                phi = as.double(parm),
                dphi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            if (! is.validThetaNoSetNoClear(data, out$theta))
                stop("phi vector maps to invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(out$theta),
                dtheta = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            out <- .C(C_aster_xi_to_mu,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                xi = as.double(out$xi),
                dxi = double(0),
                mu = double(length(parm)),
                dmu = double(0))
            result <- out$mu
        }
        if (from == "mu" && to == "theta") {
            out <- .C(C_aster_mu_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                mu = as.double(parm),
                dmu = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            if (! is.validXiNoSetNoClear(data, out$xi))
                stop("mu vector maps to invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(out$xi),
                dxi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            result <- out$theta
        }
        if (from == "mu" && to == "phi") {
            out <- .C(C_aster_mu_to_xi,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                mu = as.double(parm),
                dmu = double(0),
                xi = double(length(parm)),
                dxi = double(0))
            if (! is.validXiNoSetNoClear(data, out$xi))
                stop("mu vector maps to invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(0),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(out$xi),
                dxi = double(0),
                theta = double(length(parm)),
                dtheta = double(0))
            out <- .C(C_aster_theta_to_phi,
                nnode = length(parm),
                deriv = as.integer(0),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(out$theta),
                dtheta = double(0),
                phi = double(length(parm)),
                dphi = double(0))
            result <- out$phi
        }
    } else {
        ##### ! missing(differential) #####
        stopifnot(is.atomic(differential))
        stopifnot(is.numeric(differential))
        stopifnot(is.finite(differential))
        stopifnot(length(differential) == length(parm))
        if (from == to)
            result <- as.vector(differential)
        if (from == "theta" && to == "phi") {
            if (! is.validThetaNoSetNoClear(data, parm))
                stop("invalid theta vector")
            out <- .C(C_aster_theta_to_phi,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(parm),
                dtheta = as.double(differential),
                phi = double(length(parm)),
                dphi = double(length(parm)))
            result <- out$dphi
        }
        if (from == "phi" && to == "theta") {
            out <- .C(C_aster_phi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                phi = as.double(parm),
                dphi = as.double(differential),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            result <- out$dtheta
            if (! is.validThetaNoSetNoClear(data, out$theta))
                stop("phi vector maps to invalid theta vector")
        }
        if (from == "theta" && to == "xi") {
            if (! is.validThetaNoSetNoClear(data, parm))
                stop("invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(parm),
                dtheta = as.double(differential),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            result <- out$dxi
        }
        if (from == "xi" && to == "theta") {
            if (! is.validXiNoSetNoClear(data, parm))
                stop("invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(parm),
                dxi = as.double(differential),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            result <- out$dtheta
        }
        if (from == "xi" && to == "mu") {
            if (! is.validXiNoSetNoClear(data, parm))
                stop("invalid xi vector")
            out <- .C(C_aster_xi_to_mu,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                xi = as.double(parm),
                dxi = as.double(differential),
                mu = double(length(parm)),
                dmu = double(length(parm)))
            result <- out$dmu
        }
        if (from == "mu" && to == "xi") {
            out <- .C(C_aster_mu_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                mu = as.double(parm),
                dmu = as.double(differential),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            if (! is.validXiNoSetNoClear(data, out$xi))
                stop("mu vector maps to invalid xi vector")
            result <- out$dxi
        }
        if (from == "theta" && to == "mu") {
            if (! is.validThetaNoSetNoClear(data, parm))
                stop("invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(parm),
                dtheta = as.double(differential),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            out <- .C(C_aster_xi_to_mu,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                xi = out$xi,
                dxi = out$dxi,
                mu = double(length(parm)),
                dmu = double(length(parm)))
            result <- out$dmu
        }
        if (from == "xi" && to == "phi") {
            if (! is.validXiNoSetNoClear(data, parm))
                stop("invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(parm),
                dxi = as.double(differential),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            out <- .C(C_aster_theta_to_phi,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = out$theta,
                dtheta = out$dtheta,
                phi = double(length(parm)),
                dphi = double(length(parm)))
            result <- out$dphi
        }
        if (from == "phi" && to == "xi") {
            out <- .C(C_aster_phi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                phi = as.double(parm),
                dphi = as.double(differential),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            if (! is.validThetaNoSetNoClear(data, out$theta))
                stop("phi vector maps to invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(out$theta),
                dtheta = as.double(out$dtheta),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            result <- out$dxi
        }
        if (from == "phi" && to == "mu") {
            out <- .C(C_aster_phi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                phi = as.double(parm),
                dphi = as.double(differential),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            if (! is.validThetaNoSetNoClear(data, out$theta))
                stop("phi vector maps to invalid theta vector")
            out <- .C(C_aster_theta_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(out$theta),
                dtheta = as.double(out$dtheta),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            out <- .C(C_aster_xi_to_mu,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                xi = as.double(out$xi),
                dxi = as.double(out$dxi),
                mu = double(length(parm)),
                dmu = double(length(parm)))
            result <- out$dmu
        }
        if (from == "mu" && to == "theta") {
            out <- .C(C_aster_mu_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                mu = as.double(parm),
                dmu = as.double(differential),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            if (! is.validXiNoSetNoClear(data, out$xi))
                stop("mu vector maps to invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(out$xi),
                dxi = as.double(out$dxi),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            result <- out$dtheta
        }
        if (from == "mu" && to == "phi") {
            out <- .C(C_aster_mu_to_xi,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                initial = as.double(data$initial),
                mu = as.double(parm),
                dmu = as.double(differential),
                xi = double(length(parm)),
                dxi = double(length(parm)))
            if (! is.validXiNoSetNoClear(data, out$xi))
                stop("mu vector maps to invalid xi vector")
            out <- .C(C_aster_xi_to_theta,
                nnode = length(parm),
                deriv = as.integer(1),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                xi = as.double(out$xi),
                dxi = as.double(out$dxi),
                theta = double(length(parm)),
                dtheta = double(length(parm)))
            out <- .C(C_aster_theta_to_phi,
                nnode = length(parm),
                deriv = as.integer(1),
                pred = as.integer(data$repred),
                group = as.integer(data$regroup),
                code = as.integer(data$recode),
                delta = as.double(data$redelta),
                theta = as.double(out$theta),
                dtheta = as.double(out$dtheta),
                phi = double(length(parm)),
                dphi = double(length(parm)))
            result <- out$dphi
        }
    }
    fam.reset.tolerance()
    fam.clear()
    if (is.null(result)) stop("Can't happen, this is a bug")
    return(result)
}

transformConditional <- function(parm, modmat, data, from = "beta",
    to = c("theta", "phi", "xi", "mu"), differential, offset,
    tolerance = 8 * .Machine$double.eps)
{
    from <- match.arg(from)
    to <- match.arg(to)
    stopifnot(inherits(data, "asterdata"))
    validasterdata(data)
    stopifnot(is.atomic(parm))
    stopifnot(is.numeric(parm))
    stopifnot(is.finite(parm))
    stopifnot(is.atomic(modmat))
    stopifnot(is.numeric(modmat))
    stopifnot(is.finite(modmat))
    stopifnot(is.matrix(modmat))
    stopifnot(nrow(modmat) == length(data))
    stopifnot(ncol(modmat) == length(parm))
    stopifnot(is.atomic(tolerance))
    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > 0)
    if (missing(offset))
        offset <- rep(0, length(data))
    stopifnot(is.atomic(offset))
    stopifnot(is.numeric(offset))
    stopifnot(is.finite(offset))
    stopifnot(length(offset) == length(data))
    result <- NULL
    if (missing(differential)) {
        result <- transformSaturated(offset + modmat %*% parm, data,
            from = "theta", to = to, tolerance = tolerance)
    } else {
        stopifnot(is.atomic(differential))
        stopifnot(is.numeric(differential))
        stopifnot(is.finite(differential))
        stopifnot(length(differential) == length(parm))
        result <- transformSaturated(offset + modmat %*% parm, data,
            from = "theta", to = to, differential = modmat %*% differential,
            tolerance = tolerance)
    }
    return(result)
}

transformUnconditional <- function(parm, modmat, data,
    from = c("beta", "tau"), to = c("beta", "theta", "phi", "xi", "mu", "tau"),
    differential, offset, tolerance = 8 * .Machine$double.eps)
{
    from <- match.arg(from)
    to <- match.arg(to)
    stopifnot(inherits(data, "asterdata"))
    validasterdata(data)
    stopifnot(is.atomic(parm))
    stopifnot(is.numeric(parm))
    stopifnot(is.finite(parm))
    stopifnot(is.atomic(modmat))
    stopifnot(is.numeric(modmat))
    stopifnot(is.finite(modmat))
    stopifnot(is.matrix(modmat))
    stopifnot(nrow(modmat) == length(data))
    stopifnot(ncol(modmat) == length(parm))
    stopifnot(is.atomic(tolerance))
    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > 0)
    if (missing(offset))
        offset <- rep(0, length(data))
    stopifnot(is.atomic(offset))
    stopifnot(is.numeric(offset))
    stopifnot(is.finite(offset))
    stopifnot(length(offset) == length(data))
    result <- NULL
    if (missing(differential)) {
        if (from == to)
            result <- parm
        if (from == "beta" && to %in% c("theta", "phi", "xi", "mu"))
            result <- transformSaturated(offset + modmat %*% parm, data,
                from = "phi", to = to, tolerance = tolerance)
        if (from == "beta" && to == "tau")
            result <- as.vector(t(modmat) %*%
                transformSaturated(offset + modmat %*% parm, data,
                from = "phi", to = "mu", tolerance = tolerance))
    } else {
        stopifnot(is.atomic(differential))
        stopifnot(is.numeric(differential))
        stopifnot(is.finite(differential))
        stopifnot(length(differential) == length(parm))
        if (from == to)
            result <- differential
        if (from == "beta" && to %in% c("theta", "phi", "xi", "mu"))
            result <- transformSaturated(offset + modmat %*% parm, data,
                from = "phi", to = to, differential = modmat %*% differential,
                tolerance = tolerance)
        if (from == "beta" && to == "tau")
            result <- as.vector(t(modmat) %*%
                transformSaturated(offset + modmat %*% parm, data,
                from = "phi", to = "mu",
                differential = modmat %*% differential, tolerance = tolerance))
    }
    if (from == "tau" & to != "tau") {
        if (is.null(colnames(modmat))) {
            colnames(modmat) <- paste("m", 1:ncol(modmat), sep = "")
        }
        consmat <- constancy(data, parm.type = "phi")
        if (nrow(consmat) > 0) {
            consmat.qr <- qr(t(consmat))
            modmat.resid <- qr.resid(consmat.qr, modmat)
        } else {
            modmat.resid <- modmat
        }
        d <- dim(modmat.resid)
        tol <- max(d) * .Machine$double.eps
        modmat.resid.qr <- qr(modmat.resid, tol = tol, LAPACK = TRUE)
        modmat.resid.r <- qr.R(modmat.resid.qr)
        diagR <- diag(modmat.resid.r)
        d.i <- abs(diagR)
        inies <- colnames(modmat.resid.r)[d.i >= tol * max(d.i)]
        outies <- colnames(modmat.resid.r)[d.i < tol * max(d.i)]
        baz <- colnames(modmat) %in% inies
        my.modmat <- modmat[ , baz]

        theta.start <- starting(data)
        phi.start <- transformSaturated(theta.start, data,
            from = "theta", to = "phi")
        my.modmat.qr <- qr(my.modmat)
        my.beta.start <- qr.coef(my.modmat.qr, phi.start - offset)
        phi.start <- as.numeric(offset + my.modmat %*% my.beta.start)
        theta.start <- transformSaturated(phi.start, data,
            from = "phi", to = "theta", tolerance = tolerance)
        if (! is.validtheta(data, theta.start)) {
            stop(paste("cannot find valid beta to start iteration,",
               " try different offset"))
        }

        my.tau <- parm[baz]
        gradfun <- function(x) my.tau - transformUnconditional(x,
            my.modmat, data, from = "beta", to = "tau",
            offset = offset)
        hessfun <- function(x) - jacobian(x, data,
            transform = "unconditional",
            from = "beta", to = "tau", modmat = my.modmat,
            offset = offset)
        out <- outer.loop(my.beta.start, gradfun, hessfun, my.tau,
            fudge = 0.05, tol1 = sqrt(.Machine$double.eps), tol2 = 0.10)
        my.beta <- out$x
        beta <- rep(0, length(parm))
        beta[baz] <- my.beta
        if (missing(differential)) {
            if (to == "beta") {
                result <- beta
            } else {
                phi <- as.numeric(offset + modmat %*% beta)
                if (to == "phi") {
                    result <- phi
                } else {
                    result <- transformSaturated(phi, data,
                        from = "phi", to = to, tolerance = tolerance)
                }
            }
        } else {
            my.dtau <- differential[baz]
            my.hess <- hessfun(my.beta)
            my.dbeta <- solve(- my.hess, my.dtau)
            dbeta <- rep(0, length(parm))
            dbeta[baz] <- my.dbeta
            if (to == "beta") {
                result <- dbeta
            } else {
                phi <- as.numeric(offset + modmat %*% beta)
                dphi <- as.numeric(modmat %*% dbeta)
                if (to == "phi") {
                    result <- dphi
                } else {
                    result <- transformSaturated(phi, data,
                        from = "phi", to = to, differential = dphi,
                        tolerance = tolerance)
                }
            }
        }
    }
    if (is.null(result)) stop("transformation not implemented yet")
    return(result)
}

jacobian <- function(parm, data,
    transform = c("saturated", "conditional", "unconditional"),
    from = c("beta", "theta", "phi", "xi", "mu", "tau"),
    to = c("beta", "theta", "phi", "xi", "mu", "tau"),
    modmat, offset, tolerance = 8 * .Machine$double.eps)
{
    transform <- match.arg(transform)
    from <- match.arg(from)
    to <- match.arg(to)
    foo <- switch(transform, saturated = transformSaturated,
        conditional = transformConditional,
        unconditional = transformUnconditional)
    fred <- as.list(args(foo))
    fred.from <- eval(fred$from)
    fred.to <- eval(fred$to)
    if (! (from %in% fred.from))
        stop(paste("from = \"", from, "\" not valid for transform = \"",
            transform, "\"", sep = ""))
    if (! (to %in% fred.to))
        stop(paste("to = \"", to, "\" not valid for transform = \"",
            transform, "\"", sep = ""))
    stopifnot(inherits(data, "asterdata"))
    validasterdata(data)
    stopifnot(is.atomic(parm))
    stopifnot(is.numeric(parm))
    stopifnot(is.finite(parm))
    stopifnot(is.atomic(tolerance))
    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > 0)
    if (transform == "saturated") {
        stopifnot(length(parm) == length(data))
        bar <- function(baz) foo(parm, data, from = from, to = to,
            differential = baz, tolerance = tolerance)
    } else {
        stopifnot(is.atomic(modmat))
        stopifnot(is.numeric(modmat))
        stopifnot(is.finite(modmat))
        stopifnot(is.matrix(modmat))
        stopifnot(nrow(modmat) == length(data))
        stopifnot(ncol(modmat) == length(parm))
        if (missing(offset))
            offset <- rep(0, length(data))
        stopifnot(is.atomic(offset))
        stopifnot(is.numeric(offset))
        stopifnot(is.finite(offset))
        stopifnot(length(offset) == length(data))
        bar <- function(baz) foo(parm, modmat, data, from = from, to = to,
            differential = baz, offset = offset, tolerance = tolerance)
    }
    zeros <- rep(0, length(parm))
    e1 <- zeros
    e1[1] <- 1
    v1 <- bar(e1)
    result <- matrix(NA, nrow = length(v1), ncol = length(e1))
    result[ , 1] <- v1
    if (length(parm) > 1) {
        for (j in 2:length(parm)) {
            ej <- zeros
            ej[j] <- 1
            vj <- bar(ej)
            result[ , j] <- vj
        }
    }
    return(result)
}

validtheta <- function(data, theta,
    model.type = c("unconditional", "conditional"),
    tolerance = 8 * .Machine$double.eps)
{
    model.type <- match.arg(model.type)
    stopifnot(inherits(data, "asterdata"))
    validasterdata(data)
    stopifnot(is.atomic(theta))
    stopifnot(is.numeric(theta))
    stopifnot(is.finite(theta))
    stopifnot(length(theta) == length(data))
    stopifnot(is.atomic(tolerance))
    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > 0)
    fam.set.tolerance(tolerance)
    fam.clear()
    for (i in seq(along = data$families))
        fam.set(data$families[[i]])
    .C(C_aster_validate_theta, nnode = length(theta),
        pred = as.integer(data$repred), group = as.integer(data$regroup),
        code = as.integer(data$recode),
        want.uam = model.type == "unconditional",
        resp = as.double(data$redata[[data$response.name]]),
        delta = as.double(data$redelta), theta = as.double(theta))
    fam.reset.tolerance()
    fam.clear()
    invisible(TRUE)
}

is.validtheta <- function(data, theta,
    model.type = c("unconditional", "conditional"),
    tolerance = 8 * .Machine$double.eps)
{
    model.type <- match.arg(model.type)
    out <- try(validtheta(data, theta, model.type, tolerance), silent = TRUE)
    return(! inherits(out, "try-error"))
}

## need for internal use only an is.validtheta that doesn't set families
## and doesn't clear them either

is.validThetaNoSetNoClear <- function(data, theta,
    model.type = c("unconditional", "conditional"))
{
    model.type <- match.arg(model.type)
    stopifnot(inherits(data, "asterdata"))
    stopifnot(is.atomic(theta))
    stopifnot(is.numeric(theta))
    stopifnot(is.finite(theta))
    stopifnot(length(theta) == length(data))
    out <- try(.C(C_aster_validate_theta, nnode = length(theta),
        pred = as.integer(data$repred), group = as.integer(data$regroup),
        code = as.integer(data$recode),
        want.uam = model.type == "unconditional",
        resp = as.double(data$redata[[data$response.name]]),
        delta = as.double(data$redelta), theta = as.double(theta)),
        silent = TRUE)
    return(! inherits(out, "try-error"))
}

validxi <- function(data, xi,
    model.type = c("unconditional", "conditional"),
    tolerance = 8 * .Machine$double.eps)
{
    model.type <- match.arg(model.type)
    stopifnot(inherits(data, "asterdata"))
    validasterdata(data)
    stopifnot(is.atomic(xi))
    stopifnot(is.numeric(xi))
    stopifnot(is.finite(xi))
    stopifnot(length(xi) == length(data))
    stopifnot(is.atomic(tolerance))
    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > 0)
    fam.set.tolerance(tolerance)
    fam.clear()
    for (i in seq(along = data$families))
        fam.set(data$families[[i]])
    .C(C_aster_validate_xi, nnode = length(xi),
        pred = as.integer(data$repred), group = as.integer(data$regroup),
        code = as.integer(data$recode),
        want.uam = model.type == "unconditional",
        resp = as.double(data$redata[[data$response.name]]),
        delta = as.double(data$redelta), xi = as.double(xi))
    fam.reset.tolerance()
    fam.clear()
    invisible(TRUE)
}

is.validxi <- function(data, xi,
    model.type = c("unconditional", "conditional"),
    tolerance = 8 * .Machine$double.eps)
{
    model.type <- match.arg(model.type)
    stopifnot(inherits(data, "asterdata"))
    out <- try(validxi(data, xi, model.type, tolerance), silent = TRUE)
    return(! inherits(out, "try-error"))
}

## also need for internal use only an is.validxi that doesn't set families
## and doesn't clear them either

is.validXiNoSetNoClear <- function(data, xi,
    model.type = c("unconditional", "conditional"))
{
    stopifnot(inherits(data, "asterdata"))
    stopifnot(is.atomic(xi))
    stopifnot(is.numeric(xi))
    stopifnot(is.finite(xi))
    stopifnot(length(xi) == length(data))
    out <- try(.C(C_aster_validate_xi, nnode = length(xi),
        pred = as.integer(data$repred), group = as.integer(data$regroup),
        code = as.integer(data$recode),
        want.uam = model.type == "unconditional",
        resp = as.double(data$redata[[data$response.name]]),
        delta = as.double(data$redelta), xi = as.double(xi)), silent = TRUE)
    return(! inherits(out, "try-error"))
}

