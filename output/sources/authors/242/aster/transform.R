astertransform <- function(arg, obj, from = c("unconditional", "conditional"),
    to.cond = c("unconditional", "conditional"),
    to.mean = c("mean.value", "canonical")) {
    from <- match.arg(from)
    to.cond <- match.arg(to.cond)
    to.mean <- match.arg(to.mean)
    stopifnot(inherits(obj, "aster"))
    nind <- nrow(obj$x)
    nnode <- ncol(obj$x)
    setfam(obj$famlist)
    stopifnot(is.numeric(arg))
    stopifnot(all(is.finite(arg)))
    stopifnot(length(arg) == nind * nnode)
    if (to.mean == "canonical") {
        if (from == "unconditional" && to.cond == "conditional") {
            result <- .C(C_aster_phi2theta, nind = as.integer(nind),
                nnode = as.integer(nnode), pred = as.integer(obj$pred),
                fam = as.integer(obj$fam), phi = as.double(arg),
                theta = matrix(as.double(0), nind, nnode))$theta
        }
        if (from == "conditional" && to.cond == "unconditional") {
            result <- .C(C_aster_theta2phi, nind = as.integer(nind),
                nnode = as.integer(nnode), pred = as.integer(obj$pred),
                fam = as.integer(obj$fam), theta = as.double(arg),
                phi = matrix(as.double(0), nind, nnode))$phi
        }
        if (from == to.cond) {
            result <- arg
        }
    } else {
        ### to.mean == "mean.value"
        if (from == "conditional") {
            theta <- arg
        }
        if (from == "unconditional") {
            theta <- .C(C_aster_phi2theta, nind = as.integer(nind),
                nnode = as.integer(nnode), pred = as.integer(obj$pred),
                fam = as.integer(obj$fam), phi = as.double(arg),
                theta = matrix(as.double(0), nind, nnode))$theta
        }
        result <- .C(C_aster_theta2ctau, nind = as.integer(nind),
            nnode = as.integer(nnode), pred = as.integer(obj$pred),
            fam = as.integer(obj$fam), theta = as.double(theta),
            ctau = matrix(as.double(0), nind, nnode))$ctau
        if (to.cond == "unconditional") {
            result <- .C(C_aster_ctau2tau, nind = as.integer(nind),
               nnode = as.integer(nnode), pred = as.integer(obj$pred),
               fam = as.integer(obj$fam), root = as.double(obj$root),
               ctau = as.double(result), tau = matrix(as.double(0),
                 nind, nnode))$tau
        }
    }
    clearfam()
    return(result)
}
