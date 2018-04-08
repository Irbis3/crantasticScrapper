#' @export
#' 
#' @title MLE Fitting of Normal Bulk and GPD for Both Tails with 
#'  Single Continuity Constraint at Both Thresholds Extreme Value Mixture Model
#'
#' @description Maximum likelihood estimation for fitting the extreme value 
#' mixture model with normal for bulk distribution between thresholds and conditional
#' GPDs for both tails with continuity at thresholds. With options for profile likelihood estimation for both thresholds and
#' fixed threshold approach.
#'
#' @inheritParams fgng
#' 
#' @details The extreme value mixture model with normal bulk and GPD for both tails
#' with continuity at thresholds is 
#' fitted to the entire dataset using maximum likelihood estimation. The estimated
#' parameters, variance-covariance matrix and their standard errors are automatically
#' output.
#' 
#' See help for \code{\link[evmix:fnormgpd]{fnormgpd}} and 
#' \code{\link[evmix:fgng]{fgng}}for details, type \code{help fnormgpd} and \code{help fgng}. 
#' Only the different features are outlined below for brevity.
#' 
#' The GPD \code{sigmaul} and \code{sigmaur} parameters are now specified as function of
#' other parameters, see 
#' help for \code{\link[evmix:gngcon]{dgngcon}} for details, type \code{help gngcon}.
#' Therefore, \code{sigmaul} and \code{sigmaur} should not be included in the parameter
#' vector if initial values are provided, making the full parameter vector 
#' The full parameter vector is
#' (\code{nmean}, \code{nsd}, \code{ul}, \code{xil}, \code{ur}, \code{xir})
#' if thresholds are also estimated and
#' (\code{nmean}, \code{nsd}, \code{xil}, \code{xir})
#' for profile likelihood or fixed threshold approach.
#' 
#' If the profile likelihood approach is used, then a grid search over all combinations of both thresholds
#' is carried out. The combinations which lead to less than 5 in any datapoints beyond the thresholds are not considered.
#' 
#' @return Log-likelihood is given by \code{\link[evmix:fgngcon]{lgngcon}} and it's
#'   wrappers for negative log-likelihood from \code{\link[evmix:fgngcon]{nlgngcon}}
#'   and \code{\link[evmix:fgngcon]{nlugngcon}}. Profile likelihood for both
#'   thresholds given by \code{\link[evmix:fgngcon]{proflugngcon}}. Fitting function
#'   \code{\link[evmix:fgngcon]{fgngcon}} returns a simple list with the
#'   following elements
#'
#' \tabular{ll}{
#'  \code{call}:      \tab \code{optim} call\cr
#'  \code{x}:         \tab data vector \code{x}\cr
#'  \code{init}:      \tab \code{pvector}\cr
#'  \code{fixedu}:    \tab fixed thresholds, logical\cr
#'  \code{ulseq}:     \tab lower threshold vector for profile likelihood or scalar for fixed threshold\cr
#'  \code{urseq}:     \tab upper threshold vector for profile likelihood or scalar for fixed threshold\cr
#'  \code{nllhuseq}:  \tab profile negative log-likelihood at each threshold pair in (ulseq, urseq)\cr
#'  \code{optim}:     \tab complete \code{optim} output\cr
#'  \code{mle}:       \tab vector of MLE of parameters\cr
#'  \code{cov}:       \tab variance-covariance matrix of MLE of parameters\cr
#'  \code{se}:        \tab vector of standard errors of MLE of parameters\cr
#'  \code{rate}:      \tab \code{phiu} to be consistent with \code{\link[evd:fpot]{evd}}\cr
#'  \code{nllh}:      \tab minimum negative log-likelihood\cr
#'  \code{n}:         \tab total sample size\cr
#'  \code{nmean}:     \tab MLE of normal mean\cr
#'  \code{nsd}:       \tab MLE of normal standard deviation\cr
#'  \code{ul}:        \tab lower threshold (fixed or MLE)\cr
#'  \code{sigmaul}:   \tab MLE of lower tail GPD scale (estimated from other parameters)\cr
#'  \code{xil}:       \tab MLE of lower tail GPD shape\cr
#'  \code{phiul}:     \tab MLE of lower tail fraction (bulk model or parameterised approach)\cr
#'  \code{se.phiul}:  \tab standard error of MLE of lower tail fraction\cr
#'  \code{ur}:        \tab upper threshold (fixed or MLE)\cr
#'  \code{sigmaur}:   \tab MLE of upper tail GPD scale (estimated from other parameters)\cr
#'  \code{xir}:       \tab MLE of upper tail GPD shape\cr
#'  \code{phiur}:     \tab MLE of upper tail fraction (bulk model or parameterised approach)\cr
#'  \code{se.phiur}:  \tab standard error of MLE of upper tail fraction\cr
#' }
#' 
#' @note When \code{pvector=NULL} then the initial values are:
#' \itemize{
#'  \item MLE of normal parameters assuming entire population is normal; and
#'  \item lower threshold 10\% quantile (not relevant for profile likelihood for threshold or fixed threshold approaches);
#'  \item upper threshold 90\% quantile (not relevant for profile likelihood for threshold or fixed threshold approaches);
#'  \item MLE of GPD shape parameters beyond threshold. 
#' }
#' 
#' @references
#' \url{http://www.math.canterbury.ac.nz/~c.scarrott/evmix}
#' 
#' \url{http://en.wikipedia.org/wiki/Normal_distribution}
#' 
#' \url{http://en.wikipedia.org/wiki/Generalized_Pareto_distribution}
#' 
#' Scarrott, C.J. and MacDonald, A. (2012). A review of extreme value
#' threshold estimation and uncertainty quantification. REVSTAT - Statistical
#' Journal 10(1), 33-59. Available from \url{http://www.ine.pt/revstat/pdf/rs120102.pdf}
#' 
#' Hu, Y. (2013). Extreme value mixture modelling: An R package and simulation study.
#' MSc (Hons) thesis, University of Canterbury, New Zealand.
#' \url{http://ir.canterbury.ac.nz/simple-search?query=extreme&submit=Go}
#' 
#' Zhao, X., Scarrott, C.J. Reale, M. and Oxley, L. (2010). Extreme value modelling
#' for forecasting the market crisis. Applied Financial Econometrics 20(1), 63-72.
#' 
#' Mendes, B. and H. F. Lopes (2004). Data driven estimates for mixtures. Computational
#' Statistics and Data Analysis 47(3), 583-598.
#' 
#' @author Yang Hu and Carl Scarrott \email{carl.scarrott@@canterbury.ac.nz}
#'
#' @section Acknowledgments: See Acknowledgments in
#'   \code{\link[evmix:fnormgpd]{fnormgpd}}, type \code{help fnormgpd}. Based on code
#' by Xin Zhao produced for MATLAB.
#' 
#' @seealso \code{\link[stats:Normal]{dnorm}},
#'  \code{\link[evmix:fgpd]{fgpd}} and \code{\link[evmix:gpd]{gpd}}
#'  
#' @aliases fgngcon lgngcon nlgngcon proflugngcon nlugngcon
#' @family  normgpd normgpdcon gng gngcon fnormgpd fnormgpdcon fgng fgngcon
#' 
#' @examples
#' \dontrun{
#' set.seed(1)
#' par(mfrow = c(2, 1))
#' 
#' x = rnorm(1000)
#' xx = seq(-4, 4, 0.01)
#' y = dnorm(xx)
#' 
#' # Continuity constraint
#' fit = fgngcon(x)
#' hist(x, breaks = 100, freq = FALSE, xlim = c(-4, 4))
#' lines(xx, y)
#' with(fit, lines(xx, dgngcon(xx, nmean, nsd, ul, xil, phiul,
#'    ur, xir, phiur), col="red"))
#' abline(v = c(fit$ul, fit$ur), col = "red")
#'   
#' # No continuity constraint
#' fit2 = fgng(x)
#' with(fit2, lines(xx, dgng(xx, nmean, nsd, ul, sigmaul, xil, phiul,
#'    ur, sigmaur, xir, phiur), col="blue"))
#' abline(v = c(fit2$ul, fit2$ur), col = "blue")
#' legend("topleft", c("True Density","No continuity constraint","With continuty constraint"),
#'   col=c("black", "blue", "red"), lty = 1)
#'   
#' # Profile likelihood for initial value of threshold and fixed threshold approach
#' fitu = fgngcon(x, ulseq = seq(-2, -0.2, length = 10), 
#'  urseq = seq(0.2, 2, length = 10))
#' fitfix = fgngcon(x, ulseq = seq(-2, -0.2, length = 10), 
#'  urseq = seq(0.2, 2, length = 10), fixedu = TRUE)
#' 
#' hist(x, breaks = 100, freq = FALSE, xlim = c(-4, 4))
#' lines(xx, y)
#' with(fit, lines(xx, dgngcon(xx, nmean, nsd, ul, xil, phiul,
#'    ur, xir, phiur), col="red"))
#' abline(v = c(fit$ul, fit$ur), col = "red")
#' with(fitu, lines(xx, dgngcon(xx, nmean, nsd, ul, xil, phiul,
#'    ur, xir, phiur), col="purple"))
#' abline(v = c(fitu$ul, fitu$ur), col = "purple")
#' with(fitfix, lines(xx, dgngcon(xx, nmean, nsd, ul, xil, phiul,
#'    ur, xir, phiur), col="darkgreen"))
#' abline(v = c(fitfix$ul, fitfix$ur), col = "darkgreen")
#' legend("topright", c("True Density","Default initial value (90% quantile)",
#'  "Prof. lik. for initial value", "Prof. lik. for fixed threshold"),
#'  col=c("black", "red", "purple", "darkgreen"), lty = 1)
#' }
#'   

# maximum likelihood fitting for normal bulk with GPD for
# both tails with continuity at thresholds
fgngcon <- function(x, phiul = TRUE, phiur = TRUE, ulseq = NULL, urseq = NULL, fixedu = FALSE, pvector = NULL,
  std.err = TRUE, method = "BFGS", control = list(maxit = 10000), finitelik = TRUE, ...) {

  call <- match.call()
    
  np = 6 # maximum number of parameters

  # Check properties of inputs
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.logic(phiul)
  check.logic(phiur)
  check.param(ulseq, allowvec = TRUE, allownull = TRUE)
  check.param(urseq, allowvec = TRUE, allownull = TRUE)
  check.logic(fixedu)
  check.logic(std.err)
  check.optim(method)
  check.control(control)
  check.logic(finitelik)

  if (any(!is.finite(x))) {
    warning("non-finite cases have been removed")
    x = x[is.finite(x)] # ignore missing and infinite cases
  }

  check.quant(x)
  n = length(x)

  if ((method == "L-BFGS-B") | (method == "BFGS")) finitelik = TRUE
  
  # useq must be specified if threshold is fixed
  if (fixedu & (is.null(ulseq) | is.null(urseq)))
    stop("for fixed threshold approach, ulseq and urseq must be specified (as scalar or vector)")
  
  # Check if profile likelihood or fixed threshold is being used
  # and determine initial values for parameters in each case
  if (is.null(ulseq) | is.null(ulseq)) { # not profile or fixed

    check.nparam(pvector, nparam = np, allownull = TRUE)
    
    if (is.null(pvector)) {
      pvector[1] = mean(x, trim = 0.2)
      pvector[2] = sd(x)
      pvector[3] = as.vector(quantile(x, 0.1))
      initfgpd = fgpd(-x, -pvector[3], std.err = FALSE)
      pvector[4] = initfgpd$xi
      pvector[5] = as.vector(quantile(x, 0.9))
      initfgpd = fgpd(x, pvector[5], std.err = FALSE)
      pvector[6] = initfgpd$xi
    }
    
  } else { # profile or fixed
    
    check.nparam(pvector, nparam = np - 2, allownull = TRUE)

    # profile likelihood for threshold or scalar given
    if ((length(ulseq) != 1) | (length(urseq) != 1)) {
      
      # remove thresholds with less than 5 excesses
      ulseq = ulseq[sapply(ulseq, FUN = function(u, x) sum(x < u) > 5, x = x)]
      check.param(ulseq, allowvec = TRUE)
      urseq = urseq[sapply(urseq, FUN = function(u, x) sum(x > u) > 5, x = x)]
      check.param(urseq, allowvec = TRUE)

      ulrseq = expand.grid(ulseq, urseq)
      
      # remove those where ulseq >= urseq
      if (any(ulrseq[1] >= ulrseq[2])) {
        warning("lower thresholds above or equal to upper threshold are ignored")
        ulrseq = ulrseq[ulrseq[1] < ulrseq[2],]
      }

      nllhu = apply(ulrseq, 1, proflugngcon, pvector = pvector, x = x,
        phiul = phiul, phiur = phiur, method = method, control = control, finitelik = finitelik, ...)
      
      if (all(!is.finite(nllhu))) stop("thresholds are all invalid")
      ul = ulrseq[which.min(nllhu), 1]
      ur = ulrseq[which.min(nllhu), 2]

    } else {
      if (ulseq >= urseq) stop("lower threshold cannot be above or equal to upper threshold")
      ul = ulseq
      ur = urseq
    }

    if (fixedu) { # threshold fixed
      if (is.null(pvector)) {
        pvector[1] = mean(x, trim = 0.2)
        pvector[2] = sd(x)
        initfgpd = fgpd(-x, -ul, std.err = FALSE)
        pvector[3] = initfgpd$xi
        initfgpd = fgpd(x, ur, std.err = FALSE)
        pvector[4] = initfgpd$xi
      }
    } else { # threshold as initial value in usual MLE
      if (is.null(pvector)) {
        pvector[1] = mean(x, trim = 0.2)
        pvector[2] = sd(x)
        pvector[3] = ul
        initfgpd = fgpd(-x, -pvector[3], std.err = FALSE)
        pvector[4] = initfgpd$xi
        pvector[5] = ur
        initfgpd = fgpd(x, pvector[5], std.err = FALSE)
        pvector[6] = initfgpd$xi
      } else {
        pvector[6] = pvector[4] # shift upper tail GPD shape to add in ur
        pvector[5] = ur
        pvector[4] = pvector[3] # shift lower tail GPD shape to add in ul
        pvector[3] = ul
      }
    }
  }

  if (fixedu) { # fixed threshold (separable) likelihood
    nllh = nlugngcon(pvector, ul, ur, x, phiul, phiur)
    
    # if initial parameter vector is invalid then try xi=0.1 (shape parameter is common problem)
    if (is.infinite(nllh)) {
      pvector[3] = 0.1
      pvector[4] = 0.1
      nllh = nlugngcon(pvector, ul, ur, x, phiul, phiur)
    }
    
    if (is.infinite(nllh)) stop("initial parameter values are invalid")
  
    fit = optim(par = as.vector(pvector), fn = nlugngcon, ul = ul, ur = ur, x = x,
      phiul = phiul, phiur = phiur, finitelik = finitelik, method = method, control = control, hessian = TRUE, ...)    
    
    nmean = fit$par[1]
    nsd = fit$par[2]
    xil = fit$par[3]
    xir = fit$par[4]
    
  } else { # complete (non-separable) likelihood
    
    nllh = nlgngcon(pvector, x, phiul, phiur)
    
    # if initial parameter vector is invalid then try xi=0.1 (shape parameter is common problem)
    if (is.infinite(nllh)) {
      pvector[4] = 0.1
      pvector[6] = 0.1
      nllh = nlgngcon(pvector, x, phiul, phiur)
    }

    if (is.infinite(nllh)) stop("initial parameter values are invalid")
  
    fit = optim(par = as.vector(pvector), fn = nlgngcon, x = x, phiul = phiul, phiur = phiur,
      finitelik = finitelik, method = method, control = control, hessian = TRUE, ...)    
    
    nmean = fit$par[1]
    nsd = fit$par[2]
    ul = fit$par[3]
    xil = fit$par[4]
    ur = fit$par[5]
    xir = fit$par[6]
  }

  conv = TRUE
  if ((fit$convergence != 0) | any(fit$par == pvector) | (abs(fit$value) >= 1e6)) {
    conv = FALSE
    warning("check convergence")
  }

  pul = pnorm(ul, nmean, nsd)
  if (phiul) {
    phiul = pul
    se.phiul = NA
  } else {
    phiul = mean(x < ul, na.rm = TRUE)
    se.phiul = sqrt(phiul * (1 - phiul) / n)
  }
  pur = pnorm(ur, nmean, nsd)
  if (phiur) {
    phiur = 1 - pur
    se.phiur = NA
  } else {
    phiur = mean(x > ur, na.rm = TRUE)
    se.phiur = sqrt(phiur * (1 - phiur) / n)
  }
  phib = (1 - phiul - phiur) / (pur - pul)
  
  dul = dnorm(ul, nmean, nsd)
  dur = dnorm(ur, nmean, nsd)
    
  sigmaul = phiul / (phib * dul)
  sigmaur = phiur / (phib * dur)
  
  if (std.err) {
    qrhess = qr(fit$hessian)
    if (qrhess$rank != ncol(qrhess$qr)) {
      warning("observed information matrix is singular")
      se = NULL
      invhess = NULL
    } else {
      invhess = solve(qrhess)
      vars = diag(invhess)
      if (any(vars <= 0)) {
        warning("observed information matrix is singular")
        invhess = NULL
        se = NULL
      } else {
        se = sqrt(vars)
      }  
    }
  } else {
    invhess = NULL
    se = NULL
  }
  
  if (!exists("nllhu")) nllhu = NULL

  list(call = call, x = as.vector(x),
    init = as.vector(pvector), fixedu = fixedu, ulseq = ulseq, urseq = urseq, nllhuseq = nllhu,
    optim = fit, conv = conv, cov = invhess, mle = fit$par, se = se, ratel = phiul, rater = phiur,
    nllh = fit$value, n = n, nmean = nmean, nsd = nsd,
    ul = ul, sigmaul = sigmaul, xil = xil, phiul = phiul, se.phiul = se.phiul, 
    ur = ur, sigmaur = sigmaur, xir = xir, phiur = phiur, se.phiur = se.phiur)
}

#' @export
#' @aliases fgngcon lgngcon nlgngcon proflugngcon nlugngcon
#' @rdname  fgngcon

# log-likelihood function for normal bulk with GPD for
# both tails with continuity at thresholds
lgngcon <- function(x, nmean = 0, nsd = 1,
  ul = 0, xil = 0, phiul = TRUE, ur = 0, xir = 0, phiur = TRUE, log = TRUE) {

  # Check properties of inputs
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.param(nmean)
  check.param(nsd)
  check.param(ul)                       
  check.param(xil)
  check.param(ur)                       
  check.param(xir)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.logic(log)
  
  if (any(!is.finite(x))) {
    warning("non-finite cases have been removed")
    x = x[is.finite(x)] # ignore missing and infinite cases
  }

  check.quant(x)
  n = length(x)
  
  check.inputn(c(length(nmean), length(nsd), 
    length(ul), length(xil), length(phiul), length(ur), length(xir), length(phiur)), allowscalar = TRUE)

  # assume NA or NaN are irrelevant as entire lower tail is now modelled
  # inconsistent with evd library definition
  # hence use which() to ignore these

  xur = x[which(x > ur)]
  nur = length(xur)
  xul = x[which(x < ul)]
  nul = length(xul)
  xb = x[which((x >= ul) & (x <= ur))]
  nb = length(xb)

  if ((nsd <= 0) | (ul <= min(x)) | (ul >= max(x)) | (ur <= min(x)) | (ur >= max(x))| (ur <= ul)) {
    l = -Inf
  } else {
    if (is.logical(phiul)) {
      pul = pnorm(ul, nmean, nsd)
      if (phiul) {
        phiul = pul
      } else {
        phiul = nul / n
      }
    }
    if (is.logical(phiur)) {
      pur = pnorm(ur, nmean, nsd)
      if (phiur) {
        phiur = 1 - pur
      } else {
        phiur = nur / n
      }
    }
    phib = (1 - phiul - phiur) / (pur - pul)

    dul = dnorm(ul, nmean, nsd)
    dur = dnorm(ur, nmean, nsd)
    
    sigmaul = phiul / (phib * dul)
    sigmaur = phiur / (phib * dur)
    
    syul = 1 + xil * (ul - xul) / sigmaul  
    syur = 1 + xir * (xur - ur) / sigmaur  
    yb = (xb - nmean) / nsd    # used for normal
  
    if ((min(syul) <= 0) | (phiul <= 0) | (phiul >= 1) | 
        (min(syur) <= 0) | (phiur <= 0) | (phiur >= 1) | ((phiul + phiur) > 1) |
        (pul <= 0) | (pul >= 1) | (pur <= 0) | (pur >= 1) |
        (phib < .Machine$double.eps) |
        (dul < .Machine$double.eps) | (dur < .Machine$double.eps) | 
        (sigmaul <= 0) | (sigmaur <= 0)) {
      l = -Inf
    } else { 
      l = lgpd(-xul, -ul, sigmaul, xil, phiul)
      l = l + lgpd(xur, ur, sigmaur, xir, phiur)
      l = l - nb * log(2 * pi * nsd ^ 2) / 2 - sum(yb ^ 2) / 2 + nb * log(phib)
    }
  }
  
  if (!log) l = exp(l)
  
  l
}

#' @export
#' @aliases fgngcon lgngcon nlgngcon proflugngcon nlugngcon
#' @rdname  fgngcon

# negative log-likelihood function for normal bulk with GPD for
# both tails with continuity at thresholds
# (wrapper for likelihood, inputs and checks designed for optimisation)
nlgngcon <- function(pvector, x, phiul = TRUE, phiur = TRUE, finitelik = FALSE) {

  np = 6 # maximum number of parameters

  # Check properties of inputs
  check.nparam(pvector, nparam = np)
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.logic(finitelik)

  nmean = pvector[1]
  nsd = pvector[2]
  ul = pvector[3]
  xil = pvector[4]
  ur = pvector[5]
  xir = pvector[6]

  nllh = -lgngcon(x, nmean, nsd, ul, xil, phiul, ur, xir, phiur) 
  
  if (finitelik & is.infinite(nllh)) {
    nllh = sign(nllh) * 1e6
  }

  nllh
}

#' @export
#' @aliases fgngcon lgngcon nlgngcon proflugngcon nlugngcon
#' @rdname  fgngcon

# profile negative log-likelihood function for given threshold for
# normal bulk with GPD for both tails with continuity at thresholds
# designed for apply to loop over vector of thresholds (hence c(ul, ur) vector is first input)
proflugngcon <- function(ulr, pvector, x, phiul = TRUE, phiur = TRUE,
  method = "BFGS", control = list(maxit = 10000), finitelik = TRUE, ...) {

  np = 6 # maximum number of parameters

  # Check properties of inputs
  check.nparam(pvector, nparam = np - 2, allownull = TRUE)
  check.param(ulr, allowvec = TRUE)
  check.nparam(ulr, nparam = 2)
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.optim(method)
  check.control(control)
  check.logic(finitelik)

  if (any(!is.finite(x))) {
    warning("non-finite cases have been removed")
    x = x[is.finite(x)] # ignore missing and infinite cases
  }

  check.quant(x)
  
  ul = ulr[1]
  ur = ulr[2]

  if (ul >= ur) stop("lower threshold cannot be above or equal to upper threshold")

  # check initial values for other parameters, try usual alternative
  if (!is.null(pvector)) {
    nllh = nlugngcon(pvector, ul, ur, x, phiul, phiur)
    
    if (is.infinite(nllh)) pvector = NULL
  }

  if (is.null(pvector)) {
    pvector[1] = mean(x, trim = 0.2)
    pvector[2] = sd(x)
    initfgpd = fgpd(-x, -ul, std.err = FALSE)
    pvector[3] = initfgpd$xi
    initfgpd = fgpd(x, ur, std.err = FALSE)
    pvector[4] = initfgpd$xi
    nllh = nlugngcon(pvector, ul, ur, x, phiul, phiur)
  }

  # if initial parameter vector is invalid then try xi=0.1 (shape parameter is common problem)
  if (is.infinite(nllh)) {
    pvector[3] = 0.1
    pvector[4] = 0.1
    nllh = nlugngcon(pvector, ul, ur, x, phiul, phiur)
  }

  # if still invalid then output cleanly
  if (is.infinite(nllh)) {
    warning(paste("initial parameter values for thresholds ul =", ul, "and ur =", ur,"are invalid"))
    fit = list(par = rep(NA, np), value = Inf, counts = 0, convergence = NA, 
      message = "initial values invalid", hessian = rep(NA, np))
  } else {

    fit = optim(par = as.vector(pvector), fn = nlugngcon, ul = ul, ur = ur, x = x, 
      phiul = phiul, phiur = phiur, finitelik = finitelik, method = method, control = control, hessian = TRUE, ...)
  }
    
  if (finitelik & is.infinite(fit$value)) {
    fit$value = sign(fit$value) * 1e6
  }

  fit$value
}

#' @export
#' @aliases fgngcon lgngcon nlgngcon proflugngcon nlugngcon
#' @rdname  fgngcon

# negative log-likelihood function for normal bulk with GPD for
# both tails with continuity at thresholds
# (wrapper for likelihood, designed for threshold to be fixed and other parameters optimised)
nlugngcon <- function(pvector, ul, ur, x, phiul = TRUE, phiur = TRUE, finitelik = FALSE) {

  np = 6 # maximum number of parameters

  # Check properties of inputs
  check.nparam(pvector, nparam = np - 2)
  check.param(ul)
  check.param(ur)
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.logic(finitelik)
    
  if (ul >= ur) stop("lower threshold cannot be above or equal to upper threshold")

  nmean = pvector[1]
  nsd = pvector[2]
  xil = pvector[3]
  xir = pvector[4]

  nllh = -lgngcon(x, nmean, nsd, ul, xil, phiul, ur, xir, phiur)
  
  if (finitelik & is.infinite(nllh)) {
    nllh = sign(nllh) * 1e6
  }

  nllh
}
