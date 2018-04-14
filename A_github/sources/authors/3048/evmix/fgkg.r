#' @export
#' 
#' @title MLE Fitting of Kernel Density Estimate for Bulk and GPD for Both Tails
#'  Extreme Value Mixture Model
#'
#' @description Maximum likelihood estimation for fitting the extreme value 
#' mixture model with kernel density estimate for bulk distribution between thresholds and conditional
#' GPDs beyond thresholds. With options for profile likelihood estimation for both thresholds and
#' fixed threshold approach.
#'
#' @inheritParams fgng
#' @inheritParams fkdengpd
#' @inheritParams fkden
#' @inheritParams kden
#' @inheritParams fnormgpd
#' @inheritParams fgpd
#' 
#' @details The extreme value mixture model with kernel density estimate for bulk and
#' GPD for both tails is 
#' fitted to the entire dataset using maximum likelihood estimation. The estimated
#' parameters, variance-covariance matrix and their standard errors are automatically
#' output.
#' 
#' See help for \code{\link[evmix:fnormgpd]{fnormgpd}} and \code{\link[evmix:fgkg]{fgkg}} 
#' for details, type \code{help fnormgpd} and \code{help fgkg}. 
#' Only the different features are outlined below for brevity.
#' 
#' The full parameter vector is
#' (\code{lambda}, \code{ul}, \code{sigmaul}, \code{xil}, \code{ur}, \code{sigmaur}, \code{xir})
#' if thresholds are also estimated and
#' (\code{lambda}, \code{sigmaul}, \code{xil}, \code{sigmaur}, \code{xir})
#' for profile likelihood or fixed threshold approach.
#' 
#' Cross-validation likelihood is used for KDE, but standard likelihood is used
#' for GPD components. See help for \code{\link[evmix:fkden]{fkden}} for details,
#' type \code{help fkden}.
#' 
#' The alternate bandwidth definitions are discussed in the 
#' \code{\link[evmix:kernels]{kernels}}, with the \code{lambda} as the default
#' used in the likelihood fitting. The \code{bw} specification is the same as
#' used in the \code{\link[stats:density]{density}} function.
#' 
#' The possible kernels are also defined in \code{\link[evmix:kernels]{kernels}}
#' with the \code{"gaussian"} as the default choice.
#' 
#' The tail fractions \code{phiul} and \code{phiur} are treated separately to the other parameters, 
#' to allow for all their representations. In the fitting functions 
#' \code{\link[evmix:fgkg]{fgkg}} and
#' \code{\link[evmix:fgkg]{proflugkg}} they are logical:
#' \itemize{
#'  \item default values \code{phiul=TRUE} and \code{phiur=TRUE} - tail fractions specified by 
#'    KDE distribution and survivior functions respectively and
#'    standard error is output as \code{NA}.
#'  \item \code{phiul=FALSE} and \code{phiur=FALSE} - treated as extra parameters estimated using
#'    the MLE which is the sample proportion beyond the thresholds and 
#'    standard error is output.
#' }
#' In the likelihood functions \code{\link[evmix:fgkg]{lgkg}},
#' \code{\link[evmix:fgkg]{nlgkg}} and \code{\link[evmix:fgkg]{nlugkg}} 
#' it can be logical or numeric:
#' \itemize{
#'  \item logical - same as for fitting functions with default values \code{phiul=TRUE} and \code{phiur=TRUE}.
#'  \item numeric - any value over range \eqn{(0, 1)}. Notice that the tail
#'    fraction probability cannot be 0 or 1 otherwise there would be no
#'    contribution from either tail or bulk components respectively. Also,
#'    \code{phiul+phiur<1} as bulk must contribute.
#' }
#' 
#' If the profile likelihood approach is used, then a grid search over all combinations of both thresholds
#' is carried out. The combinations which lead to less than 5 in any datapoints beyond the thresholds are not considered.
#' 
#' @section Warning:
#' See important warnings about cross-validation likelihood estimation in 
#' \code{\link[evmix:fkden]{fkden}}, type \code{help fkden}.
#' 
#' @return Log-likelihood is given by \code{\link[evmix:fgkg]{lgkg}} and it's
#'   wrappers for negative log-likelihood from \code{\link[evmix:fgkg]{nlgkg}}
#'   and \code{\link[evmix:fgkg]{nlugkg}}. Profile likelihood for both
#'   thresholds given by \code{\link[evmix:fgkg]{proflugkg}}. Fitting function
#'   \code{\link[evmix:fgkg]{fgkg}} returns a simple list with the
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
#'  \code{lambda}:    \tab MLE of lambda (kernel half-width)\cr
#'  \code{ul}:        \tab lower threshold (fixed or MLE)\cr
#'  \code{sigmaul}:   \tab MLE of lower tail GPD scale\cr
#'  \code{xil}:       \tab MLE of lower tail GPD shape\cr
#'  \code{phiul}:     \tab MLE of lower tail fraction (bulk model or parameterised approach)\cr
#'  \code{se.phiul}:  \tab standard error of MLE of lower tail fraction\cr
#'  \code{ur}:        \tab upper threshold (fixed or MLE)\cr
#'  \code{sigmaur}:   \tab MLE of upper tail GPD scale\cr
#'  \code{xir}:       \tab MLE of upper tail GPD shape\cr
#'  \code{phiur}:     \tab MLE of upper tail fraction (bulk model or parameterised approach)\cr
#'  \code{se.phiur}:  \tab standard error of MLE of upper tail fraction\cr
#'  \code{bw}:        \tab MLE of bw (kernel standard deviations)\cr
#'  \code{kernel}:    \tab kernel name\cr
#' }
#' 
#' @note The data and kernel centres are both vectors. Infinite and missing sample values
#' (and kernel centres) are dropped.
#' 
#' When \code{pvector=NULL} then the initial values are:
#' \itemize{
#'  \item normal reference rule for bandwidth, using the \code{\link[stats:bandwidth]{bw.nrd0}} function, which is
#'    consistent with the \code{\link[stats:density]{density}} function. At least two kernel
#'    centres must be provided as the variance needs to be estimated.
#'  \item lower threshold 10\% quantile (not relevant for profile likelihood for threshold or fixed threshold approaches);
#'  \item upper threshold 90\% quantile (not relevant for profile likelihood for threshold or fixed threshold approaches);
#'  \item MLE of GPD parameters beyond thresholds. 
#' }
#' 
#' @references
#' \url{http://www.math.canterbury.ac.nz/~c.scarrott/evmix}
#' 
#' \url{http://en.wikipedia.org/wiki/Kernel_density_estimation}
#' 
#' \url{http://en.wikipedia.org/wiki/Cross-validation_(statistics)}
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
#' Bowman, A.W. (1984). An alternative method of cross-validation for the smoothing of
#' density estimates. Biometrika 71(2), 353-360.
#' 
#' Duin, R.P.W. (1976). On the choice of smoothing parameters for Parzen estimators of
#' probability density functions. IEEE Transactions on Computers C25(11), 1175-1179.
#' 
#' MacDonald, A., Scarrott, C.J., Lee, D., Darlow, B., Reale, M. and Russell, G. (2011).
#' A flexible extreme value mixture model. Computational Statistics and Data Analysis
#' 55(6), 2137-2157.
#' 
#' Wand, M. and Jones, M.C. (1995). Kernel Smoothing. Chapman && Hall.
#' 
#' @author Yang Hu and Carl Scarrott \email{carl.scarrott@@canterbury.ac.nz}
#'
#' @section Acknowledgments: See Acknowledgments in
#'   \code{\link[evmix:fnormgpd]{fnormgpd}}, type \code{help fnormgpd}. Based on code
#' by Anna MacDonald produced for MATLAB.
#' 
#' @seealso \code{\link[evmix:kernels]{kernels}}, \code{\link[evmix:kfun]{kfun}},
#'  \code{\link[stats:density]{density}}, \code{\link[stats:bandwidth]{bw.nrd0}}
#' and \code{\link[ks:kde]{dkde}} in \code{\link[ks:kde]{ks}} package.
#'  \code{\link[evmix:fgpd]{fgpd}} and \code{\link[evmix:gpd]{gpd}}.
#'  
#' @aliases fgkg lgkg nlgkg proflugkg nlugkg
#' @family  kdengpd kdengpdcon fkdengpd fkdengpdcon normgpd fnormgpd gkg gkgcon fgkg fgkgcon
#'          kden bckden bckdengpd bckdengpdcon fkden fbckden fbckdengpd fbckdengpdcon
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
#' # Bulk model based tail fraction
#' fit = fgkg(x)
#' hist(x, breaks = 100, freq = FALSE, xlim = c(-4, 4))
#' lines(xx, y)
#' with(fit, lines(xx, dgkg(xx, x, lambda, ul, sigmaul, xil, phiul,
#'    ur, sigmaur, xir, phiur), col="red"))
#' abline(v = c(fit$ul, fit$ur), col = "red")
#'   
#' # Parameterised tail fraction
#' fit2 = fgkg(x, phiul = FALSE, phiur = FALSE)
#' with(fit2, lines(xx, dgkg(xx, x, lambda, ul, sigmaul, xil, phiul,
#'    ur, sigmaur, xir, phiur), col="blue"))
#' abline(v = c(fit2$ul, fit2$ur), col = "blue")
#' legend("topright", c("True Density","Bulk Tail Fraction","Parameterised Tail Fraction"),
#'   col=c("black", "red", "blue"), lty = 1)
#'   
#' # Profile likelihood for initial value of threshold and fixed threshold approach
#' fitu = fgkg(x, ulseq = seq(-2, -0.2, length = 10), 
#'  urseq = seq(0.2, 2, length = 10))
#' fitfix = fgkg(x, ulseq = seq(-2, -0.2, length = 10), 
#'  urseq = seq(0.2, 2, length = 10), fixedu = TRUE)
#' 
#' hist(x, breaks = 100, freq = FALSE, xlim = c(-4, 4))
#' lines(xx, y)
#' with(fit, lines(xx, dgkg(xx, x, lambda, ul, sigmaul, xil, phiul,
#'    ur, sigmaur, xir, phiur), col="red"))
#' abline(v = c(fit$ul, fit$ur), col = "red")
#' with(fitu, lines(xx, dgkg(xx, x, lambda, ul, sigmaul, xil, phiul,
#'    ur, sigmaur, xir, phiur), col="purple"))
#' abline(v = c(fitu$ul, fitu$ur), col = "purple")
#' with(fitfix, lines(xx, dgkg(xx, x, lambda, ul, sigmaul, xil, phiul,
#'    ur, sigmaur, xir, phiur), col="darkgreen"))
#' abline(v = c(fitfix$ul, fitfix$ur), col = "darkgreen")
#' legend("topright", c("True Density","Default initial value (90% quantile)",
#'  "Prof. lik. for initial value", "Prof. lik. for fixed threshold"),
#'  col=c("black", "red", "purple", "darkgreen"), lty = 1)
#' }
#'   

# maximum likelihood fitting for kernel density estimate for bulk with GPD for both tails
fgkg <- function(x, phiul = TRUE, phiur = TRUE, ulseq = NULL, urseq = NULL, fixedu = FALSE,
  pvector = NULL, kernel = "gaussian", add.jitter = FALSE, factor = 0.1, amount = NULL,
  std.err = TRUE, method = "BFGS", control = list(maxit = 10000), finitelik = TRUE, ...) {

  call <- match.call()
    
  np = 7 # maximum number of parameters

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

  check.kernel(kernel)
  check.posparam(factor)
  check.posparam(amount, allownull = TRUE)
  check.logic(add.jitter)
  
  if (any(!is.finite(x))) {
    warning("non-finite cases have been removed")
    x = x[is.finite(x)] # ignore missing and infinite cases
  }

  check.quant(x)
  n = length(x)

  if (add.jitter) x = jitter(x, factor, amount)

  xuniq = unique(x)
  if (length(xuniq) < (0.95*n))
    warning("data may be rounded, as more than 5% are ties, so bandwidth could be biased to zero")

  if ((method == "L-BFGS-B") | (method == "BFGS")) finitelik = TRUE
  
  # useq must be specified if threshold is fixed
  if (fixedu & (is.null(ulseq) | is.null(urseq)))
    stop("for fixed threshold approach, ulseq and urseq must be specified (as scalar or vector)")
  
  # Check if profile likelihood or fixed threshold is being used
  # and determine initial values for parameters in each case
  if (is.null(ulseq) | is.null(ulseq)) { # not profile or fixed

    check.nparam(pvector, nparam = np, allownull = TRUE)
    
    if (is.null(pvector)) {
      if (n == 1) {
        stop("Automated bandwidth estimation requires 2 or more kernel centres")
      } else if (n < 10) {
        stop("Automated bandwidth estimation unreliable with less than 10 kernel centres")
      }
      pvector[1] = klambda(bw.nrd0(x), kernel)
      pvector[2] = as.vector(quantile(x, 0.1))
      initfgpd = fgpd(-x, -pvector[2], std.err = FALSE)
      pvector[3] = initfgpd$sigmau
      pvector[4] = initfgpd$xi
      pvector[5] = as.vector(quantile(x, 0.9))
      initfgpd = fgpd(x, pvector[5], std.err = FALSE)
      pvector[6] = initfgpd$sigmau
      pvector[7] = initfgpd$xi
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
      
      nllhu = apply(ulrseq, 1, proflugkg, pvector = pvector, x = x,
        phiul = phiul, phiur = phiur, kernel = kernel,
        method = method, control = control, finitelik = finitelik, ...)
      
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
        if (n == 1) {
          stop("Automated bandwidth estimation requires 2 or more kernel centres")
        } else if (n < 10) {
          stop("Automated bandwidth estimation unreliable with less than 10 kernel centres")
        }
        pvector[1] = klambda(bw.nrd0(x), kernel)
        initfgpd = fgpd(-x, -ul, std.err = FALSE)
        pvector[2] = initfgpd$sigmau
        pvector[3] = initfgpd$xi
        initfgpd = fgpd(x, ur, std.err = FALSE)
        pvector[4] = initfgpd$sigmau
        pvector[5] = initfgpd$xi
      }
    } else { # threshold as initial value in usual MLE
      if (is.null(pvector)) {
        if (n == 1) {
          stop("Automated bandwidth estimation requires 2 or more kernel centres")
        } else if (n < 10) {
          stop("Automated bandwidth estimation unreliable with less than 10 kernel centres")
        }
        pvector[1] = klambda(bw.nrd0(x), kernel)
        pvector[2] = ul
        initfgpd = fgpd(-x, -pvector[2], std.err = FALSE)
        pvector[3] = initfgpd$sigmau
        pvector[4] = initfgpd$xi
        pvector[5] = ur
        initfgpd = fgpd(x, pvector[5], std.err = FALSE)
        pvector[6] = initfgpd$sigmau
        pvector[7] = initfgpd$xi
      } else {
        pvector[7] = pvector[5] # shift upper tail GPD scale and shape to add in ur
        pvector[6] = pvector[4]
        pvector[5] = ur
        pvector[4] = pvector[3] # shift lower tail GPD scale and shape to add in ul
        pvector[3] = pvector[2]
        pvector[2] = ul
      }
    }
  }

  if (fixedu) { # fixed threshold (separable) likelihood
    nllh = nlugkg(pvector, ul, ur, x, phiul, phiur, kernel = kernel)
    if (is.infinite(nllh)) {
      pvector[c(3, 5)] = 0.1
      nllh = nlugkg(pvector, ul, ur, x, phiul, phiur, kernel = kernel)
    }
    if (is.infinite(nllh)) stop("initial parameter values are invalid")
  
    fit = optim(par = as.vector(pvector), fn = nlugkg, ul = ul, ur = ur, x = x,
      phiul = phiul, phiur = phiur, kernel = kernel,
      finitelik = finitelik, method = method, control = control, hessian = TRUE, ...)    
    
    lambda = fit$par[1]
    sigmaul = fit$par[2]
    xil = fit$par[3]
    sigmaur = fit$par[4]
    xir = fit$par[5]
    
  } else { # complete (non-separable) likelihood
    
    nllh = nlgkg(pvector, x, phiul, phiur, kernel = kernel)
    if (is.infinite(nllh)) {
      pvector[c(4, 7)] = 0.1
      nllh = nlgkg(pvector, x, phiul, phiur, kernel = kernel)
    }
    if (is.infinite(nllh)) stop("initial parameter values are invalid")
  
    fit = optim(par = as.vector(pvector), fn = nlgkg, x = x, phiul = phiul, phiur = phiur, kernel = kernel,
      finitelik = finitelik, method = method, control = control, hessian = TRUE, ...)    
    
    lambda = fit$par[1]
    ul = fit$par[2]
    sigmaul = fit$par[3]
    xil = fit$par[4]
    ur = fit$par[5]
    sigmaur = fit$par[6]
    xir = fit$par[7]
  }

  bw = kbw(fit$par[1], kernel)

  conv = TRUE
  if ((fit$convergence != 0) | any(fit$par == pvector) | (abs(fit$value) >= 1e6)) {
    conv = FALSE
    warning("check convergence")
  }

  pul = pkdenx(ul, x, lambda, kernel)
  if (phiul) {
    phiul = pul
    se.phiul = NA
  } else {
    phiul = mean(x < ul, na.rm = TRUE)
    se.phiul = sqrt(phiul * (1 - phiul) / n)
  }
  pur = pkdenx(ur, x, lambda, kernel)
  if (phiur) {
    phiur = 1 - pur
    se.phiur = NA
  } else {
    phiur = mean(x > ur, na.rm = TRUE)
    se.phiur = sqrt(phiur * (1 - phiur) / n)
  }
  
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
    nllh = fit$value, n = n, lambda = lambda, 
    ul = ul, sigmaul = sigmaul, xil = xil, phiul = phiul, se.phiul = se.phiul, 
    ur = ur, sigmaur = sigmaur, xir = xir, phiur = phiur, se.phiur = se.phiur, bw = bw, kernel = kernel)
}

#' @export
#' @aliases fgkg lgkg nlgkg proflugkg nlugkg
#' @rdname  fgkg

# log-likelihood function for kernel density estimate for bulk with GPD for both tails
# cross-validation for KDE component
lgkg <- function(x, lambda = NULL,
  ul = 0, sigmaul = 1, xil = 0, phiul = TRUE,
  ur = 0, sigmaur = 1, xir = 0, phiur = TRUE, bw = NULL, kernel = "gaussian", log = TRUE) {

  # Check properties of inputs
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.param(lambda, allownull = TRUE)
  check.param(bw, allownull = TRUE)
  check.param(ul)                       
  check.param(sigmaul)
  check.param(xil)
  check.param(ur)                       
  check.param(sigmaur)
  check.param(xir)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.logic(log)

  check.kernel(kernel)

  kernel = ifelse(kernel == "rectangular", "uniform", kernel)
  kernel = ifelse(kernel == "normal", "gaussian", kernel)
  
  if (any(!is.finite(x))) {
    warning("non-finite cases have been removed")
    x = x[is.finite(x)] # ignore missing and infinite cases
  }

  check.quant(x)
  n = length(x)

  if (is.null(lambda) & is.null(bw)) {
    if (n == 1) {
      stop("Automated bandwidth estimation requires 2 or more kernel centres")
    } else if (n < 10) {
      warning("Automated bandwidth estimation unreliable with less than 10 kernel centres")
    }
    bw = bw.nrd0(x)
  }

  if (is.null(lambda)) lambda = klambda(bw, kernel, lambda)
  
  check.inputn(c(length(lambda), length(ul), length(sigmaul), length(xil), length(phiul), 
    length(ur), length(sigmaur), length(xir), length(phiur)), allowscalar = TRUE)

  # assume NA or NaN are irrelevant as entire lower tail is now modelled
  # inconsistent with evd library definition
  # hence use which() to ignore these

  xur = x[which(x > ur)]
  nur = length(xur)
  xul = x[which(x < ul)]
  nul = length(xul)
  xb = x[which((x >= ul) & (x <= ur))]
  nb = length(xb)

  if ((lambda <= 0) | (sigmaul <= 0) | (ul <= min(x)) | (ul >= max(x))
    | (sigmaur <= 0) | (ur <= min(x)) | (ur >= max(x))| (ur <= ul)) {
    l = -Inf
  } else {
    if (is.logical(phiul)) {
      pul = pkdenx(ul, x, lambda, kernel)
      if (phiul) {
        phiul = pul
      } else {
        phiul = nul / n
      }
    }
    if (is.logical(phiur)) {
      pur = pkdenx(ur, x, lambda, kernel)
      if (phiur) {
        phiur = 1 - pur
      } else {
        phiur = nur / n
      }
    }
    phib = (1 - phiul - phiur) / (pur - pul)
  
    syul = 1 + xil * (ul - xul) / sigmaul  
    syur = 1 + xir * (xur - ur) / sigmaur  
  
    if ((min(syul) <= 0) | (phiul <= 0) | (phiul >= 1) | 
        (min(syur) <= 0) | (phiur <= 0) | (phiur >= 1) | ((phiul + phiur) > 1) |
        (pul <= 0) | (pul >= 1) | (pur <= 0) | (pur >= 1) |
        (phib < .Machine$double.eps)) {
      l = -Inf
    } else { 
      l = lgpd(-xul, -ul, sigmaul, xil, phiul)
      l = l + lgpd(xur, ur, sigmaur, xir, phiur)
      l = l + lkden(xb, lambda, kernel = kernel, extracentres = c(xul, xur)) + nb*log(phib)
    }
  }
  
  if (!log) l = exp(l)
  
  l
}

#' @export
#' @aliases fgkg lgkg nlgkg proflugkg nlugkg
#' @rdname  fgkg

# negative log-likelihood function for kernel density estimate for bulk with GPD for both tails
# cross-validation for KDE component
# (wrapper for likelihood, inputs and checks designed for optimisation)
nlgkg <- function(pvector, x, phiul = TRUE, phiur = TRUE, kernel = "gaussian", finitelik = FALSE) {

  np = 7 # maximum number of parameters

  # Check properties of inputs
  check.nparam(pvector, nparam = np)
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.logic(finitelik)

  check.kernel(kernel)
  kernel = ifelse(kernel == "rectangular", "uniform", kernel)
  kernel = ifelse(kernel == "normal", "gaussian", kernel)

  lambda = pvector[1]
  ul = pvector[2]
  sigmaul = pvector[3]
  xil = pvector[4]
  ur = pvector[5]
  sigmaur = pvector[6]
  xir = pvector[7]

  nllh = -lgkg(x, lambda, ul, sigmaul, xil, phiul, ur, sigmaur, xir, phiur, kernel = kernel) 
  
  if (finitelik & is.infinite(nllh)) {
    nllh = sign(nllh) * 1e6
  }

  nllh
}

#' @export
#' @aliases fgkg lgkg nlgkg proflugkg nlugkg
#' @rdname  fgkg

# profile negative log-likelihood function for given threshold for
# kernel density estimate for bulk with GPD for both tails
# designed for apply to loop over vector of thresholds (hence c(ul, ur) vector is first input)
# cross-validation for KDE component
proflugkg <- function(ulr, pvector, x, phiul = TRUE, phiur = TRUE, kernel = "gaussian",
  method = "BFGS", control = list(maxit = 10000), finitelik = TRUE, ...) {

  np = 7 # maximum number of parameters

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

  check.kernel(kernel)

  kernel = ifelse(kernel == "rectangular", "uniform", kernel)
  kernel = ifelse(kernel == "normal", "gaussian", kernel)

  if (any(!is.finite(x))) {
    warning("non-finite cases have been removed")
    x = x[is.finite(x)] # ignore missing and infinite cases
  }

  check.quant(x)
  n = length(x)
  
  ul = ulr[1]
  ur = ulr[2]

  if (ul >= ur) stop("lower threshold cannot be above or equal to upper threshold")

  # check initial values for other parameters, try usual alternative
  if (!is.null(pvector)) {
    nllh = nlugkg(pvector, ul, ur, x, phiul, phiur, kernel = kernel)
    
    if (is.infinite(nllh)) pvector = NULL
  }

  if (is.null(pvector)) {
    if (n == 1) {
      stop("Automated bandwidth estimation requires 2 or more kernel centres")
    } else if (n < 10) {
        stop("Automated bandwidth estimation unreliable with less than 10 kernel centres")
    }
    pvector[1] = klambda(bw.nrd0(x), kernel)
    initfgpd = fgpd(-x, -ul, std.err = FALSE)
    pvector[2] = initfgpd$sigmau
    pvector[3] = initfgpd$xi
    initfgpd = fgpd(x, ur, std.err = FALSE)
    pvector[4] = initfgpd$sigmau
    pvector[5] = initfgpd$xi
    nllh = nlugkg(pvector, ul, ur, x, phiul, phiur, kernel = kernel)
  }

  if (is.infinite(nllh)) {
    pvector[c(3, 5)] = 0.1
    nllh = nlugkg(pvector, ul, ur, x, phiul, phiur, kernel = kernel)
  }

  # if still invalid then output cleanly
  if (is.infinite(nllh)) {
    warning(paste("initial parameter values for thresholds ul =", ul, "and ur =", ur,"are invalid"))
    fit = list(par = rep(NA, np), value = Inf, counts = 0, convergence = NA, 
      message = "initial values invalid", hessian = rep(NA, np))
  } else {

    fit = optim(par = as.vector(pvector), fn = nlugkg, ul = ul, ur = ur, x = x, 
      phiul = phiul, phiur = phiur, kernel = kernel,
      finitelik = finitelik, method = method, control = control, hessian = TRUE, ...)
  }
    
  if (finitelik & is.infinite(fit$value)) {
    fit$value = sign(fit$value) * 1e6
  }

  fit$value
}

#' @export
#' @aliases fgkg lgkg nlgkg proflugkg nlugkg
#' @rdname  fgkg

# negative log-likelihood function for kernel density estimate for bulk with GPD for both tails
# (wrapper for likelihood, designed for threshold to be fixed and other parameters optimised)
# cross-validation for KDE component
nlugkg <- function(pvector, ul, ur, x, phiul = TRUE, phiur = TRUE, kernel = "gaussian", finitelik = FALSE) {

  np = 7 # maximum number of parameters

  # Check properties of inputs
  check.nparam(pvector, nparam = np - 2)
  check.param(ul)
  check.param(ur)
  check.quant(x, allowna = TRUE, allowinf = TRUE)
  check.phiu(phiul, allowfalse = TRUE)
  check.phiu(phiur, allowfalse = TRUE)
  check.logic(finitelik)

  check.kernel(kernel)
  kernel = ifelse(kernel == "rectangular", "uniform", kernel)
  kernel = ifelse(kernel == "normal", "gaussian", kernel)
    
  if (ul >= ur) stop("lower threshold cannot be above or equal to upper threshold")

  lambda = pvector[1]
  sigmaul = pvector[2]
  xil = pvector[3]
  sigmaur = pvector[4]
  xir = pvector[5]

  nllh = -lgkg(x, lambda, ul, sigmaul, xil, phiul, ur, sigmaur, xir, phiur, kernel = kernel) 
  
  if (finitelik & is.infinite(nllh)) {
    nllh = sign(nllh) * 1e6
  }

  nllh
}
