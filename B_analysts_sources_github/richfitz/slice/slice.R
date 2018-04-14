mcmc <- function(target, x_init, nsteps, w,
                 lower=-Inf, upper=Inf, print_every=1) {
  npar <- length(x_init)
  hist_pars <- matrix(NA, ncol=npar, nrow=nsteps)
  hist_prob <- rep(NA, nsteps)

  colnames(hist_pars) <- names(x_init)

  lower <- recycle(lower, npar)
  upper <- recycle(upper, npar)
  w     <- recycle(w,     npar)

  check_bounds(lower, upper, x_init)

  y_init <- target(x_init)
  if (!is.finite(y_init)) {
    stop("Starting point must have finite probability")
  }

  we_should_print <- make_every_so_often(print_every)

  for (i in seq_len(nsteps)) {
    tmp <- sampler_slice(target, x_init, y_init, w, lower, upper)
    x_init <- hist_pars[i,] <- tmp[[1]]
    y_init <- hist_prob[i]  <- tmp[[2]]

    if (we_should_print()) {
      message(sprintf("%d: {%s} -> %2.5f", i,
                      paste(sprintf("%2.4f", x_init), collapse=", "),
                      y_init))
    }
  }

  data.frame(i=seq_along(hist_prob), hist_pars, hist_prob)
}

sampler_slice <- function(lik, x_init, y_init, w, lower, upper) {
  for (i in seq_along(x_init)) {
    xy <- slice_1d(make_unipar(lik, x_init, i),
                   x_init[i], y_init, w[i], lower[i], upper[i])
    x_init[i] <- xy[1]
    y_init    <- xy[2]
  }

  list(x_init, y_init)
}

## Here, w, lower and upper are scalars
slice_1d <- function(f, x_init, y_init, w, lower, upper) {
  z <- y_init - rexp(1)
  r <- slice_isolate(f, x_init, y_init, z, w, lower, upper)
  slice_sample(f, x_init, z, r)
}

slice_isolate <- function(f, x_init, y_init, z, w, lower, upper) {
  u <- runif(1) * w
  L <- x_init - u
  R <- x_init + (w-u)

  while (L > lower && f(L) > z) {
    L <- L - w
  }
  while (R < upper && f(R) > z) {
    R <- R + w
  }

  c(max(L, lower), min(R, upper))
}

slice_sample <- function(f, x_init, z, r) {
  r0 <- r[1]
  r1 <- r[2]

  repeat {
    xs <- runif(1, r0, r1)
    ys <- f(xs)
    if (ys > z) {
      break
    }
    if (xs < x_init) {
      r0 <- xs
    } else {
      r1 <- xs
    }
  }
  c(xs, ys)
}

## utilities:
make_unipar <- function(f, x, i) {
  force(f)
  force(x)
  force(i)
  function(z) {
    x[i] <- z
    f(x)
  }
}

recycle <- function(x, length, name=deparse(substitute(x))) {
  if (length(x) == 1) {
    rep(x, length)
  } else if (length(x) == length) {
    x
  } else {
    stop(sprintf("'%s' of incorrect length", name))
  }
}

check_bounds <- function(lower, upper, x0=NULL) {
  if (!is.null(x0) && (any(x0 < lower) || any(x0 > upper))) {
    stop("Starting parameter falls outside of problems bounds")
  }
  if (any(lower >= upper)) {
    stop("'upper' must be strictly greater than 'lower'")
  }
}

make_every_so_often <- function(iterations=1) {
  if (iterations == 1L) {
    function() {
      TRUE
    }
  } else if (iterations > 0) {
    i <- 0L # counter, will be updated
    iterations <- as.integer(iterations)
    function() {
      i <<- i + 1L
      i %% iterations == 0L
    }
  } else {
    function() {
      FALSE
    }
  }
}
