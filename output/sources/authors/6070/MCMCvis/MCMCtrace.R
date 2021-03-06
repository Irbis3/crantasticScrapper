#' Trace and density plots from MCMC output
#'
#' Trace and density plots of MCMC chains for specific parameters of interest. Print plots to pdf by default.
#'
#' @param object Object containing MCMC output. See DETAILS below.
#' @param params Character string (or vector of character strings) denoting parameters of interest.
#'
#' Default \code{'all'} returns chains for all parameters.
#'
#' @param excl Character string (or vector of character strings) denoting parameters to exclude. Used in conjunction with \code{params} argument to select parameters of interest.
#'
#' @param ISB Ignore Square Brackets (ISB). Logical specifying whether square brackets should be ignored in the \code{params} and \code{excl} arguments. If \code{TRUE}, square brackets are ignored - input from \code{params} and \code{excl} are otherwise matched exactly. If \code{FALSE}, square brackets are not ignored - input from \code{params} and \code{excl} are matched using grep, which can take arguments in regular expression format. This allows partial names to be used when specifying parameters of interest.
#'
#' @param iter Number of iterations to plot for trace and density plots. The default value is 5000, meaning the last 5000 iterations of the chain will be plotted.
#'
#' @param gvals Vector containing generating values if simulated data was used to fit model. These values will be plotted as vertical lines on the density plots to compare posterior distributions with the true parameter values used to generate the data. No line will be apparent if the generating value is outside the plotted range of the posterior distribution.
#'
#' @param priors Matrix containing random draws from prior distributions corresponding to parameters of interest. If specified, priors are plotted along with posterior density plots. Percent overlap between prior and posterior is also calculated and displayed on each plot. Each column of the matrix represents a prior for a different parameter. Parameters are plotted alphabetically - priors should be sorted accordingly. If \code{priors} contains only one prior and more than one parameter is specified for the \code{params} argument, this prior will be used for all parameters. The number of draws for each prior should equal the number of iterations specified by \code{iter} (or total draws if less than \code{iter}) times the number of chains, though the function will automatically adjust if more or fewer iterations are specified. See DETAILS below.
#'
#' @param pdf Logical - if \code{pdf = TRUE} plots will be exported to a pdf.
#' @param open_pdf Logical - if \code{open_pdf = TRUE} pdf will open in viewer after being generated.
#' @param filename Name of pdf file to be printed. Default is 'MCMCtrace'.
#' @param wd Working directory for pdf output. Default is current directory.
#' @param type Type of plot to be output. \code{'both'} outputs both trace and density plots, \code{'trace'}
#' outputs only trace plots, and \code{'density'} outputs only density plots.
#' @param ind Logical - if \code{ind = TRUE}, separate density lines will be plotted for each chain. If
#' \code{ind= FALSE}, one density line will be plotted for all chains.
#' @section Details:
#' \code{object} argument can be a \code{stanfit} object (\code{rstan} package), an \code{mcmc.list} object (\code{coda} package), an \code{R2jags} model object (\code{R2jags} package), a \code{jagsUI} model object (\code{jagsUI} package), or a matrix containing MCMC chains (each column representing MCMC output for a single parameter, rows representing iterations in the chain). The function automatically detects the object type and proceeds accordingly.
#'
#' Matrices for the \code{priors} argument can be generated using commands such as rnorm, rgamma, runif, etc. Distributions not supported by base R can be generated by using the appropriate packages. It is important to note that some discrepancies between MCMC samplers and R may exist regarding the parameterization of distributions - one example of this is the use of precision in JAGS but standard deviation in R for the 'second parameter' of the normal distribution. If the number of draws for each prior distribution is greater than the total number used for the density plot (\code{iter} times the number of chains), the function will use a subset of the prior draws. If the number of draws for each prior distribution is less than the total number used for the density plot, the function will resample (with replacement) from the prior to obtain the appropriate number of draws.
#'
#' @examples
#' #Load data
#' data(MCMC_data)
#'
#' #Traceplots for all 'beta' parameters - a pdf of the traceplots is generated by default
#' MCMCtrace(MCMC_data, params = 'beta', pdf = FALSE)
#'
#' #Traceplots (individual density lines for each chain) just for 'beta[1]'
#' #'params' takes regular expressions when ISB = FALSE, square brackets must be escaped with '\\'
#' MCMCtrace(MCMC_data, params = 'beta\\[1\\]', ISB = FALSE, ind = TRUE, pdf = FALSE)
#'
#' #Plot prior on top of posterior and calculate prior/posterior overlap just for 'beta[1]'
#' #'params' takes regular expressions when ISB = FALSE, square brackets must be escaped with '\\'
#' PR <- rnorm(15000, 0, 32)
#' MCMCtrace(MCMC_data, params = 'beta\\[1\\]', ISB = FALSE, priors = PR, pdf = FALSE)
#'
#' @export


MCMCtrace <- function(object,
                    params = 'all',
                    excl = NULL,
                    ISB = TRUE,
                    iter = 5000,
                    gvals = NULL,
                    priors = NULL,
                    pdf = TRUE,
                    open_pdf = TRUE,
                    filename,
                    wd = getwd(),
                    type = 'both',
                    ind = FALSE)
{

  .pardefault <- graphics::par(no.readonly = T)

  #SORTING BLOCK
  if(typeof(object) == 'double')
  {
    warning('Input type matrix - assuming only one chain for each parameter.')
    object1 <- coda::as.mcmc.list(coda::as.mcmc(object))
    object2 <- MCMCchains(object1, params, excl, ISB, mcmc.list = TRUE)
  }else{
    object2 <- MCMCchains(object, params, excl, ISB, mcmc.list = TRUE)
  }


  #OUTPUT BLOCK
  if(pdf == TRUE)
  {
    setwd(wd)
    if(missing(filename))
    {
      file_out <- 'MCMCtrace.pdf'
    }else{
      if(grepl('.pdf', filename, fixed = TRUE))
      {
        file_out <- paste0(filename)
      }else{
        file_out <- paste0(filename, '.pdf')
      }
    }
    pdf(file= file_out)
  }


  #PLOT BLOCK
  ref_col <- 'red'
  A_VAL <- 0.5 #alpha value
  graphics::layout(matrix(c(1, 2, 3, 4, 5, 6), 3, 2, byrow = TRUE))
  graphics::par(mar = c(4.1,4.1,2.1,1.1)) # bottom, left, top, right
  graphics::par(mgp = c(2.5,1,0)) #axis text distance
  gg_color_hue <- function(n)
  {
    hues = seq(15, 375, length = n+1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }

  n_chains <- length(object2)
  colors <- gg_color_hue(n_chains)
  gg_cols <- grDevices::col2rgb(colors)/255

  #see how many iter is in output and if it is at least specified iter (5k by default)
  if (nrow(object2[[1]]) > iter)
  {
    it <- (nrow(object2[[1]]) - iter+1) : nrow(object2[[1]])
  }else {
    it <- 1 : nrow(object2[[1]])
  }

  #parameter names
  np <- colnames(object2[[1]])

  #warnings and errors
  if (!is.null(priors))
  {
    if (NCOL(priors) == 1 & length(np) > 1)
    {
      warning('Only one prior specified for > 1 parameter. Using a single prior for all parameters.')
    }
    if ((NCOL(priors) > 1 & NCOL(priors) != length(np)))
    {
      stop('Number of priors does not equal number of specified parameters.')
    }
    if (NROW(priors) > length(it)*n_chains)
    {
      warning(paste0('Number of samples in prior is greater than number of total or specified iterations (for all chains) for specified parameter. Only last ', length(it)*n_chains, ' iterations will be used.'))
    }
    if (NROW(priors) < length(it)*n_chains)
    {
      warning(paste0('Number of samples in prior is less than number of total or specified iterations (for all chains) for specified parameter. Resampling from prior to generate ', length(it)*n_chains, ' total iterations.'))
    }
    if (type == 'trace')
    {
      warning("Prior overlap cannot be plotting without density plots. Use type = 'both' or type = 'density'.")
    }
  }

  if (!is.null(gvals))
  {
    if (length(gvals) == 1 & length(np) > 1)
    {
      warning('Only one generating value specified for > 1 parameter. Using a single generating value for all parameters.')
    }
    if (length(gvals) > 1 & length(gvals) != length(np))
    {
      stop('Number of generating values does not equal number of specified parameters.')
    }
  }


  if (type == 'both')
  {
    for (j in 1:length(np))
    {
      #j <- 1
      #trace
      tmlt <- do.call('cbind', object2[it, np[j]]) #make into matrix with three chains in columns
      graphics::matplot(it, tmlt, lwd = 1, lty= 1, type='l', main = paste0('Trace - ', np[j]),
              col= grDevices::rgb(red= gg_cols[1,], green= gg_cols[2,],
                       blue= gg_cols[3,], alpha = A_VAL),
              xlab= 'Iteration', ylab= 'Value')

      #density
      if (ind == TRUE & n_chains > 1)
      {
        dens <- apply(tmlt, 2, stats::density)
        max_den <- c()
        for (k in 1:NCOL(tmlt))
        {
          max_den <- c(max_den, max(dens[[k]]$y))
        }
        ylim <- c(0, max(max_den))

        graphics::plot(dens[[1]], xlab = 'Parameter estimate', ylim = ylim,
             lty = 1, lwd = 1, main = paste0('Density - ', np[j]),
             col = grDevices::rgb(red = gg_cols[1,1], green = gg_cols[2,1], blue = gg_cols[3,1]))

        for (l in 2:NCOL(tmlt))
        {
          graphics::lines(dens[[l]],
                col = grDevices::rgb(red = gg_cols[1,l], green = gg_cols[2,l],
                          blue = gg_cols[3,l]))
        }
      }else{
        #density plot
        graphics::plot(stats::density(rbind(tmlt)), xlab = 'Parameter estimate',
             lty = 1, lwd = 1, main = paste0('Density - ', np[j]))
      }

      #if priors are specified
      if (!is.null(priors))
      {
        if (NCOL(priors) == 1)
        {
          wp <- priors
        }else{
          wp <- priors[,j]
        }
        lwp <- length(wp)
        if (lwp > length(it)*n_chains)
        {
          #warnings in block above
          pit <- (lwp - (length(it)*n_chains)+1) : lwp
          wp2 <- wp[pit]
        }
        if (lwp < length(it)*n_chains)
        {
          #warnings in block above
          samps <- sample(wp, size = ((length(it)*n_chains)-lwp), replace = TRUE)
          wp2 <- c(wp, samps)
        }
        if (lwp == length(it)*n_chains)
        {
          wp2 <- wp
        }

        #calculate percent ovelap
        tmlt_1c <- matrix(tmlt, ncol = 1)
        pp <- list(wp2, tmlt_1c)
        ovrlap <- paste0(round((overlapping::overlap(pp)$OV[[1]])*100, digits = 1), '% overlap')

        #plot prior and overlap text
        dpr <- stats::density(wp2)
        graphics::lines(dpr, col = 'red')
        graphics::legend('topright', legend = ovrlap, bty = 'n', pch = NA, text.col = 'red')
      }

      #if generating values are specified - warnings in block above
      if (!is.null(gvals))
      {
        if (length(gvals) == 1)
        {
          gv <- gvals
        }else {
          gv <- gvals[j]
        }
        graphics::abline(v = gv, lty = 2, lwd = 3, col = ref_col)
      }
    }
  }

  if (type == 'trace')
  {
    for (j in 1: length(np))
    {
      #trace
      tmlt <- do.call('cbind', object2[it, np[j]])
      graphics::matplot(it, tmlt, lwd = 1, lty= 1, type='l', main = paste0('Trace - ', np[j]),
              col= grDevices::rgb(red= gg_cols[1,], green= gg_cols[2,],
                       blue= gg_cols[3,], alpha = A_VAL),
              xlab= 'Iteration', ylab= 'Value')
    }
  }

  if (type == 'density')
  {
    for (j in 1: length(np))
    {
      #trace
      tmlt <- do.call('cbind', object2[it, np[j]])

      if (ind == TRUE & n_chains > 1)
      {
        dens <- apply(tmlt, 2, stats::density)
        max_den <- c()
        for (k in 1:NCOL(tmlt))
        {
          max_den <- c(max_den, max(dens[[k]]$y))
        }
        ylim <- c(0, max(max_den))

        graphics::plot(dens[[1]], xlab = 'Parameter estimate', ylim = ylim,
             lty = 1, lwd = 1, main = paste0('Density - ', np[j]),
             col = grDevices::rgb(red= gg_cols[1,1], green= gg_cols[2,1], blue= gg_cols[3,1]))

        for (l in 2:NCOL(tmlt))
        {
          graphics::lines(dens[[l]],
                col = grDevices::rgb(red= gg_cols[1,l], green= gg_cols[2,l],
                          blue= gg_cols[3,l]))
        }
      }else{
        #density plot
        graphics::plot(stats::density(rbind(tmlt)), xlab = 'Parameter estimate',
             lty = 1, lwd = 1, main = paste0('Density - ', np[j]))
      }

      #if priors are specified
      if (!is.null(priors))
      {
        if (NCOL(priors) == 1 & length(np) > 1)
        {
          wp <- priors
        }else{
          wp <- priors[,j]
        }
        lwp <- length(wp)
        if (lwp > length(it)*n_chains)
        {
          pit <- (lwp - (length(it)*n_chains)+1) : lwp
          wp2 <- wp[pit]
        }
        if (lwp < length(it)*n_chains)
        {
          samps <- sample(wp, size = ((length(it)*n_chains)-lwp), replace = TRUE)
          wp2 <- c(wp, samps)
        }
        if (lwp == length(it)*n_chains)
        {
          wp2 <- wp
        }

        #calculate percent ovelap
        tmlt_1c <- matrix(tmlt, ncol = 1)
        pp <- list(wp2, tmlt_1c)
        ovrlap <- paste0(round((overlapping::overlap(pp)$OV[[1]])*100, digits = 1), '% overlap')

        #plot prior and overlap text
        dpr <- stats::density(wp2)
        graphics::lines(dpr, col = 'red')
        graphics::legend('topright', legend = ovrlap, bty = 'n', pch = NA, text.col = 'red')
      }

      #if generating values are specified - warnings in block above
      if (!is.null(gvals))
      {
        if (length(gvals) == 1)
        {
          gv <- gvals
        }else {
          gv <- gvals[j]
        }
        graphics::abline(v = gv, lty = 2, lwd = 3, col = ref_col)
      }
    }
  }

  if (type != 'both' & type != 'density' & type != 'trace')
  {
    stop('Invalid argument for "type". Valid inputs are "both", "trace", and "density".')
  }

  if(pdf == TRUE)
  {
    invisible(grDevices::dev.off())
    if(open_pdf == TRUE)
    {
      system(paste0('open ', paste0('"', file_out, '"')))
    }
  }else{
    graphics::par(.pardefault)
  }
}
