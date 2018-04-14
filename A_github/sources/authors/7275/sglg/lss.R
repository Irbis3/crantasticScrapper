#' Measures of location, scale and shape measures for a generalized log-gamma distribution
#'
#'\code{lss} is used to obtain the mean, variance, skewness and kurtosis for a generalized log-gamma distribution.
#' @param mu numeric, represent the location parameter of a generalized log-gamma distribution. Default value is 0.
#' @param sigma numeric, represent the scale parameter of a generalized log-gamma distribution. Default value is 1.
#' @param lambda numeric, represent the shape parameter of a generalized log-gamma distribution. Default value is 1.

#' @references Carlos Alberto Cardozo Delgado, Semi-parametric generalized log-gamma regression models. Ph. D. thesis. Sao Paulo University.
#' @references National Institute of Standards and Technology, NIST.  Engineering Statistics Handbook.
#' http://www.itl.nist.gov/div898/handbook/apr/section1/apr163.htm
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>, G. Paula and L. Vanegas.
#' @examples
#' lss(0,1,-1)    # Extreme value type I distribution, maximum case.
#' lss(0,1,1)     # Extreme value type I distribution, minimum case.
#' lss(0,1,0.005) # Standard normal distribution.
#' @export lss
lss = function(mu, sigma, lambda) {
    if (missingArg(mu)) 
        mu <- 0
    if (missingArg(sigma)) 
        sigma <- 1
    if (missingArg(lambda)) 
        lambda <- 1
    
    mean <- round(mu + sigma * (digamma(1/lambda^2) - log(1/lambda^2))/(lambda), 
        digits = 4)
    variance <- round((sigma^2) * (trigamma(1/lambda^2))/(lambda^2), digits = 4)
    skewness <- sign(lambda) * round(psigamma(1/lambda^2, deriv = 2)/(trigamma(1/lambda^2)^(3/2)), 
        digits = 4)
    kurtosis <- round((psigamma(1/lambda^2, deriv = 3)/((trigamma(1/lambda^2))^2)) + 
        3, digits = 4)
    
    return(list(mean = mean, variance = variance, skewness = skewness, kurtosis = kurtosis))
}
