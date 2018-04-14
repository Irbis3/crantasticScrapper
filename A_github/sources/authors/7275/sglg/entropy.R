#' Tool to calculate the entropy for a generalized log-gamma distribution.
#'
#'\code{entropy} is used to obtain the entropy for a generalized log-gamma distribution.
#' @param mu numeric, represent the location parameter of a generalized log-gamma distribution. Default value is 0.
#' @param sigma numeric, represent the scale parameter of a generalized log-gamma distribution. Default value is 1.
#' @param lambda numeric, represent the shape parameter of a generalized log-gamma distribution. Default value is 1.

#' @references Carlos Alberto Cardozo Delgado, Semi-parametric generalized log-gamma regression models. Ph. D. thesis. Sao Paulo University.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>, G. Paula and L. Vanegas.
#' @examples
#' entropy(0,1,-1)    # Extreme value type I distribution, maximum case.
#' entropy(0,1,1)     # Extreme value type I distribution, minimum case.
#' entropy(0,1,0.077) # Standard normal distribution.
#' @export entropy
entropy <- function(mu, sigma, lambda) {
    if (missingArg(mu)) 
        mu <- 0
    if (missingArg(sigma)) 
        sigma <- 1
    if (missingArg(lambda)) 
        lambda <- 1
    
    output <- log(gamma(1/(lambda^2))/abs(lambda))
    output <- output + (1/(lambda^2)) * (1 - digamma(1/(lambda^2)))
    output <- round((1/sigma) * (log(sigma) + output), 4)
    
    return(output)
}
