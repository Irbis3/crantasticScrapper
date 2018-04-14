setClass("MISQ", representation=representation(phi = "numeric") )

# MISQ_method <- function(name) {
#   paste("MISQ", name, sep="__")
# }

setMethod("initialize", "MISQ",definition=function(.Object, phi) {
			  if (length(phi)) 
              .Object@phi = phi
              return(.Object)
          }	)

setMethod("filter",
          signature(x = "numeric", filter = "MISQ"),
          function (x, filter) 
          {
            .Call("MISQPlusNumericFilter", x, filter@phi)
          }
)

setMethod("filter",
          signature(x = "matrix", filter = "MISQ"),
          function (x, filter) 
          {
            .Call("MISQPlusNumericMatrixFilter", x, filter@phi)
          }
)

setMethod("dist",
          signature(x = "matrix", method = "MISQ"),
          function (x, method, diag = FALSE, upper = FALSE) 
          {
            if(ncol(x) > 2)
              stop("TODO")
            n <- nrow(x)
            phi <- method@phi;
            m <- length(phi) - 1
            Phi <- matrix(0, n - m, n)
            for (i in 1:(n-m)) {
              Phi[i,0:m + i] <- phi
            }
            Lambda <- t(Phi) %*% Phi
            
            diff_x <- .Call("MISQPlusDiffMatrix", x)
            Phi_e <- Phi %*% diff_x
			M_2 <- apply(Phi_e^2, 2, sum) / ( (n-m) * sum(phi^2) )
			Y_1 <- sum(Phi_e^2)^2
			Y_2 <- mean(Phi_e^4)
			M_4_coef_1 <- sum(diag(Lambda)^2)
			M_2_sq_coef_1 <- sum(diag(Lambda))^2 + 2 * sum(Lambda^2) - 3 * sum(diag(Lambda)^2)
			M_4_coef_2 <- sum(phi^4)
			M_2_sq_coef_2 <- 3 * ( sum(phi^2)^2 - sum(phi^4) )
			M_4_vec <- solve(matrix(c(M_4_coef_1,M_2_sq_coef_1,
					 M_4_coef_2,M_2_sq_coef_2
							),2,2,byrow=TRUE), c(Y_1,Y_2))
			dist_hat <- sum(diff_x^2)/n - sum(Phi_e^2)/( (n - m) * sum(phi^2) )
			dist_hat_var <- 4 * dist_hat * M_2 / n + ( sum(diag(Lambda^2))/( (n-m)^2 * sum(phi^2)^2 ) - 1/n ) * M_4_vec[1] + (1/n - ( sum(Lambda) - 2 * sum(diag(Lambda)^2) )/( (n-m)^2 * sum(phi^2)^2 )  ) * M_4_vec[2]
            list(dist_hat = dist_hat, dist_hat_var = dist_hat_var, M_2=  M_2, M_4 = M_4_vec[1])
          } )