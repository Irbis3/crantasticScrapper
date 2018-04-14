pmc_internal <-
function(X, alpha = 0.05){
  
  # create output data.frame
  out <- data.frame(
    k = integer(),
    P.R. = integer(),
    P = numeric(),
    Mesor = numeric(),
    CI.M = numeric(),
    Amplitude =  numeric(),
    Lo.CI.A =  numeric(),
    Hi.CI.A =  numeric(),
    PHI =  numeric(),
    Lo.CI.PHI =  numeric(),
    Hi.CI.PHI =  numeric()
  )
  printP = numeric()
  # phi <- intToUtf8(0x03A6)   upper case
  # phi <- intToUtf8(0x03C6)   lower case
  sigma_matrix <- matrix(0, nrow = 3, ncol = 3)
  
  k <- nrow(X)
  df <- k - 1
  
  beta= X$amp*cos(X$phi * pi/180)
  gamma=-X$amp*sin(X$phi * pi/180)
  
  beta_hat <- mean(beta)
  gamma_hat <- mean(gamma)
  
  # fill covariance matrix (triangular)
  sigma_matrix[1,1] <- sum((X$mesor - mean(X$mesor))^2)/df
  sigma_matrix[1,2] <- sum((X$mesor - mean(X$mesor)) %*% (beta - mean(beta)))/df
  sigma_matrix[1,3] <- sum((X$mesor - mean(X$mesor)) %*% (gamma - mean(gamma)))/df
  sigma_matrix[2,2] <- sum((beta - mean(beta))^2)/df
  sigma_matrix[2,3] <- sum((beta - mean(beta)) %*% (gamma - mean(gamma)))/df
  sigma_matrix[3,3] <- sum((gamma - mean(gamma))^2)/df
  
  # mirror lower matrix
  sigma_matrix[lower.tri(sigma_matrix)] <- sigma_matrix[upper.tri(sigma_matrix)]
  
  # Percentage Rhythm
  pr <- mean(X$pr)
  
  # Population Mesor
  mesor <- mean(X$mesor)
  
  # Population Acrophase
  phi <- -phsrd(beta, gamma)
  
  # Population Amplitude
  amp <- module(beta, gamma)
  
  # t-value
  tval <- qt(1-alpha/2, df)
  
  # Mesor CI
  cim <- tval * sqrt(sigma_matrix[1,1]/k)
  
  # Amplitude CI
  c22 <- (sigma_matrix[2,2] * beta_hat^2 + 2*sigma_matrix[2,3]*beta_hat*gamma_hat + sigma_matrix[3,3] * gamma_hat^2) / (k * amp^2)
  cia <- tval*sqrt(c22)

  # Acrophase CI
  c23 <- (-(sigma_matrix[2,2] - sigma_matrix[3,3]) * (beta_hat*gamma_hat) + sigma_matrix[2,3]*(beta_hat^2 - gamma_hat^2)) / (k * amp^2)
  c33 <- (sigma_matrix[2,2] * gamma_hat^2 - 2 * sigma_matrix[2,3] * beta_hat * gamma_hat + sigma_matrix[3,3]*beta_hat^2) / (k * amp^2)
  
  an1 <- amp^2 - (c22*c33 - c23^2) * (tval^2)/c33
  
  if(an1 < 0){
    phi1 <- 0
    phi2 <- 0
  }else{
    phi1 <- phi + atan((c23 * tval^2 + tval*sqrt(c33) * sqrt(an1))/(amp^2 - c22*tval^2)) * 180/pi
    phi2 <- phi + atan((c23 * tval^2 - tval*sqrt(c33) * sqrt(an1))/(amp^2 - c22*tval^2)) * 180/pi
    phi1 <- phase(phi1)
    phi2 <- phase(phi2)
  }
  
  
  # p-value calculation
  r <- sigma_matrix[2,3]/sqrt(sigma_matrix[2,2]*sigma_matrix[3,3])
  fval <- k*(k-2)/(2*(k-1) * (1-r^2)) *
    (beta_hat^2/sigma_matrix[2,2]
     -2*r*beta_hat*gamma_hat/sqrt(sigma_matrix[2,2]*sigma_matrix[3,3])
     +gamma_hat^2/sigma_matrix[3,3]
    )
  p <- pf(fval, df1 = 2, df2 = k - 2, lower.tail = FALSE)
  
  
  
  # This could be much more concise but doing it like this to be explicit
  out[1,"k"] <- as.integer(k)
  out[1,"P.R."] <- round(mean(X$pr),1)
  printP <- round(p, 4)
  printP[printP<.005]<-c("<.001")
  out[1,"P"]<-printP
  out[1,"Mesor"] <- signif(mesor, 6)
  out[1,"CI.M"] <- signif(cim, 4)
  out[1,"Amplitude"] <- signif(amp, 5)
  out[1,"Lo.CI.A"] <- ifelse(p < alpha, signif(amp - cia, 5), 0)     #  round indicates # decimal places
  out[1,"Hi.CI.A"] <- ifelse(p < alpha, signif(amp + cia, 5), 0)     #  signif indicates # of significant digires
  out[1,"PHI"] <- round(phi, 1)
  out[1,"Lo.CI.PHI"] <- ifelse(p < alpha, round(phi1, 1), 0)    #  remove -.05
  out[1,"Hi.CI.PHI"] <- ifelse(p < alpha, round(phi2, 1), 0)
  #out[1,"Period"] <- perd
  
  return(out )
  
}
