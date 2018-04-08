phsrd <-
function(beta, gamma){
  #browser()
  m_beta <- mean(beta)
  m_gamma <- mean(gamma)
  
  tmp <- 180/pi * atan(m_gamma/m_beta)
  
  # correction if in different quadrants
  if(m_beta < 0 | m_gamma < 0){
    if(m_beta > 0){
      # If in Quadrant IV
      tmp <- tmp + 360
    }else{
      # If in Quandrant II or III
      tmp <- tmp + 180
    }
  }
  return(tmp)
}
