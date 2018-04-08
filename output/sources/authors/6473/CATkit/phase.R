phase <-
function(phi){
  if(phi > 0){
    phi <- phi - 360
  }
  if(phi < -360){
    phi <- phi + 360
  }
  return(phi)
}
