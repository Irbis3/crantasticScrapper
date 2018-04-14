
getCarLocations = 
function(g)
{
  i = row(g)[g != ""]
  j = col(g)[g != ""]
  pos = cbind(i, j)
  data.frame(i = i, j = j, colors = g[pos])
}





getCarLocations =
function(g)
{
  i = row(g)[g != ""]
  j = col(g)[g != ""]
  pos = cbind(i, j)
  structure(pos, dimnames = list(g[pos], c("i", "j")))
}





getCarLocations =
function(g)
{
  w = (g != "")
  i = row(g)[w]
  j = col(g)[w]
  pos = cbind(i, j)
  structure(pos, dimnames = list(g[pos], c("i", "j")))
}

