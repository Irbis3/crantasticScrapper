
# Functions
createCluster = function(noCores, logfile = "/dev/null", export = NULL, lib = NULL) {
  require(doSNOW)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) { 
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}
