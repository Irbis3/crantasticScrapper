# Functions to query the provenance information.
# See sessionProv.R

getProvInfo =
function(obj)
{
  attr(obj, "provenanceInfo")
}

setProvInfo =
function(obj, info = getSessionProvInfo())
{
   # keep any fields already there that are not in info.
  old = getProvInfo(obj)
  if(!is.null(old)) {
    ids = setdiff(names(old), names(info))
    if(length(ids))
      info[ids] = old[ids]
  }
  
  attr(obj, "provenanceInfo") <- info
  obj
}



creator =
function(obj, info = getProvInfo(obj))
{
  info$user
}

pid =
function(obj, info = getProvInfo(obj))
{
  info$pid
}

rng =
function(obj, info = getProvInfo(obj))
{
  info[c("R.rngKind", "R.Random.seed")]
}

session =
function(obj, info = getProvInfo(obj))
{
   info$R.sessionInfo
}

sys =
function(obj, info = getProvInfo(obj))
{
   info$R.systemInfo
} 

when =
function(obj, info = getProvInfo(obj))
{
   info$when
} 

