getSessionProvInfo <-
  #
  # Create provenance information for an object, including how it was created (the command)
  # and the session, system, informaton.
  # The PID allows us to connect objects created in the same session.
  #
function(code = character(), ..., .meta = list(...))
{  
  if(length(code)) 
    .meta[["command"]] = code # paste(deparse(code), collapse = "\n")

  .meta[["when"]] = Sys.time()
  .meta[["pid"]] = Sys.getpid()
  .meta[["user"]] = Sys.info()[["login"]]
  .meta[["R.sessionInfo"]] = sessionInfo()  # png:::serializeBase64(sessionInfo())
  .meta[["R.systemInfo"]] = Sys.info()  # png:::serializeBase64(sessionInfo())  
  .meta[["R.rngKind"]] = RNGkind() # png:::serializeBase64(RNGkind())
  if(exists(".Random.seed", globalenv()))
     .meta[["R.Random.seed"]] = get(".Random.seed", globalenv()) # png:::serializeBase64(get(".Random.seed", globalenv()))

  .meta
}
  
