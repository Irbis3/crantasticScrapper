excludeFunctions = c("[", "[[", "[[<-", "[<-", "{", "=", "for", "seq", "vector", "length")

traceProv =
  #
  # Add expressions to the code of a function to trace the result.
  #
  #
function(fun, op = as.name("addProvenance"), enter = NULL,
          addLoopCounters = containsLoops(fun), recursive = TRUE)
{
  w = character()
  name = fun
  if(is.character(fun)) {
     if(fun %in% excludeFunctions)
       return(fun)
     
     w = find(fun, mode = "function")
     if(length(w) == 0)
       stop("cannot find ", fun)

     if(w[1] == "package:base")
       return(invisible(fun))
     
     fun = get(fun, w[1])
   }

  nfun = provenanceTrace(fun, op, enter, addLoopCounters)

  if(length(w)) {
    e = environment(fun)
    if(isNamespace(e)) 
      assignInNamespace(name, fun, asNamespace(e))  # sprintf("package:%s", e$.packageName))
    else
      assign(name, fun, w[1])
  }

  if(recursive) {
     funNames = findGlobals(fun, FALSE)$functions
     lapply(funNames, traceProv, op, enter, addLoopCounters, recursive)
  }
  
  invisible(nfun)
}


provenanceTrace =
  #
  # add code to the return expression of the function (either explicit return or the last expression)
  # that is a call to op
  #
  #  See RLLVMCompile for code to do the rewriting. Merge the code.
  #
  # Totally not wedded to these names or implementation. Just putting these here.
  #
  #  We may want to capture information such  as the value of .Random.seed or simply the current time (Sys.time())
  # when we enter the function.
  # In a loop, we may want to capture the number of iterations before we exit the loop
  # So we may want to add code to the top of the function and include that in the call to the provenance function
  #
  #  When reading data, we want to capture the name of the file or URL, the current date and time.
  #  Within a call to readLines(), we may want to identify the line numbers being read or the start and ending
  #  position in the connection.
  #
  #  Push the provenance information and data to a central repository.
  #
  #  Grab the process id, time stamp, etc.  See getSessionProvInfo().
  #
  #
  #  op can be the name of a function or an actual call. For a call, the first
  #  argument will be replaced by the actual expression giving the object whose
  #  provenance we want.
  # See connections.R
  #

function(fun, op = as.name("addProvenance"), enter = NULL,
         addLoopCounters = containsLoops(fun),
         files = character())
{
  b = body(fun)
  
  
  if(class(b) != "{") {  # so just a single expression
     body(fun) = setProvCall(b, op, TRUE, addLoopCounters, files = files)
     return(fun)
  }

  for(i in 2:length(b)) {
    e = b[[ i ]]
    e = setProvCall(e, op, i == length(b), addLoopCounters = addLoopCounters, files = files)
    b[[ i ]] = e
  }

   # Add code to run when we enter this function.
  if(length(enter)) {
    b[2:length(b) + 1 ] = b[2:length(b)]
    b[[2]] = enter
  }
  

  if(addLoopCounters) {
       # need to add this to the call to the function.
      b[(2:length(b))+1]  =  b[(2:length(b))]
      b[[2]] = substitute(.loopCounter <- integer())
   }
  
  body(fun) = b
  fun
}

containsLoops =
function(fun, globals = findGlobals(fun, FALSE)$functions)
{
  any(c("for", "while") %in% globals)
}


setProvCall =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
  UseMethod("setProvCall")
}

is.literal =
function(x)
  class(x) %in% c("character", "logical", "integer", "numeric")

setProvCall.default =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
     # Can do this by pasting the class name and dispatching ourselves
     # but have to find the function.
     # R should do this.
    if(is(expr, "if"))
      setProvCall.if(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)
    else if(is(expr, "for"))
      setProvCall.for(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)
    else if(is(expr, "while"))
      setProvCall.while(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)
    else if(is(expr, "=") || is.call(expr) && as.character(expr[[1]]) == "<-")
      `setProvCall.=`(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)
    else if(is(expr, "{"))
      setProvCall.list(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)
    else if(is.literal(expr))
      setProvCall.literal(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)                
    else if(is(expr, "name"))
      setProvCall.name(expr, op, last, addLoopCounters = addLoopCounters, files = files, ...)
    else stop("no method yet for ", class(expr))
}

setProvCall.symbol = setProvCall.name =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
  if(last) {
    if(is.call(op)) {
      op[[2]] = expr
      e = op
    } else {
      e = substitute(op(expr, sys.calls(), sys.frames(), sys.nframe()), list(expr = expr, op = op))
      if(length(files)) 
        e[["files"]] = as.name(files)
    }

    if(addLoopCounters)
      e[[length(e) + 1]] = quote(addLoopCounters)
    e
  } else
    expr
}

setProvCall.call =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
  isReturn = as.character(expr[[1]]) == "return"
  if(!isReturn && !last)
    return(expr)
  
  if(isReturn) {
     expr[[2]] = setProvCall.name(expr[[2]], op, TRUE, addLoopCounters, files = files, ...)
     expr
  } else
     setProvCall.name(expr, op, TRUE, addLoopCounters, files = files, ...)
}

setProvCall.if =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
   expr[[3]] = setProvCall(expr[[3]], op, last, addLoopCounters, files = files, ...)
   if(length(expr) == 4)
      expr[[4]] = setProvCall(expr[[4]], op, last, addLoopCounters, files = files, ...)
   expr
}

setProvCall.for =
  #
  # XXX reset/decrement the loop counter when we are finished
  #
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
   expr[[4]] = setProvCall(expr[[4]], op, last, addLoopCounters, files = files, ...)

   if(addLoopCounters) {
     if(class(expr[[4]]) != "{") {
       tmp = quote({ a ; b})
       tmp[[3]] = expr[[4]]
     } else {
       tmp = expr[[4]]
       tmp[2:length(tmp) + 1] = tmp[2:length(tmp)]
     }
     tmp[[2]] = quote(.loopCounters <- .loopCounters + 1)
     expr[[4]] = tmp
   } 
   expr
}

setProvCall.while =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
   expr[[3]] = setProvCall(expr[[3]], op, last, addLoopCounters, files = files, ...)
   expr
}

`setProvCall.=` =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
   if(!last)
      return(expr)
   expr[[3]] = setProvCall(expr[[3]], op, TRUE, addLoopCounters, files = files, ...)
   expr
}

setProvCall.literal =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
  setProvCall.name(expr, op, last, addLoopCounters, files = files, ...)
}

setProvCall.list =
function(expr, op, last = FALSE, addLoopCounters = FALSE, files = character(), ...)
{
#  expr[] = lapply(expr, setProvCall, op, last, ...)
#  expr

 for( i in seq(along = expr)[-1]) {
    expr[[i]] = setProvCall(expr[[i]], op, i == length(expr), addLoopCounters, files = files, ...)
 }
 expr
}

addProvenance =
  #
  # This is a function that is the default for provenanceTrace()
  # to add to expressions in functions to return an object along
  # with provenance information.
  #
function(obj, calls = sys.calls(), frames = sys.frames(), nframe = sys.nframe(), files = NULL,
          .conInitialState = NULL, ...)
{
  info = list(timestamp =  Sys.time(),
                        call = calls[[nframe]],
                        calls = calls)

  if(length(files)) 
    info$files = structure(lapply(files, getFileProvenance), names = files)

  if(length(.conInitialState))
    info$connectionState = .conInitialState
  
  setProvInfo(obj, info)
}

getFileProvenance =
function(file)
{
  if(is.character(file))
   file.info(file)
  else
    stop("bob")
}
