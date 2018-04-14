Task =
function(expr, value, status, visible, time = Sys.time(),
          obj = new(guessTaskClass(expr, value, status)))
{

  obj@expression = expr
  obj@time = time
  obj@status = status
  obj@visible = visible

  if(is(obj, "AssignTask"))
    obj = setTargetVariable(expr, obj)
  
  obj
}

setTargetVariable =
function(expr,  obj = new("AssignTask"))
{
  if(is.name(expr[[2]]))
       obj@symName = expr[[2]]
  else {
         # far too simple
      obj@symName = expr[[2]][[2]]
      obj@subExpr = expr[[2]][-1] #XX Don't want this exactly, but want to remove the var. name
                                  # then R prints the result as a call.
  }
  obj
}

GRZPlotFunctionNames = c("hist", "plot")  #XXX extend.

ProvenanceFunctionNames = c("startProv")

guessTaskClass =
  #
  # guessTaskType in CodeDepends:codeTypes.R
  #
function(expr, value, status = TRUE)
{
   if(is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=", "assign"))
     return(if(is.name(expr[[2]])) "AssignTask" else "SubAssignTask") # make this test for SubAssign more robust

   if(is(value, "trellis"))
      return("LatticePlotTask")

   if(is(value, "ProvenanceObject"))
      return("ProvenanceTask")      # have to be careful, but basic idea okay. Perhaps escape with I().
   
   if(is.call(expr) && is.name(expr[[1]])) {
     fun = as.character(expr[[1]])
     if(fun %in% GRZPlotFunctionNames) 
        return("GRZPlotTask")
     if(fun %in% ProvenanceFunctionNames) 
        return("ProvenanceTask")     
   }
   
   "Task"
}
