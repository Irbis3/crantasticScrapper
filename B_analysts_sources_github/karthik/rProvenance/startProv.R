startProv =
  #
  # Start collecting top-level task information
  #
function(cb = provCallback())
{
    #  Use taskCallbackManager(). Can be useful with, e.g. source() where we want to perform
    #  within call top-level evaluations via the evaluate method.
  
  id = addTaskCallback(if(is.function(cb)) cb else cb$handler)
  attr(cb, "taskCallbackID") = id
  cb
}


provCallback =
  #
  # Create the callback for addTaskCallback() along with accessor and control functions
  # Uses lexical scoping to collect and manage the list of tasks.
function(store = NULL, addToObject = TRUE)
{
  tasks = list()
  suspended = FALSE
  
  handler = function(expr, value, ok, visible) {
    if(suspended)
      return(TRUE)
    
    tasks[[ length(tasks) + 1L ]] <<- task <- Task(expr, value, ok, visible)

    if(addToObject) {
      if(length(expr) > 1 && as.character(expr[[1]])[1] %in% c("=", "<-", "<<-")) {
            # compute the var name from the expr
        varName = getVarName(expr[[2]])
        where = find(varName)
        obj = get(varName, where)
        obj = setProvInfo(obj, getSessionProvInfo(expr))
        assign(varName, obj, where)
      }
    }
    
    TRUE
  }
  
structure(
  list(handler = handler,
       tasks = function(omitProvenance = TRUE) {
                 if(omitProvenance) 
                   filterTasks(tasks, "ProvenanceTask")
                 else
                    tasks
                },
       pause = function() { suspended <<- TRUE
                            invisible(new("ProvenanceTask"))
                          },
       resume = function() { suspended <<- FALSE
                             invisible(new("ProvenanceTask"))
                           },       
       reset = function() { tasks <<- list()
                            invisible(new("ProvenanceTask"))
                          }),
    class = "BasicProvenanceTaskHandlers")
}

getVarName =
function(e)
{
  if(is.call(e))
    getVarName(e[[2]])
  else if(is.name(e))
    as.character(e)
  else
    stop("getVarName")
    
}

filterTasks =
function(taskList, className)
{
   taskList[ ! sapply(taskList, inherits, className) ]
}



