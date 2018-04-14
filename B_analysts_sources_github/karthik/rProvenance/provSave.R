# Save R objects based on provenance information, i.e. how they are related to each other.
#

provSave =
  #
  #
  #  This save()s the specified object along with the objects
  #  that were used to create it. We compute this information
  #  from the provenance information in each object.
  #  We may not want to store all these objects.
  #  Instead, we may just want to store the object obj
  #  but add to it an attribute that gives the commands to
  #  recreate it.  In other words, we don't store the objects
  #  on which this depends, but we store how to recreate them.
  #  Should also include the random seed
function(obj, file, name = deparse(substitute(obj)), dependsCommandsOnly = FALSE)
{
  name
  if(dependsCommandsOnly) {
     cmds = getCommands(obj)
     info = getProvInfo(obj)
     info$command = cmds
     e = new.env()
     attr(obj, "provenanceInfo") = info
     assign(name, obj, e)

     return(save(list = name, file = file, envir = e))
  }
  
  objs = getInputVariables(obj)
  assign(name, obj, objs)
  save(list = ls(objs, all.names = TRUE), file = file, envir = objs)
}

getInputVariables =
  #
  #  From the current object obj, find the objects on which it depends
  #  by traversing the provenance information it contains and the information
  #  in these other objects.
  #
function(obj,  info = getProvInfo(obj), recursive = TRUE,
         objects = new.env(parent = emptyenv()))  
{
  inputs = getInputs(info$command)
  vars = inputs@inputs

  for(v in vars) {
      val = get(v)
      assign(v, val, objects)
      getInputVariables(val, objects = objects)
  }
  objects
}
