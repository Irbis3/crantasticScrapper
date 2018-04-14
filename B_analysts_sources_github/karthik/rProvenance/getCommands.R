#
# These functions are related to recovering the information
# about how to recreate an object.  We use the provenance
# information

getCommands =
  #
  #  From the current object obj, find the objects on which it depends
  #  by traversing the provenance information it contains and the information
  #  in these other objects.
  #
function(obj,  info = getProvInfo(obj), recursive = TRUE, name = deparse(substitute(obj)))
{
  expr = info$command # parse(text = info$command)
  inputs = getInputs(expr)
  vars = inputs@inputs
  ans = list()
  ans[[name]] = expr
  while(length(vars)) {
      v = vars[1]
      vars = vars[-1]
      val = get(v)
      cmd <- ans[[v]] <- getProvInfo(val)$command # parse(text = )
      inputs = getInputs(cmd)
      vars <- c(vars, inputs@inputs) 
  }
  rev(ans)
}

