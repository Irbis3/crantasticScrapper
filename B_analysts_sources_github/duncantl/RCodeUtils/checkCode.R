library(codetools)
checkPkgCode =
    #
    #  Check that thecode in every function references symbols that we can 
    # find.
    #
    
function(pkg, classes = TRUE,  pkgs = c("methods", "utils", "stats", "base") )  # "graphics"
{    
  library(pkg, character.only = TRUE)

  pkg0 = paste0("package:", pkg)
  
  o = ls(2)
  funs = lapply(o, function(var) get(var, pkg0))
  names(funs) = o

  isFun = sapply(funs, is.function)
  g = lapply(funs[isFun], codetools::findGlobals) # function(f) try(findGlobals(f)))

  ag = unlist(g)
  uag = unique(ag)
  ok =  uag %in% o

  baseSyms = ls("package:base", all = TRUE)

  invisible(lapply(pkgs, library, character.only = TRUE))
  allSyms = c(unlist(lapply(paste0("package:", pkgs), ls, all = TRUE)), baseSyms)

  w2 = uag[!ok] %in% allSyms
  
  m = uag[!ok][!w2]

  e = getNamespace(pkg)
  w3 = sapply(m, exists, e)
  miss = uag[!ok][!w2][!w3]

  if(classes)
    checkPkgClasses(pkg, pkg0)

  miss
}

#  Check the classes.

checkPkgClasses =
function(pkg, pkg0)
{    
  k = getClasses(pkg0)

    # exported ones
  kall = getClasses(pkg0, TRUE)
    #XXX Find the classes that are not exported.  
    # all including unexported ones.
  kall = unique(c(kall, gsub(".__C__", "", ls(e, pattern = ".__C", all = TRUE))))

  defs = sapply(k, checkClass, kall)
  stopifnot(all(sapply(defs, length)  == 0))
  TRUE
}

checkClass =
function(k, classNames, def = getClass(k))
{    
   supers = lapply(def@contains, slot, "superClass")
   w = unlist(supers) %in% classNames
   all(w)
   supers[!w]
}   



# Check all the parameter names and uses of them are correct.
