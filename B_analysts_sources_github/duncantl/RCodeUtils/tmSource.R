tmSource = tmsource =
function(file, local = globalenv(), ...)
{
    if(length(file) > 1)
       return(structure(lapply(file, tmSource, local, ...), names = file))
    
    e = parse(file)
    times = lapply(e, function(x) system.time( eval( e, local) ))
    names(times) = lapply(e, function(x) paste(deparse(x), collapse = " "))
    times
}
