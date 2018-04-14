readData =
function(filename, ...)
{
  if(grepl("rda$", filename)) {
    e = new.env()
    vars = load(filename,  e)
    mget(vars, e)
  } else
    read.csv(filename)
}

if(FALSE) {
 readData = rProv:::provenanceTrace(readData)  
}
