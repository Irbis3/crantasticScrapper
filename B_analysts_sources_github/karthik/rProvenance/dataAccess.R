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




mt = readData("../sampleData/mtcars.csv")
mt1 = readData("../sampleData/mtcars.rda")

logAccess = function(...) { k = sys.calls() ; browser() }

trace(load, quote(logAccess("load", file)))
trace(read.table, quote(logAccess("read.table", file)))
#trace(scan, logAccess)
trace(open, logAccess)

mt = readData("../sampleData/mtcars.csv")
mt1 = readData("../sampleData/mtcars.rda")
