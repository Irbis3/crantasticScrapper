
library(RLLVMCompile)
mod = Module("fgets")
ee = ExecutionEngine(mod)

stringType = pointerType(getIntegerType(8L, getContext(mod)))
#FILEType = pointerType(Int32Type) # an opaque struct type?
FILEType = pointerType(structType(list(o=Int32Type))) # an opaque struct type?

.str = createGlobalVariable(".str", mod, val = string(1000), linkage = PrivateLinkage)

 # we'd like to be able to use .str and have R functions call getGetElementPtr for us.
p = getGetElementPtr(.str)
.ptr = createGlobalVariable("ptr", mod, stringType, p) 

  # could specify this information via .routineInfo parameter of compileFunction()
declareFunction(list(stringType, stringType, Int32Type, FILEType), "fgets", mod) 
llvmAddSymbol(fgets = getNativeSymbolInfo("fgets")$address)


Fgets  =
function(file)
{
  fgets(ptr, 1000L, file) #  != NULL # should raise an error if NULL.
}


fun = compileFunction(Fgets, stringType, list(FILEType), mod, name = "Fgets")


readTo =
function(n, file)
{
  ctr = 0L
  tmp = "" # character() # ""
    # perhaps use replicate(n, Fgets(file))
    # and compile that out.  But only want the last value.
  while(ctr < n) {
     tmp = Fgets(file)   
     ctr = ctr + 1L
  }
  tmp
}


rr = compileFunction(readTo, stringType, list(Int32Type, FILEType), mod, name = "readTo")


readSelectedLines = 
function(lineSkip, file)
{
   ans = character(length(lineSkip))
   for(i in seq(along = lineSkip))
      ans[i] = readTo(lineSkip[i], file)
   ans
}


readSelectedLines =
function(lineSkip, file, .ee)
{
  f = function(i, file) .llvm(rr, i, file, .ee = .ee)
  sapply(lineSkip, f, file)
}


library(RCurl)
#ff = CFILE("../NAMESPACE")
#ee = ExecutionEngine(mod)
#readSelectedLines(c(3, 10, 10), ff, ee)


readSelectedLines =
function(lineSkip, file)
{
  sapply(lineSkip, readTo, file)
}
readSelectedLines.c = compileFunction(readSelectedLines, STRSXPType, list(INTSXPType, FILEType), mod)



targetFile = "sample.csv"
if(!file.exists(targetFile))
  cat(1:1e8, sep = "\n", file = targetFile)

N = 1e3
lineNums = sort(as.integer(sample(1e8, N)))
lineSkips = as.integer(diff(c(0L, lineNums)))


# A smaller file.
ff = CFILE("sample1.csv")
.llvm(mod$Fgets, ff)
.llvm(mod$readTo, 1, ff) # 2nd line
.llvm(mod$readTo, 10, ff) # 12th line

.llvm(mod$readSelectedLines, as.integer(c(5, 7, 2, 19)), ff)


tm.1e3 = replicate(10, { ff = CFILE(targetFile); system.time(.llvm(mod$readSelectedLines, lineSkips, ff))})



library(FastCSVSample)
tmf.1e3 = replicate(10, system.time(.Call("R_csv_sample", targetFile, lineNums)))



readTo = 
function(numLines, con)
 readLines(con, numLines)[numLines]

readSelectedLines =
function(lineSkip, file)
  sapply(lineSkip, readTo, file)


con = file("sample.csv", "r"); tmr.1e3 = system.time(readSelectedLines(lineSkips, con))
tmr.1e3




targetFile = "sample.csv"
N = 1e5
lineNums = sort(as.integer(sample(1e8, N)))
lineSkips = as.integer(diff(c(0L, lineNums)))


readTo = 
function(numLines, con)
 readLines(con, numLines)[numLines]

con = file(targetFile, "r"); tmr.1e6 = system.time(readSelectedLines(lineSkips, con))

tm.1e5 = replicate(10, { ff = CFILE(targetFile); system.time(.llvm(mod$readSelectedLines, lineSkips, ff))})
library(FastCSVSample)
tmf.1e5 = replicate(10, system.time(.Call("R_csv_sample", targetFile, lineNums)))

o = rbind("readLines" = tmr.1e6[3], "LLVM" = median(tm.1e5[3,]), "FastCSVSample" = median(tmf.1e5[3,]))
max(o)/o


res = structure(list("readLines" = tmr.1e6, "LLVM" = tm.1e5, "FastCSVSample" = tmr.1e6),
                   N = N, session = sessionInfo(), system = Sys.info(), when = Sys.time())
id = sprintf("sampleCSV.tm_%s", N)
assign(id, res, globalenv())
#save( list = id, file = sprintf("%s.rda", id))




