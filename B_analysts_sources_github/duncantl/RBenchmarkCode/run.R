targetFile = "sample.csv"
if(!file.exists(targetFile))
  cat(1:1e8, sep = "\n", file = targetFile)

source("sampleCSV.R")

MaxNumLines = 1e8
MaxNumLines = 1e5
sampleSize = 1e3
#i = ceiling(runif(1e3, max = MaxNumLines))
i = sample(MaxNumLines, sampleSize)

approach3 = system.time(sampleLines(targetFile, whichLines = i, header = FALSE))
approach1 = system.time(readLines(targetFile)[i])


if(require("FastCSVSample")) {
  # This is the compiled version and so what we would like to be able to achieve.
  # The Rllvm-compiled version comes within between 2% faster to 20% slower
  # than the C code, depending on the machine and compiler and version of the compiler.
  # See RllvmTimings
  tmf.1e3 = replicate(10, system.time(.Call("R_csv_sample", targetFile, i)))/10
     # includes some GC
  tmf.1e3 = system.time(replicate(10, .Call("R_csv_sample", targetFile, i)))/10
}
