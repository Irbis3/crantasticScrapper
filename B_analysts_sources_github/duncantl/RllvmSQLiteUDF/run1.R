# git@github.com:duncantl/RSQLiteUDF.git
# RSQLiteUDF needs RSQlite version 1.0-0, not greater than this, for now!

library("RSQLite")
library("RSQLiteUDF")
library("Rllvm") 

db = dbConnect(SQLite(), "foo")
#sqliteExtension(db) 

m = parseIR("fib.ll")
llvmAddSymbol(getNativeSymbolInfo("sqlite3_value_int"))
llvmAddSymbol(getNativeSymbolInfo("sqlite3_result_int"))

ee = ExecutionEngine(m)

# Check code works
ans = .llvm(m$fib2, 10L, .ee = ee)
stopifnot(ans == 55)


######
# Before we call any routines in our LLVM module via SQL
# Register the sqlTen routine from the llvm Module with SQLite3 via the RSQLiteUDF package
# This will ensure the sqlite3_api variable is set in the RSQLiteUDF DLL
sqliteExtension(db, system.file("libs", "RSQLiteUDF.so", package = "RSQLiteUDF"))

# Initialize the sqlite3_api variable in the LLVM module with the value of sqlite3_api in the RSQLiteUDF DLL
b = .Call("R_getSQLite3API", PACKAGE = "RSQLiteUDF")
.llvm(m$R_setSQLite3API, b, .ee = ee, .ffi = Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)))



ptr = getPointerToFunction(m$sqlTen, ee)
createSQLFunction(db, ptr@ref, "ten", nargs = 0L)

d = dbGetQuery(db, "SELECT ten()")
stopifnot(d[1,1] == 10)
print(d)

ptr = getPointerToFunction(m$sqlFib3, ee)
createSQLFunction(db, ptr@ref, "fib", nargs = 1L)
d = dbGetQuery(db, "SELECT fib(x) FROM mytable")
print(d)



