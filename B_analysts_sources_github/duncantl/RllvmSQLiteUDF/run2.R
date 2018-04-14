library("RSQLite")
library("RSQLiteUDF")
library("Rllvm") 

db = dbConnect(SQLite(), "foo")

m = parseIR("minSQLFib.ll")
llvmAddSymbol(getNativeSymbolInfo("sqlite3_value_int"))
llvmAddSymbol(getNativeSymbolInfo("sqlite3_result_int"))
# for dexp()
llvmAddSymbol(getNativeSymbolInfo("sqlite3_value_double"))
llvmAddSymbol(getNativeSymbolInfo("sqlite3_result_double"))

ee = ExecutionEngine(m)

ans = .llvm(m$fib2, 10L, .ee = ee)
stopifnot(ans == 55)


sqliteExtension(db, system.file("libs", "RSQLiteUDF.so", package = "RSQLiteUDF"))
b = .Call("R_getSQLite3API", PACKAGE = "RSQLiteUDF")


cif = Rffi::CIF(Rffi::voidType, list(Rffi::pointerType))
.llvm(m$R_setSQLite3API, b, .ee = ee, .ffi = cif)


ptr = getPointerToFunction(m$sqlFib3, ee)
createSQLFunction(db, ptr@ref, "fib", nargs = 1L)
d = dbGetQuery(db, "SELECT fib(x) FROM mytable")

ptr = getPointerToFunction(m$sqlDexp, ee)
createSQLFunction(db, ptr@ref, "dexp", nargs = 2L)

createSQLFunction(db, getNativeSymbolInfo("sqlLog", "RSQLiteUDF"), "log", nargs = 1L)

d1 = dbGetQuery(db, "SELECT dexp(y/x, 1.2) FROM mytable")
d2 = dbGetQuery(db, "SELECT sum(log(dexp(y/x, 1.2))) FROM mytable")


data = dbGetQuery(db, "SELECT * FROM mytable")
stopifnot(all.equal(dexp(data$y/data$x, 1.2), d1[[1]]))
stopifnot(all.equal(sum(log(dexp(data$y/data$x, 1.2))), d2[1,1]))

