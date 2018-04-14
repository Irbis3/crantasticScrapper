    # git@github.com:duncantl/RSQLiteUDF.git

# RSQLiteUDF needs RSQlite version 1.0-0, not greater than this, for now!


# load the fib.so. Of course, we wouldn't have this.
sqliteExtension(db, paste(getwd(), "fib.so", sep = "/") )
# So we'll load the RSQLiteUDF.so
sqliteExtension(db, system.file("libs", "RSQLiteUDF.so", package = "RSQLiteUDF"))
# This triggers the call to sqlite3_extension_init in RSQLiteUDF and sets the sqlite3_api variable there.

# From the llvm Module
ptr = getPointerToFunction(m$sqlTen, ee)
# registering the function triggers the call to the sqlite3_X_init routine in RSQLiteUDF which we want
# so that the sqlite3_api variable in the RSQLiteUDF DLL is set.
createSQLFunction(db, ptr@ref, "ten", nargs = 0L)


b = getNativeSymbolInfo("sqlite3_api", "RSQLiteUDF")$address
#dyn.load("fib.so")
#r = getNativeSymbolInfo("R_setSQLite3API", "fib")
#library(Rffi)
#callCIF(Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)), r, b)
#.Call(r, b)

.llvm(m$R_setSQLite3API, b, .ee = ee, .ffi = Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)))
d = dbGetQuery(db, "SELECT ten()")

ptr = getPointerToFunction(m$sqlFib3, ee)
createSQLFunction(db, ptr@ref, "fib", nargs = 1L)
d = dbGetQuery(db, "SELECT fib(x) FROM mytable LIMIT 5")

dyn.load("fib.so")
p = getNativeSymbolInfo("fib2")
createSQLFunction(db, p, "fib2", nargs = 1L)
d = dbGetQuery(db, "SELECT fib2(x) FROM mytable LIMIT 5")
}





if(FALSE) {
a = getNativeSymbolInfo("sqlite3_api", "RSQLite")$address
.llvm(m$R_setSQLite3API, a, .ee = ee)
.llvm(m$R_getSQLite3API, .ee = ee)
}
#m[["sqlite3_api"]] = a

