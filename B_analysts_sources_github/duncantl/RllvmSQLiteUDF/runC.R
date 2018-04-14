require("RSQLite") && require("RSQLiteUDF")
db = dbConnect(SQLite(), "foo")
sqliteExtension(db)


a = getNativeSymbolInfo("sqlite3_api", "RSQLiteUDF")$address

sqliteExtension(db, system.file("libs", "RSQLiteUDF.so", package = "RSQLiteUDF"))
sqliteExtension(db, "fib.so")

dyn.load("fib.so")
.Call("R_setSQLite3API", a)

p = getNativeSymbolInfo("sqlTen")
createSQLFunction(db, "sqlTen", "ten", nargs = 0L)

d = dbGetQuery(db, "SELECT ten()")


########

p = getNativeSymbolInfo("sqlFib3")
createSQLFunction(db, "sqlFib3", "fib3", nargs = 1L)


d = dbGetQuery(db, "SELECT x, fib3(x) FROM mytable")



#################
createSQLFunction(db, "myfloorFunc", "myfloor", nargs = 1L)
d = dbGetQuery(db, "SELECT myfloor(y) FROM mytable LIMIT 5")


p = getNativeSymbolInfo("sqlFib2")
createSQLFunction(db, p, "fib2", nargs = 1L)
d = dbGetQuery(db, "SELECT x, fib2(x) FROM mytable")
