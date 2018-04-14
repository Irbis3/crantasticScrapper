dyn.load("fib.so")
library(Rffi)
# WRONG! We need its contents
r = getNativeSymbolInfo("R_setSQLite3API")$address
b = new("externalptr")
callCIF(Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)), r, b, returnInputs = FALSE)

r = getNativeSymbolInfo("R_getSQLite3API")
callCIF(Rffi::CIF(Rffi::sexpType), r, returnInputs = FALSE)

r = getNativeSymbolInfo("R_2setSQLite3API")
callCIF(Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)), r, 10L, returnInputs = FALSE)

