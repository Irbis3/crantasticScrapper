library(Rffi)
dyn.load("cif.so")
r = getNativeSymbolInfo("R_foo")
callCIF(Rffi::CIF(Rffi::sexpType), r)


r = getNativeSymbolInfo("R_nil")
callCIF(Rffi::CIF(Rffi::sexpType), r)

r = getNativeSymbolInfo("R_bar")
callCIF(Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)), r, 4L)

r = getNativeSymbolInfo("R_nilPtr")
callCIF(Rffi::CIF(Rffi::sexpType, list(Rffi::sexpType)), r, new("externalptr"))
