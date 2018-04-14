library(XML)
doc = xmlParse("doc.xml")

library(Rllvm)

m = parseIR("fib.ll")
llvmAddSymbol("xmlXPathPopNumber", "xmlXPathNewFloat", "valuePush")

ee = ExecutionEngine(m)

fib.ptr = getPointerToFunction(m$xpathFib, ee)@ref

getNodeSet(doc, "//value[ fib(number(.)) > 10 ]", xpathFuns = list(fib = fib.ptr))
