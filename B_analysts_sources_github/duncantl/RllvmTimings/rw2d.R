
rw2d1 =
function(n = 100) {
    xpos = ypos = numeric(n)
    for(i in 2:n) {
          # Decide whether we are moving horizontally or vertically.
      delta = if(runif(1) > .5) 1 else -1
      if (runif(1) > .5) {
        xpos[i] = xpos[i-1] + delta
        ypos[i] = ypos[i-1]
      }
      else {
        xpos[i] = xpos[i-1]
        ypos[i] = ypos[i-1] + delta
      }
    }
    list(x = xpos, y = ypos)
}


library(Rllvm)
InitializeNativeTarget()
mod = Module("2Drw")
ptrInt = pointerType(Int32Type)
fun = Function("rw2d", VoidType, c(x = ptrInt, y = ptrInt, len = Int32Type), mod)


entry = Block(fun, "entry")
cond = Block(fun, "loopCond")
body = Block(fun, "loopBody")
ret = Block(fun, "return")
h = Block(fun, "Horizontal")
v = Block(fun, "Vertical")
increment = Block(fun, "increment")


one = createIntegerConstant(1L)
minusOne = createIntegerConstant(-1L)


ir = IRBuilder(entry)

iv = ir$createLocalVariable(Int32Type, "i")
lena = ir$createLocalVariable(Int32Type, "lenp")
xa = ir$createLocalVariable(ptrInt, "xp")
ya = ir$createLocalVariable(ptrInt, "yp")
delta = ir$createLocalVariable(Int32Type, "delta")
ir$createStore(fun$x, xa)
ir$createStore(fun$y, ya)
ir$createStore(fun$len, lena)
ir$createStore(one, iv)
ir$createBr(cond)


ir$setInsertPoint(cond)
a = ir$createLoad(iv)
b = ir$createLoad(lena)
ok = ir$createICmp(ICMP_SLT, a, b)
ir$createCondBr(ok, body, ret)


ir$setInsertPoint(ret)
ir$createRetVoid()


ir$setInsertPoint(body)
 # declare runif which takes a number but ignores it. 
runif = Function("runif", DoubleType, c(n = Int32Type), mod)

 # compute delta
u = ir$createCall(runif, one)
gt = ir$createFCmp(FCMP_UGE, u, createDoubleConstant(.5))
ir$createStore(ir$createSelect(gt, minusOne, one),  delta)

 # now determine whether to go horiz or vert.
u = ir$createCall(runif, one)
gt = ir$createFCmp(FCMP_UGE, u, createDoubleConstant(.5))

ir$createCondBr(gt, h, v)


ir$setInsertPoint(h)
a = ir$createLoad(iv)
b = ir$binOp(Sub, a, one)
r = ir$createLoad(xa)
idx = ir$createSExt(b, 64L)
x.prev = ir$createLoad(ir$createGEP(r, idx))
nw = ir$binOp(Add, x.prev, ir$createLoad(delta))
a = ir$createLoad(xa)
i = ir$createLoad(iv)
idx = ir$createSExt(i, 64L)
xi = ir$createGEP(a, idx)
ir$createStore(nw, xi)


a = ir$createLoad(iv)
b = ir$binOp(Sub, a, one)
r = ir$createLoad(ya)
idx = ir$createSExt(b, 64L)
y.prev = ir$createLoad(ir$createGEP(r, idx))
a = ir$createLoad(ya)
i = ir$createLoad(iv)
idx = ir$createSExt(i, 64L)
yi = ir$createGEP(a, idx)
ir$createStore(y.prev, yi)


ir$createBr(increment)


ir$setInsertPoint(increment)
i = ir$createLoad(iv)
inc = ir$binOp(Add, i, 1L)
ir$createStore(inc, iv)
ir$createBr(cond)


ir$setInsertPoint(v)

a = ir$createLoad(iv)
b = ir$binOp(Sub, a, one)
r = ir$createLoad(ya)
idx = ir$createSExt(b, 64L)
y.prev = ir$createLoad(ir$createGEP(r, idx))
nw = ir$binOp(Add, y.prev, ir$createLoad(delta))
a = ir$createLoad(ya)
i = ir$createLoad(iv)
idx = ir$createSExt(i, 64L)
yi = ir$createGEP(a, idx)
ir$createStore(nw, yi)


a = ir$createLoad(iv)
b = ir$binOp(Sub, a, one)
r = ir$createLoad(xa)
idx = ir$createSExt(b, 64L)
x.prev = ir$createLoad(ir$createGEP(r, idx))
a = ir$createLoad(xa)
i = ir$createLoad(iv)
idx = ir$createSExt(i, 64L)
xi = ir$createGEP(a, idx)
ir$createStore(x.prev, xi)



ir$createBr(increment)


ee = ExecutionEngine(mod)
Optimize(mod, ee)
llvmAddSymbol(runif = getNativeSymbolInfo("runif")$address)


n = 1e7


interp = system.time({rw2d1(n)})
#interp = replicate(2, system.time({rw2d1(n)}))
#interp = apply(interp, 1, median)


doit = 
function(n = 1000000) {
 x = integer(n)
 y = integer(n)
 .llvm(fun, x = x, y = y, n, .ee = ee, .all = TRUE)[c("x", "y")]
}

tt = system.time({ doit(n)})
tt = system.time({ doit(n)}) # second time runs faster


tt = replicate(10, system.time({ doit(n)}))
tt = apply(tt, 1, median)


library(compiler)
g = cmpfun(rw2d1)
cmp = replicate(3, system.time(g(n)))
cmp = apply(cmp, 1, median)
interp/cmp


ross = "rw1.R"
source(ross)
fastInterp = replicate(10, system.time({rw2d5(n)}))
fastInterp = apply(fastInterp, 1, median)
fastInterp/interp


i = 3
tmp = c(interp[i], cmp[i], fastInterp[i], tt[i])
m = matrix(c(tmp, interp[i]/tmp), length(tmp), ,
        dimnames = list(c("Interpreted", "Byte Compiled", "Vectorized", "Rllvm"), c("Time", "Speedup")))
m




res = structure(m, session = sessionInfo(), system = Sys.info(), when = Sys.time(), n = n)
id = sprintf("rw2d.tm.1e7_%s", Sys.info()["sysname"])
assign(id, res, globalenv())
#save( list = id, file = sprintf("%s.rda", id))

