dist =
function(X, Y, nx = nrow(X), ny = nrow(Y), p = ncol(X))
{
    ctr = 1L
    ans = numeric(nx * ny)
    for (i in 1:nx) {
        for (j in 1:ny) {
            posX = i
            posY = j
            total = 0.0
            for (k in 1:p) {
                total = total + (X[posX] - Y[posY])^2
                posX = posX + nx
                posY = posY + ny
            }
            ans[ctr] = sqrt(total)
            ctr = ctr + 1L
        }
    }
    ans
}


library(RLLVMCompile)
mod = Module()
declareFunction(list(VoidType, Int32Type), "printInt", mod)
llvmAddSymbol("printInt")
distc = compileFunction(dist, REALSXPType, list(DoublePtrType, DoublePtrType, Int32Type, Int32Type, Int32Type), module = mod, .integerLiterals = FALSE)


A = matrix(as.numeric(1:15), 3,5)
B = matrix(as.numeric(20:1), 4,5)

A = matrix(rnorm(15), 3,5)
B = matrix(rnorm(20), 4,5)

ee = ExecutionEngine(distc)
ans = .llvm(distc, A, B, nrow(A), nrow(B), ncol(A), .ee = ee)
ans = matrix(ans, nrow(A), nrow(B), byrow = TRUE)
true = as.matrix(stats::dist(rbind(A, B)))[ 1:nrow(A), - (1:nrow(A))]
all(ans == true)


p = 40L
n1 = 8000L
n2 = 1000L
A = matrix(rnorm(n1 * p), n1, p)
B = matrix(rnorm(n2 * p), n2, p)

lf = function(A, B, .ee = ExecutionEngine(distc)) {
   matrix(.llvm(distc, A, B, nrow(A), nrow(B), ncol(A), .ee = .ee), nrow(A), nrow(B), byrow = TRUE)
}

rm(dist)
rf = function(A, B) 
    as.matrix(dist(rbind(A, B)))[ 1:nrow(A), - (1:nrow(A)) ]


# Timing

tm.ll = system.time(a <- lf(A, B, ee))
tm.r = system.time(b <- rf(A, B))
all.equal(a, b)  # mismatch on dimnames should be only issue.

res = structure(list(llvm = tm.ll, r.dist = tm.r), 
                 session = sessionInfo(), when = Sys.time(), system = Sys.info())
id = sprintf("distance.tm.%d:%d:%d_%s", n1, n2, p, Sys.info()["sysname"])
assign(id, res, globalenv())
save(list = id, file = sprintf("%s.rda", id))

