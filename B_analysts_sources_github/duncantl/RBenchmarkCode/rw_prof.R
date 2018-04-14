source("rw1.R")
n = 1e5

#fns = paste("rw2d", c(0:2, 2.5, 3:5), sep = "")
fns = list(rw2d0, rw2d1, rw2d2, rw2d2.5, rw2d3, rw2d4, rw2d5)
times = sapply(fns, function(f) {
              system.time(f(n))
            })

plot(times[3,])
# rw2d0 is ridiculous in any language, so let's omit it.
plot(times[3,-1])

# Relative to best - speedup factor
plot(times[3,-1]/min(times[3,-1]))

