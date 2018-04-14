volumes <- function() {
        if(Sys.info()["sysname"] == "Darwin") {
                out <- system("df", intern = TRUE)
                vols <- grep("/Volumes", out, fixed = TRUE, 
                             value = TRUE)
                if(length(vols) == 0L) 
                        return(NULL)
                else {
                        spl <- strsplit(vols, " +", perl = TRUE)
                        paths <- sapply(spl, function(x) x[length(x)])
                }
        }
        else {
                stop("unrecognized system")
        }
        paths
}

library(parallel)

par.write <- function(x, file, paths) {
        n <- length(paths)
        xs <- serialize(x, NULL)
        L <- length(xs)
        meta <- list(paths = paths, file = file)
        outpaths <- file.path(paths, file)
        byte.idx <- parallel::splitIndices(L, n)
        tryCatch({
                out <- mcmapply(function(idx, outpath) {
                        writeBin(xs[idx], outpath)
                }, byte.idx, outpaths, mc.cores = n)
        })
}