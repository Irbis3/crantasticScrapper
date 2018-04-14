
# Tradeoff Functions 
# 1. sJ.from.m - generates juvenile survival rates with a tradeoff based on maturation rates.
# 2. dem.model - creates a matrix of values and solves for the eigenvalue (and returns the max eigenvalue).
# 3. tradeoff  - Calculates a basic tradeoff based on a simple matrix case.
# 4. do_tradeoff - a wrapper for tradeoff (prepares the data first), runs it through 3. and then compiles the results.
# 5. run_vdm - 
# 6. run_vdm_jg - 
# 7. run_vdm_corr - 
# 8. vd_tradeoff_basic - prepares data, calls run_vdm, and compiles results (with simulation ids)
# 9. vd_tradeoff_jg - prepares data, calls run_vdm_jg, and compiles results (with simulation ids)
# 10. vd_tradeoff_corr - prepares and calls run_vdm_corr and compiles results with parameter combinations (and simulation ids)
# 11. do_vd_tradeoff - wrapper for functions 8-10. Which one is called depends in list length.
# 12. validity - Check to make sure combinations do not result in biologically unrealisitc assumptions (e.g. negative mortality).
# 13. param_check - Checks to make sure there are no duplicates in generated parameter combinations
# 14. param_combs - Generates all combinations of parameters for basic tradeoff
# 15. param_combs_jg - Generates all combinations of parameters for basic tradeoff but with added juvenile var
# 16. param_combs_corr - Generates all combinations of parameters for basic tradeoff + juv var + correlation between stages.
# 17. arg_max - calculates arg_max for any tradeoff curve by fitting a spline.
# 18. tradeoff.plot - 
# 19. do_tradeoff.plot - wrapper for tradeoff.plot
# 20. assemble_plots - assembles all ggplots specified in argument list.
# 21. generate_filename - Generates a filename by appending a timestamp


# This is to increase the batch size for runs to lower simulation noise (I might ignore these and stick with a simpler version of the model)
juvenile <- c(20, 0, 0, 0, 200, "FALSE")
adult <- c(20, 0, 0, 0, 200, "FALSE")
controlz <- data.frame(rbind(juvenile, adult))
names(controlz) <- c("batch.size", "alive.target", "dead.target", "max.dead", "max.total", "ragged.save")

# ---------------------------------------------------
# Generate juvenile survival from maturity rate given a certain tradeoff [works fine]
sJ.from.m <- function(m, a, b) {
    return(a + b * m)
}

# ---------------------------------------------------
# Calculate lamba from a basic matrix model

dem.model <- function(m, sJ, sA, Fec) {
    # message(sprintf("%s, %s, %s, %s", m, sJ,sA, Fec))
    mat <- matrix(c((1 - m) * sJ, m * sJ, Fec, sA), nrow = 2)
    return(max(eigen(mat)$values))
}

 # ---------------------------------------------------
 # Imposing the tradeoff for a simple matrix case

tradeoff <- function(a, b, sA, Fec, m = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length = 20)

    sJ <- sJ.from.m(m, a, b)
    df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec))
    lambda <- apply(df, 1, function(x) dem.model(x[1], x[2], x[4], x[3]))
    type <- "simple"
    return((data.frame(m, sJ, lambda, cv = 1, type)))
}


# =------------------------------------------
# Running a simple tradeoff.

do_tradeoff <- function(tlist) {
    # Run each combination in tlist through tradeoff and return only the results of valid runs
    toff <- tryCatch(tradeoff(tlist$a, tlist$b, tlist$Fec, tlist$sA, m = NULL), error=function(e) NULL)
    # Write the results and the parameters used into a list.
    basic_result <- list(data = toff, params = tlist)
    return(basic_result)
}

# ---------------------------------------------------
# Running a vd model but with no variation in juvenile development or correlation among stages. Results should be identical to a matrix model case (with a small amount of simulation error)
run_vdm <- function(m, sJ, sA, Fec) {
    
    vdmodel <- suppressMessages(VD.model(num.stages = 2, marginal.durations = list(VD.dist("geomp1",
        list(prob = m)), VD.dist("geomp1", list(prob = (1 - sA)))), marginal.death.times = list(VD.dist("geomp1",
        list(prob = (1 - sJ))), VD.dist("infinite")), fecundity = Fec, controls=data.frame(batch.size = 10000, alive.target = 20000, dead.target =  c(rep(10000, 1), 0), max.dead = 20000, max.total = 250000, ragged.save = TRUE)))

     # vdmodel <- (VD.model(2, marginal.durations = list(VD.dist("gamma",
     #    list(shape = juvshape, scale = juvscale)), VD.dist("geomp1", list(prob = (1 -
     #    sA)))), marginal.death.times = list(VD.dist("geomp1", list(prob = (1 - sJ))),
     #    VD.dist("infinite")), fecundity = Fec))
    # run vardev model
    VDS <- VD.run(vdmodel)
    # compile the results. See ?compile.dev.table for more information
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = Fec)
    r <- VD.solve.euler(mean.fec)
    return(exp(r))
}  # end run_vdm
# -----------------------------------------
# Running the model by adding compexity that juvenile development follows a gamma. We vary the cv on a scale of 0 to 1.
run_vdm_jg <- function(m, sJ, sA, Fec, juvshape) {

    lambdaJ <- -log(1 - m)
    # probability of not maturing for one time step is exp(-lambdaJ)
    meanjuv <- 1/lambdaJ
    # mean of the exponential
    # we need the scale parameter. mean <- shape * scale. 
    # sd <- sqrt(shape) * scale [see ?dgamma.]  so:

    juvscale <- meanjuv/juvshape
    vdmodel <- (VD.model(2, marginal.durations = list(VD.dist("gamma",
        list(shape = juvshape, scale = juvscale)), VD.dist("geomp1", list(prob = (1 -
        sA)))), marginal.death.times = list(VD.dist("geomp1", list(prob = (1 - sJ))),
        VD.dist("infinite")), fecundity = Fec, controls=data.frame(batch.size = 10000, alive.target = 20000, dead.target =  c(rep(10000, 1), 0), max.dead = 20000, max.total = 250000, ragged.save = TRUE)))
    VDS <- VD.run(vdmodel)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = Fec)
    r <- VD.solve.euler(mean.fec)
    return(exp(r))
}  # end run_vdm_jg


# -----------------------------------------
# Running the model with the added complexity of correlation
run_vdm_corr <- function(m, sJ, sA, Fec, juvshape, corr) {

    my.gauss.cov <- matrix(c(1, corr, corr, 1), nrow = 2)
    lambdaJ <- -log(1 - m)
    # prob of not maturing for one time step is exp(-lambdaJ)
    meanjuv <- 1/lambdaJ
    ## mean of the exponential
    # we need the scale parameter. mean = shape * scale.
    # sd = sqrt(shape) * scale. [see ?dgamma.]  so:
    juvscale <- meanjuv/juvshape
    vdmodel <- suppressMessages(VD.model(2, marginal.durations = list(VD.dist("gamma",
        list(shape = juvshape, scale = juvscale)), VD.dist("geomp1", list(prob = (1 - sA)))), marginal.death.times = list(VD.dist("geomp1",
        list(prob = (1 - sJ))), VD.dist("infinite")), gauss.cov = my.gauss.cov, fecundity = Fec, controls=data.frame(batch.size = 10000, alive.target = 20000, dead.target =  c(rep(10000, 1), 0), max.dead = 20000, max.total = 250000, ragged.save = TRUE)))
    VDS <- VD.run(vdmodel)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = Fec)
    r <- VD.solve.euler(mean.fec)
    return(exp(r))
}  # end run_vdm_corr
# ---------------------------------------------------

# Function is a wrapper for do_tradeoff
vd_tradeoff_basic <- function(a, b, sA, Fec, m = NULL, corr = NULL, juvshape = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length = 20)

        sJ <- sJ.from.m(m, a, b)
        df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec))
        lambda <- apply(df, 1, function(x) run_vdm(x[1], x[2], x[3], x[4]))
        df <- data.frame(m, sJ, lambda, cv=1)
        df$type <- "vd_tradeoff"
        return(df)
    }

# Function is a wrapper for run_vdm_jg
vd_tradeoff_jg <- function(a, b, sA, Fec, m = NULL, corr = NULL, juvshape = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length = 20)

        sJ <- sJ.from.m(m, a, b)
        df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec, juvshape = juvshape))
        lambda <- apply(df, 1, function(x) run_vdm_jg(x[1], x[2], x[3], x[4], x[5]))
        cv <- 1/sqrt(juvshape)
        df <- data.frame(m, sJ, lambda, cv)
        df$type <- "juvshape"
        return(df)
    }

# Function is a wrapper for run_vdm_corr
vd_tradeoff_corr <- function(a, b, sA, Fec, m = NULL, corr = NULL, juvshape = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length = 20)

        sJ <- sJ.from.m(m, a, b)
        df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec, juvshape = juvshape, corr = corr))
        lambda <- apply(df, 1, function(x) run_vdm_corr(x[1], x[2], x[3], x[4], x[5], x[6]))
           cv <- 1/sqrt(juvshape)
        df <- data.frame(m, sJ, lambda, cv)
        df$type <- "corr"
        return(df)
    }



# =------------------------------------------
# The wrapper for the above 3 functions above (vd_tradeoff_basic, vd_tradeoff_jg, vd_tradeoff_corr). The right function to call is determined by the number and type of arguments.


do_vd_tradeoff <- function(tlist) {
    if (length(tlist) == 5) {
        message('running run_vdm()....\n')
       vd_toff <- tryCatch(expr = evalWithTimeout(vd_tradeoff_basic(tlist$a, tlist$b, tlist$sA, tlist$Fec, m = NULL) , timeout = 80),
             TimeoutException = function(ex) "TimedOut")
}

    if (length(tlist) == 6) {
        message(sprintf('running run_vdm_jg: %s\n', tlist$sim_id))
         vd_toff <- suppressWarnings(tryCatch(expr = evalWithTimeout(vd_tradeoff_jg(tlist$a, tlist$b, tlist$sA, tlist$Fec, juvshape = tlist$juvshape,
            m = NULL) , timeout = 80),
             TimeoutException = function(ex) "TimedOut"))
        }

    if (length(tlist) == 7) {
        message(sprintf('running run_vdm_corr: %s\n', tlist$sim_id))
              vd_toff <- suppressWarnings(tryCatch(expr = evalWithTimeout(vd_tradeoff_corr(tlist$a, tlist$b, tlist$sA, tlist$Fec, juvshape = tlist$juvshape,
            tlist$corr, m = NULL) , timeout = 80),
             TimeoutException = function(ex) "TimedOut"))
        }

    vd_result <- list(data = vd_toff, params = tlist)
    return(vd_result)
}
# =------------------------------------------
# checks for tradeoff combinations that might result in unrealistic mortality rates.
validity <- function(aa, bb) {
    mm <- seq(0.01, 0.99, length = 20)
    sJ <- sJ.from.m(mm, aa, bb)
    if (min(sJ) < 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
# After checking for invalid tradeoffs, this function removes duplicates.
param_check <- function(ax, bx) {
    para2 <- expand.grid(ax, bx)
    para2$rr <- apply(para2, 1, function(x) validity(x[1], x[2]))
    clear <- as.data.table(para2[which(para2$rr), ])
    res <- data.frame(unique(clear[, list(Var1, Var2)]))
    final <- list(a = res$Var1, b = res$Var2)
    return(final)
}

# Generates a list of tradeoff combinations that allow all combinations to be pushed through any tradeoff model. First one generates the simple case with no variation in juvenile growth or correlation among stages.
param_combs <- function(a, b, sA, Fec) {
    valid <- param_check(a, b)
    base <- data.frame(a = valid[[1]], b = valid[[2]])
    others <- expand.grid(sA = sA, Fec = Fec)
    parameters <- ddply(others, .(sA, Fec), function(x) data.frame(base$a, base$b,
        sA = rep(x$sA, dim(base)[1]), Fec = rep(x$Fec, dim(base)[1])))
    parameters <- parameters[!duplicated(parameters), ]
    parameters$sim_id <- paste0("S", 1:dim(parameters)[1])
    params <- mapply(list, a = parameters[, 1], b = parameters[, 2], sA = parameters[,
        3], Fec = parameters[, 4], sim_id = parameters[, 5], SIMPLIFY = F)
    return(params)
}


param_combs_jg <- function(a, b, sA, Fec, juvshape) {
    valid <- param_check(a, b)
    base <- data.frame(a = valid[[1]], b = valid[[2]])
    others <- expand.grid(sA = sA, Fec = Fec, juvshape = juvshape)
    parameters <- ddply(others, .(sA, Fec, juvshape), function(x) data.frame(base$a, base$b,
        sA = rep(x$sA, dim(base)[1]), Fec = rep(x$Fec, dim(base)[1]), juvshape = rep(x$juvshape,
            dim(base)[1])))
    parameters <- parameters[!duplicated(parameters), ]
    parameters$sim_id <- paste0("JG", 1:dim(parameters)[1])
    params <- mapply(list, a = parameters[, 1], b = parameters[, 2], sA = parameters[,
        3], Fec = parameters[, 4], juvshape = parameters[, 5], sim_id = parameters[,
        6], SIMPLIFY = F)
    return(params)
}


param_combs_corr <- function(a, b, sA, Fec, juvshape, corr) {
    valid <- param_check(a, b)
    base <- data.frame(a = valid[[1]], b = valid[[2]])
    others <- expand.grid(sA = sA, Fec = Fec, juvshape = juvshape, corr = corr)
    parameters <- ddply(others, .(sA, Fec, juvshape, corr), function(x) data.frame(base$a, base$b,
        sA = rep(x$sA, dim(base)[1]), Fec = rep(x$Fec, dim(base)[1]), juvshape = rep(x$juvshape, dim(base)[1]), corr = rep(x$corr,
            dim(base)[1])))
    parameters <- parameters[!duplicated(parameters), ]
    parameters$sim_id <- paste0("CO", 1:dim(parameters)[1])
    params <- mapply(list, a = parameters[, 1], b = parameters[, 2], sA = parameters[,
        3], Fec = parameters[, 4], juvshape = parameters[,5], corr = parameters[, 6], sim_id = parameters[,
        7], SIMPLIFY = F)
    return(params)
}

# ---------------------------------------
# A simple spline to smooth simulated points and find the max.
# Increased the n if m from around 100, to 10k. Hopefully this should get me more resolution on the issue.
arg_max <- function(data) {
m <- data$m
lambda <- data$lambda
m1 <- mgcv::gam(lambda ~ s(m, fx = FALSE, k=-1, bs = "cr"))
predm <- predict(m1, data.frame(m = seq(0.01, 0.99, length = 10000)), se=TRUE)$fit
pm <- seq(0.01, 0.99, length = 100000)
maxy <- max(predm)
maxx <- pm[which(predm == max(predm))]
return(list(maxx, maxy))
}
# =------------------------------------------
# The below 3 functions are for plotting.

tradeoff.plot <- function(data, ptitle = "") {
          # Make colors red, blue, and gold.
          # make shapes also 3 different kinds.
    if (length(unique(data$type)) > 3)
        stop("Function not set up to deal with more than 3 categories at the moment")
    base_colours <- c("#8a0033", "#68bac2", "#006d44")
    colours <- base_colours[1:length(unique(data$type))]
    tplot <- ggplot(data, aes(m, lambda, colour = type)) + geom_point(size = 2.8,
        shape = 16) + ggtitle(ptitle) + scale_color_manual(values = colours)
    return(tplot)
}

# This combines the plots.
do_tradeoff.plot <- function(p1, p2) {
    data <- rbind(p1$data, p2$data)
    params <- ldply(c(p1$params,p2$params), data.frame)
    a <- unique(params$a)
    b <- unique(params$b)
    sA <- unique(params$b)
    main_t <- sprintf("a=%s, b=%s, sA=%s", a,b,sA)
    return(cplot <- tradeoff.plot(data, main_t))
}


# ------------------------------------------------------------------
# assembles plots in the same fig.
assemble_plots <- function(dat) {
 s1 <- as.numeric(dat$sim_id)
 s2 <- as.numeric(dat$sim_juv)
 s3 <- as.numeric(dat$sim_cor)
 title  <- sprintf("a:%s, b:%s, sA:%s, Fec:%s, juvshape:%s, corr:%s",dat$a, dat$b, dat$sA, dat$Fec, dat$juvshape, dat$corr)
 plot_data <- rbind(t1_simple[[s1]]$data, t1_juvshape[[s2]]$data, t1_corr[[s3]]$data)
 tradeoff.plot(plot_data, title)
}


# ------------------------------------------------------------------
# generates a unique filename with timestamp
generate_filename <- function(name) {
filename <- paste(name, "_", str_replace_all(now()," ","_"), ".rdata", sep="")
filename <- str_replace_all(filename, ":", "_")
return (filename)
}

