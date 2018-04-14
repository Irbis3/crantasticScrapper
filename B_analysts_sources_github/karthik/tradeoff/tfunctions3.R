# Tradeoff Functions - tradeoff 3
# Tradeoff between fecundity and adult survival.
# ---------------------------------------------------
# Generate juvenile survival from maturity rate given a certain tradeoff
fec.from.m <- function( a, b, m) {
    Fec <- a + b*m
    return (100*Fec)
}

# ---------------------------------------------------
# Calculate lamba from a basic matrix model
dem.model <- function(m, Fec, sA, sJ) {
    # message(sprintf("%s, %s, %s, %s", m, Fec,sA, sJ))
    mat <- matrix(c((1 - m) * sJ, m * sJ, Fec, sA), nrow = 2)
    return(max(eigen(mat)$values))
}

 # ---------------------------------------------------
 # Imposing the tradeoff for a simple matrix case
tradeoff <- function(a, b, sAa, sJa, m = NULL, Fec = NULL) {
    if (is.null(m))
        m <- seq(0.01, .99, length=20)

        Fec <- fec.from.m(a, b, m)
        df <- as.matrix(data.frame(m = m, Fec = Fec, sJ = rep(sJa, length(m)), sA = rep(sAa, length(m))))
    lambda <- apply(df, 1, function(x) dem.model(x[1], x[2], x[4], x[3]))
    type <- "simple"
    return((data.frame(m, Fec, lambda, cv = 1, type)))
}


# =------------------------------------------
# Running a simple tradeoff.
do_tradeoff <- function(tlist) {
    toff <- tryCatch(tradeoff(tlist$a, tlist$b, tlist$sA, tlist$sJ, m = NULL, Fec = NULL), error=function(e) NULL)
    basic_result <- list(data = toff, params = tlist)
    return(basic_result)
}

# ---------------------------------------------------
# Running a vd model but with no variation in juvenile development or correlation among stages. Results should be identical to a matrix model case (with a small amount of simulation error)
run_vdm <- function(m, sJ, sA, Fec) {
    # message(sprintf("%s %s %s %s", m, sJ,sA,Fec))
    vdmodel <- suppressMessages(VD.model(num.stages = 2, marginal.durations = list(VD.dist("geomp1",
        list(prob = m)), VD.dist("geomp1", list(prob = (1 - sA)))), marginal.death.times = list(VD.dist("geomp1",
        list(prob = (1 - sJ))), VD.dist("infinite")), fecundity = Fec))
    VDS <- VD.run(vdmodel)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = Fec)
    r <- VD.solve.euler(mean.fec)
    return(exp(r))
}  # end run_vdm
# -----------------------------------------
# Running the model by adding compexity that juvenile development follows a gamma. We vary the cv on a scale of 0 - 1.
run_vdm_jg <- function(m, sJ, sA, Fec, juvshape) {

    lambdaJ <- -log(1 - m)
    # prob of not maturing for one time step is exp(-lambdaJ)
    meanjuv <- 1/lambdaJ
    # mean of the exponential
    # we need the scale parameter. mean = shape * scale. sd = sqrt(shape) * scale. see ?dgamma.  so:

    juvscale <- meanjuv/juvshape
    vdmodel <- (VD.model(2, marginal.durations = list(VD.dist("gamma",
        list(shape = juvshape, scale = juvscale)), VD.dist("geomp1", list(prob = (1 -
        sA)))), marginal.death.times = list(VD.dist("geomp1", list(prob = (1 - sJ))),
        VD.dist("infinite")), fecundity = Fec))
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
    ## prob of not maturing for one time step is exp(-lambdaJ)
    meanjuv <- 1/lambdaJ
    ## mean of the exponential
    # we need the scale parameter. mean = shape * scale.
    # sd = sqrt(shape) * scale. see ?dgamma.  so:
    juvscale <- meanjuv/juvshape
    vdmodel <- suppressMessages(VD.model(2, marginal.durations = list(VD.dist("gamma",
        list(shape = juvshape, scale = juvscale)), VD.dist("geomp1", list(prob = (1 - sA)))), marginal.death.times = list(VD.dist("geomp1",
        list(prob = (1 - sJ))), VD.dist("infinite")), gauss.cov = my.gauss.cov, fecundity = Fec))
    VDS <- VD.run(vdmodel)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = Fec)
    r <- VD.solve.euler(mean.fec)
    return(exp(r))
}  # end run_vdm_corr
# ---------------------------------------------------


vd_tradeoff_basic <- function(a, b, sA, Fec, m = NULL, corr = NULL, juvshape = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length=20)

        Fec <- fec.from.m(a, b, m)
        df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec))
        lambda <- apply(df, 1, function(x) run_vdm(x[1], x[2], x[3], x[4]))
        df <- data.frame(m, Fec, lambda, cv=1)
        df$type <- "vd_tradeoff"
        return(df)
    }


vd_tradeoff_jg <- function(a, b, sA, Fec, m = NULL, corr = NULL, juvshape = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length=20)

        Fec <- fec.from.m(a, b, m)
        df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec, juvshape = juvshape))
        lambda <- apply(df, 1, function(x) run_vdm_jg(x[1], x[2], x[3], x[4], x[5]))
        cv <- 1/sqrt(juvshape)
        df <- data.frame(m, Fec, lambda, cv)
        df$type <- "juvshape"
        return(df)
    }


vd_tradeoff_corr <- function(a, b, sA, Fec, m = NULL, corr = NULL, juvshape = NULL) {
    if (is.null(m))
        m <- seq(0.01, 0.99, length=20)

        Fec <- fec.from.m(a, b, m)
        df <- as.matrix(data.frame(m = m, sJ = sJ, sA = sA, Fec = Fec, juvshape = juvshape, corr = corr))
        lambda <- apply(df, 1, function(x) run_vdm_corr(x[1], x[2], x[3], x[4], x[5], x[6]))
           cv <- 1/sqrt(juvshape)
        df <- data.frame(m, Fec, lambda, cv)
        df$type <- "corr"
        return(df)
    }



# =------------------------------------------
# The wrapper for the above 3 functions. The right function to call is determined by the number and type of arguments.


do_vd_tradeoff <- function(tlist) {
    if (length(tlist) == 5) {
        message('running run_vdm()....\n')
        safe_vd_basic <- failwith(NULL, vd_tradeoff_basic, quiet = TRUE)
       # vd_toff <- tryCatch(expr = evalWithTimeout(vd_tradeoff_basic(tlist$a, tlist$b, tlist$sA, tlist$sJ, m = NULL) , timeout = 40),
       #       TimeoutException = function(ex) "TimedOut")
       vd_toff <- safe_vd_basic(tlist$a, tlist$b, tlist$sA, tlist$sJ, m = NULL)
}

    if (length(tlist) == 6) {
        message(sprintf('running run_vdm_jg: %s\n', tlist$sim_id))
        # safe_vd_jg <- failwith(NULL, vd_tradeoff_jg, quiet = TRUE)
         vd_toff <- suppressWarnings(tryCatch(expr = evalWithTimeout(vd_tradeoff_jg(tlist$a, tlist$b, tlist$sA, tlist$sJ, juvshape = tlist$juvshape,
            m = NULL) , timeout = 40),
             TimeoutException = function(ex) "TimedOut"))
# vd_toff <- safe_vd_jg(tlist$a, tlist$b, tlist$sA, tlist$sJ, juvshape = tlist$juvshape, m = NULL)
        }

    if (length(tlist) == 7) {
        message(sprintf('running run_vdm_corr: %s\n', tlist$sim_id))
         # safe_vd_corr <- failwith(NULL, vd_tradeoff_corr, quiet = TRUE)
              vd_toff <- suppressWarnings(tryCatch(expr = evalWithTimeout(vd_tradeoff_corr(tlist$a, tlist$b, tlist$sA, tlist$sJ, juvshape = tlist$juvshape,
            tlist$corr, m = NULL) , timeout = 40),
             TimeoutException = function(ex) "TimedOut"))
    # vd_toff <- safe_vd_corr(tlist$a, tlist$b, tlist$sA, tlist$sJ, juvshape = tlist$juvshape, tlist$corr, m = NULL)   
        }

    vd_result <- list(data = vd_toff, params = tlist)
    return(vd_result)
}
# =------------------------------------------
# checks for tradeoff combinations that might result in unrealistic mortality rates.
validity <- function(aa, bb) {
         m <- seq(0.01, .99, length=20)
        Fec <- fec.from.m(a, b, m)
    
    if (min(Fec) <= 0) {
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
param_combs <- function(a, b, sA, sJ) {
    valid <- param_check(a, b)
    base <- data.frame(a = valid[[1]], b = valid[[2]])
    others <- expand.grid(sA=sA, sJ=sJ)
    parameters <- ddply(others, .(sA, sJ), function(x) data.frame(base$a, base$b, sA = rep(x$sA, dim(base)[1]), sJ = rep(x$sJ, dim(base)[1])))
    parameters <- parameters[!duplicated(parameters), ]
    parameters$sim_id <- paste0("S", 1:dim(parameters)[1])
    params <- mapply(list, a = parameters[, 1], b = parameters[, 2], sA = parameters[,
        3],sJ = parameters[,
        3], sim_id = parameters[, 5], SIMPLIFY = F)
    return(params)
}


param_combs_jg <- function(a, b, sA, sJ, juvshape) {
    valid <- param_check(a, b)
    base <- data.frame(a = valid[[1]], b = valid[[2]])
    others <- expand.grid(sA = sA, sJ = sJ, juvshape = juvshape)
    parameters <- ddply(others, .(sA, sJ, juvshape), function(x) data.frame(base$a, base$b,
        sA = rep(x$sA, dim(base)[1]), sJ = rep(x$sJ, dim(base)[1]), juvshape = rep(x$juvshape,
            dim(base)[1])))
    parameters <- parameters[!duplicated(parameters), ]
    parameters$sim_id <- paste0("JG", 1:dim(parameters)[1])
    params <- mapply(list, a = parameters[, 1], b = parameters[, 2], sA = parameters[,
        3], sJ = parameters[, 4], juvshape = parameters[, 5], sim_id = parameters[,
        6], SIMPLIFY = F)
    return(params)
}


param_combs_corr <- function(a, b, sA, sJ, juvshape, corr) {
    valid <- param_check(a, b)
    base <- data.frame(a = valid[[1]], b = valid[[2]])
    others <- expand.grid(sA = sA, sJ = sJ, juvshape = juvshape, corr = corr)
    parameters <- ddply(others, .(sA, sJ, juvshape, corr), function(x) data.frame(base$a, base$b,
        sA = rep(x$sA, dim(base)[1]), sJ = rep(x$sJ, dim(base)[1]), juvshape = rep(x$juvshape, dim(base)[1]), corr = rep(x$corr,
            dim(base)[1])))
    parameters <- parameters[!duplicated(parameters), ]
    parameters$sim_id <- paste0("CO", 1:dim(parameters)[1])
    params <- mapply(list, a = parameters[, 1], b = parameters[, 2], sA = parameters[,
        3], sJ = parameters[, 4], juvshape = parameters[,5], corr = parameters[, 6], sim_id = parameters[,
        7], SIMPLIFY = F)
    return(params)
}

# ---------------------------------------
# A simple spline to smooth simulated points and find the max.
arg_max <- function(data) {
m <- data$m
lambda <- data$lambda
m1 <- mgcv::gam(lambda ~ s(m, fx = FALSE, k=-1, bs = "cr"))
predm <- predict(m1, data.frame(m = seq(0.01, 0.99, by = 0.1)), se=TRUE)$fit
pm <- seq(0.01, 0.99, by = 0.1)
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

# This combines the plots
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

