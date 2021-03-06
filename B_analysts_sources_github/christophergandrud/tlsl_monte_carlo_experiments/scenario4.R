# TLSL Scenario 4 --------------------------------------------------------------
simpleSetup::library_install(pkgs)
theme_set(theme_bw())

s4_under_list <- list()
s4_over_list <- list()
set.seed(seed)

for (u in 1:nsims) {
    tu <- t_per_indiv + 1
    comb <- data.frame()
    for (n in 1:n_indiv) {
        epsilon <- rnorm(tu, 0, 1)
        x1 <- sample(x = c(0, 1), size = tu, replace = TRUE)
        x2 <- rnorm(n = tu, 0, 1)

        y <- numeric(length(epsilon))
        yinit <- rnorm(1, 0, 1)

        for(l in 1:length(y)){
            if(l==1) y[l] <- alpha + b1*x1[l] + b2*x2[l] + phi * yinit + epsilon[l]
            if(l > 1) y[l] <- alpha + b1*x1[l] + b2*x2[l] + phi * y[l-1] + epsilon[l]
        }
        temp <- data.frame(id = n, t = 1:t_per_indiv, y = y[-1],
                           x1 = x1[-1], x2 = x2[-1],
                           ytm1 = y[-length(y)])
        comb <- rbind(comb, temp)
    }
    location_df <- location_builder_continuous(n_indiv = n_indiv,
                                                t_per_indiv = t_per_indiv)
    comb <- cbind(comb, location = location_df$location)

    # Create global monadic spatial weight
    sw <- spatialWeights::monadic_spatial_weights(
                            comb, id_var = 'id', time_var = 't',
                            y_var = 'y', location_var = 'location',
                            weight_name = 'wy', mc_cores = num_cores)
    sw$t <- as.integer(sw$t)
    # Lag weight
    sw <- sw %>% arrange(id, t) %>% group_by(id) %>%
                mutate(lag_wy = dplyr::lag(wy, order_by = id))

    sw <- merge(sw, comb)

    # burn in
    sw <- subset(sw, t != 1:burnin)

    # Estimate models
    s4_under <- lm(y ~ x1 + x2 + lag_wy, data = sw)

    s4_over <- lm(y ~ x1 + x2 + ytm1 + lag_wy, data = sw)

    # Save estimates
    s4_under_list <- results_combiner(s4_under_list, s4_under)
    s4_over_list <- results_combiner(s4_over_list, s4_over)
}

# Find the root mean squared error for DGP variable coefficients
s4_under_list[['rmse']] <- rmse(s4_under_list, c('x1'), 'b1', b1)
s4_over_list[['rmse']] <- rmse(s4_over_list, c('x1', 'x2', 'ytm1'),
                             c('b1', 'b2', 'phi'), c(b1, b2, phi))

# Save simulations -------------------------------------------------------------
save(s4_over_list, s4_under_list, file = 'mc_results/scenario4.rda')
