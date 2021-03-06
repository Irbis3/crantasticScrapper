# ------------------------------------------------------------------------------
# Plot MC Experiment Results
# Christopher Gandrud
# MIT License
# ------------------------------------------------------------------------------

library(xtable)
library(ggplot2)
theme_set(theme_bw())

# Load MC results
rm_lists <- (Filter( function(x) 'list' %in% class( get(x) ), ls() ))
rm(rm_lists)
results_files <- list.files('mc_results')
lapply(sprintf('mc_results/%s', results_files), load, .GlobalEnv)
rm(results_files)
all_results <- Filter( function(x) 'list' %in% class( get(x) ), ls() )
no_range_results <- all_results[!(all_results %in%
                                 c('s2_phi_range_over_list',
                                   's2_phi_range_under_list',
                                   's3_theta_wz_range_over_list',
                                   's3_theta_wz_range_under_list'))]

# False discovery rate formula
fdr_fun <- function(x) (sum(x < 0.05)/length(x))

# Plot p-values for TLSL -------------------------------------------------------
pvalues_df <- data.frame()
for (i in no_range_results) {
    message(i)
    ptemp <- extract_element(eval(parse(text=paste(i))), 'pvalue', 'lag_wy')
    ptemp$scenario <- i
    pvalues_df <- rbind(pvalues_df, ptemp)
}

pvalues_df$under_l <- grepl('under', pvalues_df$scenario)

pvalues_df$Type[pvalues_df$under_l] <- "Under-estimated"
pvalues_df$Type[pvalues_df$under_l == FALSE] <- "Over-estimated"

p_labels <- c('Scenario 1 (over)', 'Scenario 1 (under)',
              'Scenario 2 (over)', 'Scenario 2 (under)',
              'Scenario 3 (over)', 'Scenario 3 (under)',
              'Scenario 4 (over)', 'Scenario 4 (under)',
              'Scenario 5 (over)', 'Scenario 5 (under)')
pvalues_df$scenario <- factor(pvalues_df$scenario, labels = p_labels)

#ggplot(pvalues_df, aes(scenario, value, group = scenario,
#                       color = Type)) +
#    geom_boxplot() +
#    geom_point(alpha = 0.2, position = 'jitter') +
#    geom_hline(yintercept = 0.05, linetype = 'dashed', color = 'red', size = 1) +
#    geom_hline(yintercept = 0.1, linetype = 'dotted', color = 'red', size = 1) +
#    scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.5, 1), limits = c(0, 1)) +
#    scale_color_manual(values =  c('#bdbdbd', '#636363')) +
#    coord_flip() +
#    ylab('p-values for temporally-lagged spatial lag') + xlab('')

# ggsave('mc_figures/mc_lagwy_pvalues.pdf', height = 12, width = 10)

no_range_fdr <- pvalues_df %>% group_by(scenario) %>%
    summarise(fdr = fdr_fun(value))

is.even <- function(x) x %% 2 == 0

under <- no_range_fdr[is.even(1:nrow(no_range_fdr)), ]
over <- no_range_fdr[!is.even(1:nrow(no_range_fdr)), ]

no_range_fdr <- data.frame(Scenario = 1:5, Under = under[, 2],
                           Over = over[, 2])
names(no_range_fdr) <- c('Scenario No.', 'FDR (under)', 'FDR (over)')

print(xtable(no_range_fdr), file = 'mc_tables/no_range_fdr.tex',
      floating = FALSE, row.names = FALSE)


# Scenario 2 (omitted autoregressive covariate) ----- --------------------------
# Coefficient Bias
load('mc_results/scenario2_range_phi.rda')
rmse_s2_range_df <- data.frame()
for (i in names(s2_phi_range_under_list)) {
    message(i)
    rmse_temp <- s2_phi_range_under_list[[i]][['rmse']]
    rmse_temp <- cbind(rmse_temp, data.frame(phi = as.numeric(i),
                                             type = 'Scenario 2 under estimate'))
    rmse_s2_range_df <- rbind(rmse_s2_range_df, rmse_temp)
}

for (i in names(s2_phi_range_over_list)) {
    message(i)
    rmse_temp <- s2_phi_range_over_list[[i]][['rmse']]
    rmse_temp <- cbind(rmse_temp, data.frame(phi = as.numeric(i),
                                             type = 'Scenario 2 over estimate'))
    rmse_s2_range_df <- rbind(rmse_s2_range_df, rmse_temp)
}

rmse_s2_range <- ggplot(rmse_s2_range_df, aes(phi, rmse, group = variable, 
                                              linetype = variable)) +
    facet_wrap(~type) +
    geom_line() +
    scale_linetype(name = "", labels = expression(hat(beta)[1], hat(beta)[2])) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
#    scale_y_continuous(limits = c(0, 2.5)) +
    scale_x_continuous(breaks = as.numeric(names(s2_phi_range_under_list))) +
    ylab('Bias (RMSE)\n') + xlab(expression(phi))

# False Discovery Rate (underestimated)
fdr_s2_range_df1 <- data.frame()
for (i in names(s2_phi_range_under_list)) {
    message(i)
    pvalue <- s2_phi_range_under_list[[i]][['pvalue']]
    pvalue <- pvalue[names(pvalue) == 'lag_wy']
    fdr_temp <- cbind(pvalue, data.frame(phi = as.numeric(i),
                                         type = 'Under estimate'))
    fdr_s2_range_df1 <- rbind(fdr_s2_range_df1, fdr_temp)
}
fdr_s2_range_df1 <- fdr_s2_range_df1 %>% group_by(phi, type) %>%
    summarise(fdr = fdr_fun(pvalue))


# False Discovery Rate (over estimated)
fdr_s2_range_df2 <- data.frame()
for (i in names(s2_phi_range_over_list)) {
    message(i)
    pvalue <- s2_phi_range_over_list[[i]][['pvalue']]
    pvalue <- pvalue[names(pvalue) == 'lag_wy']
    fdr_temp <- cbind(pvalue, data.frame(phi = as.numeric(i),
                                         type = 'Over estimate'))
    fdr_s2_range_df2 <- rbind(fdr_s2_range_df2, fdr_temp)
}
fdr_s2_range_df2 <- fdr_s2_range_df2 %>% group_by(phi, type) %>%
    summarise(fdr = fdr_fun(pvalue))

fdr_s2_range_df <- rbind(fdr_s2_range_df1, fdr_s2_range_df2)

fdr_s2_range <- ggplot(fdr_s2_range_df, aes(phi, fdr, group = type, linetype = type)) +
    geom_line() +
    scale_linetype(name = "") +
    geom_hline(yintercept = 0.05, linetype = 'dotted', color = 'grey') +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = as.numeric(names(s2_phi_range_under_list))) +
    ggtitle("Scenario 2") +
    ylab('TLSL False Discovery Rate\n') + xlab(expression(phi))


# Scenario 3 (omitted spatially clustered covariate) ---------------------------
# Coefficient Bias
load('mc_results/scenario3_range_theta_wz.rda')
rmse_s3_range_df <- data.frame()
for (i in names(s3_theta_wz_range_under_list)) {
    message(i)
    rmse_temp <- s3_theta_wz_range_under_list[[i]][['rmse']]
    rmse_temp <- cbind(rmse_temp, data.frame(theta_wz = as.numeric(i),
                                             type = 'Scenario 3 under estimate'))
    rmse_s3_range_df <- rbind(rmse_s3_range_df, rmse_temp)
}

for (i in names(s3_theta_wz_range_over_list)) {
    message(i)
    rmse_temp <- s3_theta_wz_range_over_list[[i]][['rmse']]
    rmse_temp <- cbind(rmse_temp, data.frame(theta_wz = as.numeric(i),
                                             type = 'Scenario 3 over estimate'))
    rmse_s3_range_df <- rbind(rmse_s3_range_df, rmse_temp)
}

rmse_s3_range_df$theta_wz_log <- log(rmse_s3_range_df$theta_wz)

rmse_s3_range <- ggplot(rmse_s3_range_df, aes(theta_wz_log, rmse, group = variable,
                                              linetype = variable)) +
    facet_wrap(~type) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    scale_linetype(name = "", labels = expression(hat(beta)[1], hat(theta)[wz])) +
    scale_x_continuous(breaks = unique(rmse_s3_range_df$theta_wz_log),
                       labels = as.numeric(names(s3_theta_wz_range_under_list))) +
 #   scale_y_continuous(limits = c(0, 2.5)) +
    ylab('Bias (RMSE)\n') + xlab(expression(paste(theta[wz], ' (log spaced scale)')))


# False Discovery Rate (underestimated)
fdr_s3_range_df1 <- data.frame()
for (i in names(s3_theta_wz_range_under_list)) {
    message(i)
    pvalue <- s3_theta_wz_range_under_list[[i]][['pvalue']]
    pvalue <- pvalue[names(pvalue) == 'lag_wy']
    fdr_temp <- cbind(pvalue, data.frame(theta_wz = as.numeric(i),
                                         type = 'Under estimate'))
    fdr_s3_range_df1 <- rbind(fdr_s3_range_df1, fdr_temp)
}
fdr_s3_range_df1 <- fdr_s3_range_df1 %>% group_by(theta_wz, type) %>%
    summarise(fdr = fdr_fun(pvalue))


# False Discovery Rate (over estimated)
fdr_s3_range_df2 <- data.frame()
for (i in names(s3_theta_wz_range_over_list)) {
    message(i)
    pvalue <- s3_theta_wz_range_over_list[[i]][['pvalue']]
    pvalue <- pvalue[names(pvalue) == 'lag_wy']
    fdr_temp <- cbind(pvalue, data.frame(theta_wz = as.numeric(i),
                                         type = 'Over estimate'))
    fdr_s3_range_df2 <- rbind(fdr_s3_range_df2, fdr_temp)
}
fdr_s3_range_df2 <- fdr_s3_range_df2 %>% group_by(theta_wz, type) %>%
    summarise(fdr = fdr_fun(pvalue))

fdr_s3_range_df <- rbind(fdr_s3_range_df1, fdr_s3_range_df2)
fdr_s3_range_df$theta_wz_log <- log(fdr_s3_range_df$theta_wz)

fdr_s3_range <- ggplot(fdr_s3_range_df, aes(theta_wz_log, fdr, group = type,
                                            linetype = type)) +
    geom_line() +
    scale_linetype(name = "") +
    geom_hline(yintercept = 0.05, linetype = 'dotted', color = 'grey') +
    scale_x_continuous(breaks = unique(rmse_s3_range_df$theta_wz_log),
                       labels = as.numeric(names(s3_theta_wz_range_under_list))) +
    scale_y_continuous(limits = c(0, 1)) +
    ggtitle("Scenario 3") +
    ylab('TLSL False Discovery Rate\n') + xlab(expression(theta[WZ]))

pdf('mc_figures/rmse_fdr_scen_2_3.pdf', width = 15, height = 12)
    grid.arrange(fdr_s2_range, rmse_s2_range, fdr_s3_range, rmse_s3_range,
                 ncol = 2)
dev.off()

