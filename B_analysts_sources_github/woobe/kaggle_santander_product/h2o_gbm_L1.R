# ------------------------------------------------------------------------------
# H2O GBM for Santander Product Recommendations
# Generate level one data using H2O Random Grid Search
# ------------------------------------------------------------------------------

# Core model parameters
n_seed <- 1234
n_trees_max <- 500      # with early stopping, usually <300 trees
n_rate <- 0.05          # fixed
n_folds <- 5            # CV fold
n_grid_models <- 5      # max no. of random grid search models 
n_score_interval <- 5
n_stop_round <- 10
stop_metric <- "logloss"

# H2O's R Package
suppressPackageStartupMessages(library(h2o))         # h2o_3.10.2.1
suppressPackageStartupMessages(library(data.table))  # data.table_1.10.1

# Data in gz files
gz_train <- "./data/d_train.csv.gz"
gz_valid <- "./data/d_valid.csv.gz"
gz_test <- "./data/d_test.csv.gz"

csv_train <- "./data/d_train.csv"
csv_valid <- "./data/d_valid.csv"
csv_test <- "./data/d_test.csv"


# ------------------------------------------------------------------------------
# Import Data into H2O
# ------------------------------------------------------------------------------

# Start H2O clusters
h2o.init(nthreads = -1)
# h2o.no_progress() # disable progress bar 

# Data created with data_prep.R
h_train <- h2o.importFile(gz_train)
h_valid <- h2o.importFile(gz_valid)
h_test <- h2o.importFile(gz_test)

# Check size
# dim(h_train)   # 405809 x 158
# dim(h_valid)   # 35843 x 158
# dim(h_test)    # 929615 x 158


# ------------------------------------------------------------------------------
# Convert data types
# ------------------------------------------------------------------------------

# Convert some columns to categorical
h_train$indrel_1mes <- as.factor(h_train$indrel_1mes) # Customer type
h_train$cod_prov <- as.factor(h_train$cod_prov) # Province code (customer's address)
h_train$dato_month <- as.factor(h_train$dato_month) 
h_train$alta_month <- as.factor(h_train$alta_month)
h_train$alta_year <- as.factor(h_train$alta_year) 

# Convert some columns to categorical
h_valid$indrel_1mes <- as.factor(h_valid$indrel_1mes) # Customer type
h_valid$cod_prov <- as.factor(h_valid$cod_prov) # Province code (customer's address)
h_valid$dato_month <- as.factor(h_valid$dato_month) 
h_valid$alta_month <- as.factor(h_valid$alta_month) 
h_valid$alta_year <- as.factor(h_valid$alta_year) 

# Convert some columns to categorical
h_test$indrel_1mes <- as.factor(h_test$indrel_1mes) # Customer type
h_test$cod_prov <- as.factor(h_test$cod_prov) # Province code (customer's address)
h_test$dato_month <- as.factor(h_test$dato_month) 
h_test$alta_month <- as.factor(h_test$alta_month) 
h_test$alta_year <- as.factor(h_test$alta_year) 


# ------------------------------------------------------------------------------
# Define features
# ------------------------------------------------------------------------------

col_ignore <- c("fecha_dato", "ncodpers", "fecha_alta", "cod_prov", 
                "ult_fec_cli_1t", "added_products", "last_year", "last_month", 
                "alta_year_month", "dato_year_month", "cv_fold")
features <- setdiff(colnames(h_train), col_ignore) # all features
print(features)


# ------------------------------------------------------------------------------
# Using H2O random grid search to generate level one data
# ------------------------------------------------------------------------------

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = n_grid_models,
                        seed = n_seed)

params_gbm <- list(max_depth = seq(3, 5, 1),
                   sample_rate = seq(0.5, 0.9, 0.1),
                   col_sample_rate = seq(0.5, 0.9, 0.1))

# H2O GBM Grid
grid_gbm <- h2o.grid(
                     
                     # Grid search parameters
                     algorithm = "gbm",
                     grid_id = "grid_gbm",
                     hyper_params = params_gbm,
                     search_criteria = search_criteria,
                     
                     # Core model parameters
                     training_frame = h_train,
                     x = features,
                     y = "added_products",
                     learn_rate = n_rate,
                     ntrees = n_trees_max,
                     seed = n_seed,
                     nfolds = n_folds,
                     keep_cross_validation_predictions = TRUE,
                     fold_assignment = "Stratified", 
                     # using Stratified instead of Modulo as I am not using
                     # h2oEnsemble::h2o.stack() for stacking
                     
                     # Early stopping parameters
                     score_tree_interval = n_score_interval,
                     stopping_metric  = stop_metric,
                     stopping_tolerance = 0.01,
                     stopping_rounds = n_stop_round
                     
                     )


# ------------------------------------------------------------------------------
# Extract models and data
# ------------------------------------------------------------------------------

# Extract all models
gbm_models <- lapply(grid_gbm@model_ids, function(model_id) h2o.getModel(model_id))

# Extract Level One Data
for (n in 1:n_folds) {
  
  # Display
  cat("[Extracting Data] ... CV Model", n, "...\n")
  
  # Extract predictions (L1 data)
  L1_train_temp <- h2o.cross_validation_holdout_predictions(gbm_models[[n]])
  L1_valid_temp <- h2o.predict(gbm_models[[n]], h_valid)
  L1_test_temp <- h2o.predict(gbm_models[[n]], h_test)
  
  # Trim 
  L1_train_temp <- as.data.frame(L1_train_temp)[-1]
  L1_valid_temp <- as.data.frame(L1_valid_temp)[-1]
  L1_test_temp <- as.data.frame(L1_test_temp)[-1]
  
  # Update colnames (to include model number)
  colnames(L1_train_temp) <- paste0("L1_m", n, "_", colnames(L1_train_temp))
  colnames(L1_valid_temp) <- paste0("L1_m", n, "_", colnames(L1_valid_temp))
  colnames(L1_test_temp) <- paste0("L1_m", n, "_", colnames(L1_test_temp))
  
  if (n == 1) {
    
    L1_train <- L1_train_temp
    L1_valid <- L1_valid_temp
    L1_test <- L1_test_temp
    
  } else {
    
    L1_train <- cbind(L1_train, L1_train_temp)
    L1_valid <- cbind(L1_valid, L1_valid_temp)
    L1_test <- cbind(L1_test, L1_test_temp)
    
  }
  
  # Clean up
  rm(L1_train_temp, L1_valid_temp, L1_test_temp)
  gc()
  
}

# Adding target to L1_train and L1_valid (for stacking in next stage)
y_train <- as.data.frame(h_train$added_products)
y_valid <- as.data.frame(h_valid$added_products)
L1_train <- cbind(L1_train, y_train)
L1_valid <- cbind(L1_valid, y_valid)


# ------------------------------------------------------------------------------
# Evaluate Random Grid Search Models
# ------------------------------------------------------------------------------

d_eval <- c()
for (n in 1:n_folds) {
  
  # Extract model
  model <- gbm_models[[n]]

  # Evaluate performance on validation set
  perf_valid <- h2o.performance(model, newdata = h_valid)
  
  # Create results summary data frame
  d_eval_temp <- data.frame(model_id = model@model_id,
                            algo = model@algorithm,
                            learn_rate = model@parameters$learn_rate,
                            n_trees = model@parameters$ntrees,
                            max_depth = model@parameters$max_depth,
                            row_samp = model@parameters$sample_rate,
                            col_samp = model@parameters$col_sample_rate,
                            seed = model@parameters$seed,
                            n_cv_fold = n_folds,
                            logloss_train = model@model$training_metrics@metrics$logloss,
                            logloss_cv = model@model$cross_validation_metrics@metrics$logloss,
                            logloss_valid = perf_valid@metrics$logloss)
  
  # Stack
  d_eval <- rbind(d_eval, d_eval_temp)
  rm(d_eval_temp)
  
}

# Print out
cat("\n\n=============== Summary of Metrics: =============== \n")
print(d_eval)

# =============== Summary of Metrics: =============== 
#   model_id algo learn_rate n_trees max_depth row_samp col_samp seed
# 1 grid_gbm_model_0  gbm       0.05     198         4      0.7      0.9 1234
# 2 grid_gbm_model_4  gbm       0.05     194         4      0.6      0.6 1234
# 3 grid_gbm_model_2  gbm       0.05     193         4      0.6      0.9 1234
# 4 grid_gbm_model_1  gbm       0.05     240         3      0.9      0.7 1234
# 5 grid_gbm_model_3  gbm       0.05     241         3      0.7      0.8 1234
# n_cv_fold logloss_train logloss_cv logloss_valid
# 1         5     0.9502685  0.9934115     0.9464101
# 2         5     0.9522171  0.9938626     0.9448556
# 3         5     0.9566952  0.9980570     0.9685286
# 4         5     0.9734655  0.9994102     0.9443164
# 5         5     0.9742857  1.0013029     0.9458400


# ------------------------------------------------------------------------------
# Saving files
# ------------------------------------------------------------------------------

# Save H2O models
for (n in 1:n_folds) {
  h2o.saveModel(gbm_models[[n]], path = "./output/h2o_gbm_L1_run2/", force = TRUE)
}

# Write evaluaton results to disk
fwrite(d_eval, file = "./output/h2o_gbm_L1_run2/L1_eval.csv")

# Round it
L1_train[, -ncol(L1_train)] <- round(L1_train[, -ncol(L1_train)], 4)
L1_valid[, -ncol(L1_valid)] <- round(L1_valid[, -ncol(L1_valid)], 4)
L1_test <- round(L1_test, 4)

# Write L1 data to disk
options(digits = 18)
fwrite(L1_train, file = "./output/h2o_gbm_L1_run2/L1_train.csv")
fwrite(L1_valid, file = "./output/h2o_gbm_L1_run2/L1_valid.csv")
fwrite(L1_test, file = "./output/h2o_gbm_L1_run2/L1_test.csv")

# Gzip L1 data 
system("gzip -9 -v ./output/h2o_gbm_L1_run2/L1_train.csv")
system("gzip -9 -v ./output/h2o_gbm_L1_run2/L1_valid.csv")
system("gzip -9 -v ./output/h2o_gbm_L1_run2/L1_test.csv")


# ------------------------------------------------------------------------------
# Print System Info
# ------------------------------------------------------------------------------

print(sessionInfo())
print(Sys.info())

# R version 3.2.3 (2015-12-10)
# Platform: aarch64-unknown-linux-gnu (64-bit)
# Running under: Ubuntu 16.04.1 LTS
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
# [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
# [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
# [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
#   [1] methods   stats     graphics  grDevices utils     datasets  base     
# 
# other attached packages:
#   [1] data.table_1.10.0 h2o_3.10.2.1     
# 
# loaded via a namespace (and not attached):
#   [1] tools_3.2.3    RCurl_1.95-4.8 jsonlite_1.2   bitops_1.0-6  
# sysname 
# "Linux" 
# release 
# "4.4.0-38-generic" 
# version 
# "#57-Ubuntu SMP Wed Sep 7 10:19:14 UTC 2016" 
# nodename 
# "joe.local.lan" 
# machine 
# "aarch64" 
# login 
# "root" 
# user 
# "root" 
# effective_user 
# "root" 

