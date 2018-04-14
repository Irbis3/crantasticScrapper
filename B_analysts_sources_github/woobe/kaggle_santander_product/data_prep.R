# ------------------------------------------------------------------------------
# Single script to create d_train and d_test
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# R Packages
# ------------------------------------------------------------------------------

# Check and install other packages if needed
list_of_packages <- c("bit64", "dplyr", "lubridate", "data.table", "readr")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


# Load all packages
suppressPackageStartupMessages(library(bit64))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))

# ------------------------------------------------------------------------------
# Load CSVs 
# ------------------------------------------------------------------------------

# Unzip, load and delete
col_type_train <- c("DiccciDiicDccccccciicinciiiiiiiiiiiiiiiiiiiiiiii")
col_type_test <- c("DiccciDiicDccccccciicinc")

d_train <- read_csv("./data/train_ver2.csv", col_types = col_type_train)
d_test <- read_csv("./data/test_ver2.csv", col_types = col_type_test)


# for the NA in payroll (ind_nomina_ult1) and pensions (ind_nom_pens_ult1)
# just turn them into zero
rows_na <- which(is.na(d_train[, "ind_nomina_ult1"]))
d_train[rows_na, "ind_nomina_ult1"] <- 0

rows_na <- which(is.na(d_train[, "ind_nom_pens_ult1"]))
d_train[rows_na, "ind_nom_pens_ult1"] <- 0


# Create NA targets for test
d_targets <- data.frame(matrix(data = NA, nrow = nrow(d_test), ncol = 24))
colnames(d_targets) <- colnames(d_train)[25:48]
d_test <- cbind(d_test, d_targets)
rm(d_targets)


# ------------------------------------------------------------------------------
# Combine
# ------------------------------------------------------------------------------

# Combine
d_all <- rbind(d_train, d_test)

# Remove "tipodom" back in just to align with previous test
col_keep <- setdiff(colnames(d_all), "tipodom")
d_all <- d_all[, col_keep]

# Keep row numbers
nrow_train <- nrow(d_train)
nrow_test <- nrow(d_test)

# Clean up
rm(d_train, d_test)
gc()


# ------------------------------------------------------------------------------
# Basic Feature Engineering
# ------------------------------------------------------------------------------

# for negative values in antiguedad, turn into NA
rows_neg <- which(d_all[, "antiguedad"] < 0)
d_all[rows_neg, ]$antiguedad <- NA

# Add a few features
d_all <-
  d_all %>%
  arrange(ncodpers, fecha_dato) %>%
  mutate(sum_products =
           ind_ahor_fin_ult1 + ind_aval_fin_ult1 + ind_cco_fin_ult1 + ind_cder_fin_ult1 +
           ind_cno_fin_ult1 + ind_ctju_fin_ult1 + ind_ctma_fin_ult1 + ind_ctop_fin_ult1 +
           ind_ctpp_fin_ult1 + ind_deco_fin_ult1 + ind_deme_fin_ult1 + ind_dela_fin_ult1 +
           ind_ecue_fin_ult1 + ind_fond_fin_ult1 + ind_hip_fin_ult1 + ind_plan_fin_ult1 +
           ind_pres_fin_ult1 + ind_reca_fin_ult1 + ind_tjcr_fin_ult1 + ind_valo_fin_ult1 +
           ind_viv_fin_ult1 + ind_nomina_ult1 + ind_nom_pens_ult1 + ind_recibo_ult1) %>%
  mutate(dato_month = sprintf("%02d", month(fecha_dato))) %>%
  mutate(dato_year = year(fecha_dato)) %>%
  mutate(dato_year_month = paste0(dato_year, "_", dato_month)) %>%
  mutate(alta_month = sprintf("%02d", month(fecha_alta))) %>%
  mutate(alta_year = year(fecha_alta)) %>%
  mutate(alta_year_month = paste0(alta_year, "_", alta_month)) %>%
  mutate(last_month = sprintf("%02d", month(ult_fec_cli_1t))) %>%
  mutate(last_year = year(ult_fec_cli_1t))

# Reformat dates to string (safer for later export)
d_all <- d_all %>% mutate(fecha_dato = paste0(dato_year, "-", dato_month, "-28")) # reproduce in text format
d_all <- d_all %>% mutate(fecha_alta = paste0(alta_year, "-", alta_month, "-28")) # simplify it, get rid of day
d_all$ult_fec_cli_1t <- NA # make them all NA ... need to revise this later

# Add a flag to indicate closed account
rows_closed_acc <- 
  which(d_all$last_month == d_all$dato_month &
          d_all$dato_year == d_all$last_year)
d_all$flag_closed_acc <- 0
d_all[rows_closed_acc, ]$flag_closed_acc <- 1

# Keep colnames
col_d_all <- colnames(d_all)
gc()

# Size 14576924 x 57 at this point


# ------------------------------------------------------------------------------
# back to original routine
# ------------------------------------------------------------------------------

# Extract column names for later use
col_core <- colnames(d_all)[1:23]
col_targets <- colnames(d_all)[24:47]
col_new_fea <- colnames(d_all)[49:ncol(d_all)] # exclude sum_products
print(col_core)
print(col_targets)
print(col_new_fea)

# For each month (from 2015-02 to 2016-05)
year_month <- sort(unique(d_all$dato_year_month))
print(year_month)

print(summary(d_all$ncodpers))
print(table(d_all$fecha_dato))


# ------------------------------------------------------------------------------
# Function to create d_train or d_valid
# ------------------------------------------------------------------------------

# test values
# n_back <- 5 # no. of months to look back
# n_month <- 6 # target month (1 = 2015_01, 13 = 2016_01)

# function
create_train <- function(d_all, col_core, col_targets, col_new_fea, year_month,
                         n_back = 5, n_month = 6) {
  
  # Extract months
  month_now <- year_month[n_month]
  month_prev <- year_month[n_month-1]
  cat("\nWorking on records in", month_now, "\n")
  
  # Extract rows
  row_now <- which(d_all$dato_year_month == month_now)
  row_prev <- which(d_all$dato_year_month == month_prev)
  
  # Get customers with records
  d_now <- d_all[row_now, ]
  d_prev <- d_all[row_prev, ]
  cat("Records (T = 0):", nrow(d_now), "... (T = -1):", nrow(d_prev), "\n")
  
  # Common customers
  d_now <- d_now[which(d_now$ncodpers %in% d_prev$ncodpers),]
  d_prev <- d_prev[which(d_prev$ncodpers %in% d_now$ncodpers),]
  cat("Customers with 2 months' records:", nrow(d_now), "\n")
  
  # Sort
  d_now <- d_now %>% arrange(ncodpers)
  d_prev <- d_prev %>% arrange(ncodpers)
  if (identical(d_now$ncodpers, d_prev$ncodpers)) cat("Check: OK\n") else cat ("Check: Wrong!\n")
  
  # Diff (find customers with new products in current month)
  d_diff <- d_now[, col_targets] - d_prev[, col_targets]
  colnames(d_diff) <- paste0(colnames(d_diff), "_diff")
  d_diff$check <- apply(d_diff, 1, max)
  d_diff <- data.frame(ncodpers = d_now$ncodpers, d_diff)
  cat("Calculate differences (i.e. added/removed products)\n")
  
  # Customers with new products
  row_added <- which(d_diff$check == 1)
  cus_added <- d_now[row_added, ]$ncodpers
  cat("Customers with new products:", length(cus_added), "\n")
  
  # Now trim and combine
  d_now <- d_now[row_added, ]
  d_diff <- d_diff[row_added, ] # keep this for later
  #identical(d_now$ncodpers, d_diff$ncodpers) # check
  d_combine <- d_now[, c(col_core, col_new_fea)]
  
  # Loop through n months to add previous product records
  for (nn_month in (n_month - n_back) : (n_month - 1)) {
    
    # Extract customers data in nn_month
    month_temp <- year_month[nn_month]
    row_temp <- which(d_all$ncodpers %in% cus_added &
                        d_all$dato_year_month == month_temp)
    d_temp <- d_all[row_temp, c("ncodpers", col_targets, "sum_products")]
    
    # Rename columns
    colnames(d_temp)[2:ncol(d_temp)] <-
      paste0(colnames(d_temp)[2:ncol(d_temp)], "_Tminus", (n_month-nn_month))
    
    # Merge with d_combine
    d_combine <- merge(d_combine, d_temp, by = "ncodpers", all.x = TRUE)
    
    # Check NAs
    n_na <- length(which(is.na(d_combine[, ncol(d_combine)])))
    
    # Print out
    cat("Adding", month_temp, "...", nrow(d_temp),
        "records ... T minus", (n_month-nn_month),
        "... NAs:", n_na, "\n")
    
    # Clean up
    gc()
    
  }
  
  # Collapse 24 outputs to single column output
  d_output <- c()
  d_added_record <- c()
  
  for (n_col in 1:24) {
    row_added <- which(d_diff[, (n_col+1)] == 1)
    if (length(row_added) > 0) {
      d_temp <- data.frame(d_combine[row_added, ], added_products = col_targets[n_col])
      d_output <- rbind(d_output, d_temp)
    }
    
    d_added_record <-
      rbind(d_added_record,
            data.frame(product = col_targets[n_col], added = length(row_added)))
    
  }
  
  # Sep
  print(d_added_record)
  cat("\n")
  
  # Return
  return(d_output)
  
}

# ------------------------------------------------------------------------------
# Function to create d_test
# ------------------------------------------------------------------------------

# function n_month = 18 (June 2016)
create_test <- function(d_all, col_core, col_targets, col_new_fea, year_month,
                        n_back = 5, n_month = 18) {
  
  # Extract months
  month_now <- year_month[n_month]
  month_prev <- year_month[n_month-1]
  cat("\nWorking on records in", month_now, "\n")
  
  # Extract rows
  row_now <- which(d_all$dato_year_month == month_now)
  row_prev <- which(d_all$dato_year_month == month_prev)
  
  # Get customers with records
  d_now <- d_all[row_now, ]
  d_prev <- d_all[row_prev, ]
  cat("Records (T = 0):", nrow(d_now), "... (T = -1):", nrow(d_prev), "\n")
  
  # Common customers
  d_now <- d_now[which(d_now$ncodpers %in% d_prev$ncodpers),]
  d_prev <- d_prev[which(d_prev$ncodpers %in% d_now$ncodpers),]
  cat("Customers with 2 months' records:", nrow(d_now), "\n")
  
  # Sort
  d_now <- d_now %>% arrange(ncodpers)
  d_prev <- d_prev %>% arrange(ncodpers)
  if (identical(d_now$ncodpers, d_prev$ncodpers)) cat("Check: OK\n") else cat ("Check: Wrong!\n")
  
  # Now trim and combine
  d_combine <- d_now[, c(col_core, col_new_fea)]
  
  # Loop through n months to add previous product records
  for (nn_month in (n_month - n_back) : (n_month - 1)) {
    
    # Extract customers data in nn_month
    month_temp <- year_month[nn_month]
    row_temp <- which(d_all$ncodpers %in% d_combine$ncodpers &
                        d_all$dato_year_month == month_temp)
    d_temp <- d_all[row_temp, c("ncodpers", col_targets, "sum_products")]
    
    # Rename columns
    colnames(d_temp)[2:ncol(d_temp)] <-
      paste0(colnames(d_temp)[2:ncol(d_temp)], "_Tminus", (n_month-nn_month))
    
    # Merge with d_combine
    d_combine <- merge(d_combine, d_temp, by = "ncodpers", all.x = TRUE)
    
    # Check NAs
    n_na <- length(which(is.na(d_combine[, ncol(d_combine)])))
    
    # Print out
    cat("Adding", month_temp, "...", nrow(d_temp),
        "records ... T minus", (n_month-nn_month),
        "... NAs:", n_na, "\n")
    
    # Clean up
    gc()
    
  }
  
  # Add NA
  d_combine$added_products <- NA
  
  # Return
  return(d_combine)
  
}


# ------------------------------------------------------------------------------
# Create d_train and d_test
# ------------------------------------------------------------------------------

n_back <- 5 # no. of months to look back

d_train <- c()
for (n_month in c(6:16)) { ## using Jun-16 to Apr-16
  d_train <-
    rbind(d_train, create_train(d_all, col_core, col_targets, col_new_fea, year_month, n_back, n_month))
}

## Using May-16 for validation
d_valid <- create_train(d_all, col_core, col_targets, col_new_fea, year_month, n_back, n_month = 17)

## Jun-16 for test
d_test <- create_test(d_all, col_core, col_targets, col_new_fea, year_month, n_back, n_month = 18)


# ------------------------------------------------------------------------------
# Check
# ------------------------------------------------------------------------------

cat("\n === Checking d_train ====\n")
print(summary(d_train$ncodpers))
print(table(d_train$fecha_dato))

cat("\n === Checking d_valid ====\n")
print(summary(d_valid$ncodpers))
print(table(d_valid$fecha_dato))

cat("\n === Checking d_test ====\n")
print(summary(d_test$ncodpers))
print(table(d_test$fecha_dato))


# ------------------------------------------------------------------------------
# # Write results to CSV (note: change to different version name)
# ------------------------------------------------------------------------------

# Clean up (free up memory for compression)
rm(d_all)
gc()

# Write CSVs
options(digits = 18)
fwrite(d_train, file = "./data/d_train.csv")
fwrite(d_valid, file = "./data/d_valid.csv")
fwrite(d_test, file = "./data/d_test.csv")

# gzip them all
gc()
system(paste("gzip -9 -v", "./data/d_train.csv"))
gc()
system(paste("gzip -9 -v", "./data/d_valid.csv"))
gc()
system(paste("gzip -9 -v", "./data/d_test.csv"))
# These command automatically delete the CSV as well


