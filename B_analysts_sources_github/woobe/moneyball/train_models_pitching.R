# ------------------------------------------------------------------------------
# Predicting Modelling for Pitching Data
# ------------------------------------------------------------------------------


# Lib
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(fst))
suppressPackageStartupMessages(library(h2o))


# Core Parameters
n_seed = 1234 # for reproducibility


# ------------------------------------------------------------------------------
# Start H2O
# ------------------------------------------------------------------------------

h2o.init(nthreads = -1, max_mem_size = "8g")
h2o.no_progress()




# ------------------------------------------------------------------------------
# Load Munged Datasets from 'data_munging.R'
# ------------------------------------------------------------------------------


# Normal R data frame
d_train = read_fst("./cache_data/d_pit_train.fst")
d_valid = read_fst("./cache_data/d_pit_valid.fst")
d_test = read_fst("./cache_data/d_pit_test.fst")


# (Temp) Add Full Name for quick sort
d_train$full_name = paste(d_train$nameFirst, d_train$nameLast)
d_valid$full_name = paste(d_valid$nameFirst, d_valid$nameLast)
d_test$full_name = paste(d_test$nameFirst, d_test$nameLast)


# ------------------------------------------------------------------------------
# Define Targets and Features for Supervised Learning
# ------------------------------------------------------------------------------


# Targets
targets = c("W", "L", "G", "GS", "CG", "SHO", "SV",
            "IPouts", "H", "ER", "HR", "BB", "SO",
            "BAOpp", "ERA", "IBB", "WP", "HBP", "BK",
            "BFP", "GF", "R", "SH", "SF", "GIDP", "WHIP")

# Features
features = setdiff(colnames(d_train),
                   c(targets, "playerID",
                     "yearID", "debut_year",
                     "birthCountry", "birthCity", "birthState",
                     "nameFirst", "nameLast", "nameGiven", "full_name",
                     "fold"))


# Main AutoML Loop
for (n_target in 1:length(targets)) {


  # Display
  cat("[H2O AutoML]: Building Models for Target ...", targets[n_target], "...\n")


  # Clean up H2O cluster
  h2o.removeAll()


  # H2O data frame
  h_train = h2o.importFile("./cache_data/d_pit_train.csv", destination_frame = "h_train")
  h_valid = h2o.importFile("./cache_data/d_pit_valid.csv", destination_frame = "h_valid")
  h_test = h2o.importFile("./cache_data/d_pit_test.csv", destination_frame = "h_test")


  # H2O AutoML
  automl = h2o.automl(x = features, y = targets[n_target],
                      training_frame = h_train,
                      validation_frame = h_valid,
                      # fold_column = "fold",
                      max_models = 5,
                      stopping_metric = "RMSE",
                      stopping_rounds = 3,
                      seed = n_seed,
                      project_name = paste0("AutoML_", targets[n_target]),
                      exclude_algos = c("DRF", "DeepLearning"))


  # Extract model
  model_best = automl@leader

  # Save binary H2O model
  h2o.saveModel(model_best, path = "./h2o_models/pitching/", force = TRUE)

  # Remove previously saved model (if needed)
  if (file.exists(paste0("./h2o_models/pitching/automl_leader_", targets[n_target]))) {
    file.remove(paste0("./h2o_models/pitching/automl_leader_", targets[n_target]))
  }

  # Rename saved model (for easy access later)
  file.rename(from = paste0("./h2o_models/pitching/", model_best@model_id),
              to = paste0("./h2o_models/pitching/automl_leader_", targets[n_target]))

  # Make Predictions
  tmp_yhat_train = as.data.frame(h2o.predict(model_best, h_train))
  tmp_yhat_valid = as.data.frame(h2o.predict(model_best, h_valid))
  tmp_yhat_test = as.data.frame(h2o.predict(model_best, h_test))

  # Check
  if (FALSE) {

    row_check_valid = which(d_valid$full_name %in% c("Clayton Kershaw",
                                                     "Chris Sale",
                                                     "Corey Kluber"))

    print(data.frame(d_valid[row_check_valid, c("full_name", targets[n_target])],
                     yhat = tmp_yhat_valid[row_check_valid,]))

  }


  # Store Results
  colnames(tmp_yhat_train) = paste0("pred_", targets[n_target])
  colnames(tmp_yhat_valid) = paste0("pred_", targets[n_target])
  colnames(tmp_yhat_test) = paste0("pred_", targets[n_target])

  if (n_target == 1) {

    yhat_train_all = tmp_yhat_train
    yhat_valid_all = tmp_yhat_valid
    yhat_test_all = tmp_yhat_test

  } else {

    yhat_train_all = cbind(yhat_train_all, tmp_yhat_train)
    yhat_valid_all = cbind(yhat_valid_all, tmp_yhat_valid)
    yhat_test_all = cbind(yhat_test_all, tmp_yhat_test)

  }



}


# save results
d_all = rbind(d_train, d_valid, d_test)
yhat_all = rbind(yhat_train_all, yhat_valid_all, yhat_test_all)
d_output = cbind(d_all, yhat_all)


# Write to disk
fwrite(d_output, file = "./h2o_outputs/results_pitching.csv")
write_fst(d_output, path = "./h2o_outputs/results_pitching.fst", compress = 100)

