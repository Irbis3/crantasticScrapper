library(dplyr)
library(stringr)

source("utility_functions.R")

bikeshare <- read_and_prep_data("data/train.csv") %>% add_vars %>% na.omit
final_fit <- glm(count ~ prev1 + prev2 + holiday + weather + temp + atemp +
                   humidity + windspeed + hour*workingday,
                 family = poisson, data = bikeshare)

test_data <- read_and_prep_data("data/test.csv") %>% add_vars
yhat <- predict(final_fit, newdata = test_data, type = "response")

mean_count <- mean(bikeshare$count)
yhat[is.na(yhat)] <- mean_count

submit_data <- data.frame(datetime = test_data$datetime,
                          count = yhat)

filename <- paste0("submission_", str_replace(now(), " ", "_"), ".csv")
write.csv(submit_data, file.path("submissions", filename), row.names = FALSE)
