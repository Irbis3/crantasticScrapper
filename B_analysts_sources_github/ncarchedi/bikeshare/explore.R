library(dplyr)

source("utility_functions.R")

bikeshare <- read_and_prep_data("data/train.csv") %>%
  add_vars

train_split <- bikeshare[year(bikeshare$datetime) == 2011, ]
test_split <- bikeshare[year(bikeshare$datetime) == 2012, ]

test_split <- test_split %>% filter(weather != 4)

fit1 <- glm(count ~ prev1 + prev2, family = poisson, data = train_split)
fit2 <- glm(count ~ prev1 + prev2 + holiday + weather + temp + atemp + 
              humidity + windspeed + hour*workingday,
            family = poisson, data = train_split)

yhat <- predict(fit2, newdata = test_split, type = "response")

score <- compute_rmsle(yhat, test_split$count)
