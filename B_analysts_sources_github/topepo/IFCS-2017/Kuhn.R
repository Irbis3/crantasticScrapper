#####################################################################
## Code for "Predictive Modeling in R" by Max Kuhn
## Files are at  https://github.com/topepo/IFCS-17

###################################################################
## Slide 4: Load Data

library(caret)
data(scat)
str(scat)

###################################################################
## Slide 5: Missing Data Profile

pct_nonmissing <- function(x) mean(!is.na(x))
unlist(lapply(scat, pct_nonmissing))

###################################################################
## Slide 7: Split the Data

set.seed(11218)
in_train <- createDataPartition(scat$Species, p = 3/4, list = FALSE)
head(in_train)
train_data <- scat[ in_train,]
test_data  <- scat[-in_train,]
## It isn't much data but it's better than nothing...
table(test_data$Species)

###################################################################
## Slide 8: Interaction Plot Code

int_plot <- function(dat, y, x, group) {
  library(plyr)
  if(!is.factor(dat[,group])) dat[,group] <- factor(dat[,group])
  means <- ddply(dat, c(y, group), 
                 function(obj) c(mean = mean(obj[,x], na.rm = TRUE)))
  ggplot(dat, 
         aes_string(y = x,  x = y, color = group, group = group)) + 
    geom_point(position = position_dodge(width = 0.2)) + 
    geom_line(data = means, aes_string(y = "mean")) +
    theme(legend.position = "top")
}

###################################################################
## Slides 9-14

int_plot(train_data, y = "Species", x = "Mass", group = "ropey")

int_plot(train_data, y = "Species", x = "Mass", group = "Location")

int_plot(train_data, y = "Species", x = "Mass", group = "segmented")

int_plot(train_data, y = "Species", x = "Length", group = "Location")

int_plot(train_data, y = "Species", x = "Length", group = "ropey")

int_plot(train_data, y = "Species", x = "CN", group = "ropey")

###################################################################
## Slide 20: An Initial Model Specification

small_form <- 
  paste("Species ~ Month + Year + Site + Age +",
        "Number + Length*ropey + (Location + segmented)*Mass + ",
        "flat + scrape")
small_form <- as.formula(small_form)

small_tr_dat <- train_data[, all.vars(small_form)]
small_tr_dat <- small_tr_dat[complete.cases(small_tr_dat),]

###################################################################
## Slide 28: Multinomial Model

ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE)
set.seed(2592) ## locks in the resamples
mnr_tune <- train(small_form, data = small_tr_dat,
                  method = "multinom", 
                  preProc = c("center", "scale"),
                  ## avoid regularization for now
                  tuneGrid = data.frame(decay = 0),
                  trControl = ctrl,
                  ## this next argument is passed to `multinom`
                  trace = FALSE)

###################################################################
## Slide 29: Multinomial Model

mnr_tune
predict(mnr_tune, head(test_data)) ## or type = "prob"

###################################################################
## Slide 30: Multinomial Model Variable Importance

print(varImp(mnr_tune, scale = FALSE), top = 10)

###################################################################
## Slide 33: Multinomial Model -- All Data

full_form <- paste("Species ~ Month + Year + Site + Age + Number + ",
                   "Length*ropey + (Location + segmented)*Mass + ",
                   "flat + scrape + ",
                   "TI + d13C + d15N + CN + Diameter + Taper")
full_form <- as.formula(full_form)
set.seed(2592) 
mnr_impute <- train(full_form, data = train_data,
                    method = "multinom",
                    ## Add imputation to the list of pre-processing steps
                    preProc = c("center", "scale", "knnImpute", "zv"),
                    tuneGrid = data.frame(decay = 0),
                    trControl = ctrl,
                    ## do not remove missing data before modeling
                    na.action = na.pass,
                    trace = FALSE)

mnr_impute

###################################################################
## Slide 35: Multinomial Model -- All Data Variable Importance

print(varImp(mnr_impute, scale = FALSE), top = 10)

###################################################################
## Slide 36: Resampled Confusion Matrix

confusionMatrix(mnr_tune)

###################################################################
## Slide 43: Model Tuning Grid

glmn_grid <- expand.grid(alpha = c(0.05, seq(.1, 1, by = 0.025)),
                         lambda = c(.001, .01, .1))
nrow(glmn_grid)

###################################################################
## Slide 44: Model Tuning 

set.seed(2592) ## use the same resamples as mnr_impute
glmn_tune <- train(full_form, data = train_data,
                   method = "glmnet", 
                   preProc = c("center", "scale", "knnImpute", "zv"),
                   ## pass in the tuning grid
                   tuneGrid = glmn_grid,
                   ## pick the sub-model with the best kappa value
                   metric = "Kappa",
                   na.action = na.pass,
                   trControl = ctrl)
## best sub-model results:
glmn_tune$bestTune
getTrainPerf(glmn_tune)

###################################################################
## Slide 45: Model Tuning Plot

theme_set(theme_bw())
ggplot(glmn_tune) + theme(legend.position = "top")

###################################################################
## Slide 46: Resampled Confusion Matrix

confusionMatrix(glmn_tune)

###################################################################
## Slide 48: Model Comparison

compare_models(glmn_tune, mnr_impute, metric = "Kappa")

###################################################################
## Slide 49: Fitting Other Models

set.seed(2592) 
bagged_tree <- train(Species ~ ., data = train_data,
                     method = "treebag", 
                     metric = "Kappa",
                     na.action = na.pass,
                     trControl = ctrl)
getTrainPerf(bagged_tree)

###################################################################
## Slide 50: Fitting Other Models

set.seed(2592) 
knn_tune <- train(Species ~ ., data = train_data,
                  method = "knn", 
                  preProc = c("center", "scale", "knnImpute", "zv"),
                  ## pass in the tuning grid _size_
                  tuneLength = 20,
                  metric = "Kappa",
                  na.action = na.pass,
                  trControl = ctrl)
getTrainPerf(knn_tune)

###################################################################
## Slide 51: Preprocessing Too

set.seed(2592) 
transformed <- train(full_form, data = train_data,
                   method = "glmnet", 
                   ## Also transform the predictors
                   preProc = c("center", "scale", "knnImpute", 
                               "zv", "YeoJohnson"),
                   tuneGrid = glmn_grid,
                   metric = "Kappa",
                   na.action = na.pass,
                   trControl = ctrl)
getTrainPerf(transformed)

###################################################################
## Slide 52: Collecting the Results

rs <- resamples(list(knn = knn_tune, bagged = bagged_tree,
                     multinomial = mnr_impute, glmnet = glmn_tune,
                     "glmnet + trans" = transformed))
summary(rs, metric = "Kappa")

###################################################################
## Slide 53: Resampling Distributions

bwplot(rs, metric = "Kappa")

###################################################################
## Slide 54: Resampling Distributions

dotplot(rs, metric = "Kappa")

###################################################################
## Slide 55: Test Set Results

test_pred <- predict(glmn_tune, newdata = test_data, na.action = na.pass)
str(test_pred)
test_prob <- predict(glmn_tune, newdata = test_data, 
                     na.action = na.pass, type = "prob")
str(test_prob)

###################################################################
## Slide 56: Test Set Results

confusionMatrix(test_pred, test_data$Species)

###################################################################
## Slide 58: Sequentially Creating a Recipe

library(recipes)
scat_rec <- recipe(Species ~ ., data = scat) 
scat_rec

###################################################################
## Slide 59: Imputation

scat_rec <- scat_rec %>%
  step_bagimpute(Diameter, Taper, TI, Mass, d13C, d15N, CN) 

scat_rec <- scat_rec %>%
  step_dummy(all_nominal(), -all_outcomes()) 
scat_rec

###################################################################
## Slide 60: Interactions

scat_rec <- scat_rec %>%
  step_interact(~ Length:ropey) %>%
  step_interact(~ Location_middle:Mass) %>%
  step_interact(~ Location_off_edge:Mass) %>%
  step_interact(~ segmented:Mass)

###################################################################
## Slide 61: Estimating the Required Statistics

scat_rec_trained <- prep(scat_rec, training = train_data, retain = TRUE)

###################################################################
## Slide 62: Estimating the Required Statistics

scat_rec_trained

###################################################################
## Slide 63: Processing Data

proc_train_data <- bake(scat_rec_trained, newdata = train_data)
proc_test_data  <- bake(scat_rec_trained, newdata = test_data)

mean(!complete.cases(train_data))
mean(!complete.cases(proc_train_data))

names(proc_train_data)


###################################################################
## Slide 64: How Are Recipes used with Models?

train(scat_rec, 
      data = train_data,
      method = "glmnet", 
      tuneGrid = glmn_grid,
      metric = "Kappa",
      na.action = na.pass,
      trControl = ctrl)

