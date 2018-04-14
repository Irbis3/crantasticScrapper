library(magrittr)
train <- data.table::fread("trainset.csv")
test <- data.table::fread("testset.csv")
colnames(train)
posixCols <- c("dateCrawled", "dateCreated", "lastSeen")
for(col in posixCols) {
  train[[col]] <- strptime(train[[col]], "%Y-%m-%dT%H:%M:%SZ") %>% as.POSIXct()
  test[[col]] <- strptime(test[[col]], "%Y-%m-%dT%H:%M:%SZ") %>% as.POSIXct()
}
fakeIntCols <- c("monthOfRegistration", "postalCode")
for(col in fakeIntCols) {
  train[[col]] <- as.character(train[[col]])
  test[[col]] <- as.character(test[[col]])
}
binning <- function(x, breaks = NULL, by = 0.25) {
  if (is.null(breaks)) breaks <- quantile(x) %>% unique()
  result <- cut(x, breaks = breaks, include.lowest = TRUE)
  attr(result, "breaks") <- breaks
  result
}
invalidCols <- c("id", "nrOfPictures", "seller", "offerType")
for(col in invalidCols) {
  test[[col]] <- train[[col]] <- NULL
}
# binningCols <- c("yearOfRegistration", "powerPS", "kilometer", "ad_exist_time", posixCols)
binningCols <- c()
binningColsBy <- list(
  "powerPS" = 0.05
)
for(col in binningCols) {
  if (col %in% names(binningColsBy)) {
    by <- binningColsBy[[col]]
  } else {
    by <- 0.25
  }
  train[[col]] <- binning(train[[col]], by = by)
  test[[col]] <- binning(test[[col]], attr(train[[col]], "breaks"))
}
posixToNumericCols <- posixCols
for(col in posixToNumericCols) {
  train[[col]] <- as.numeric(train[[col]])
  test[[col]] <- as.numeric(test[[col]])
}
m.train <- FeatureHashing::hashed.model.matrix(price ~ ., train, hash.size = 2^20)
m.test <- FeatureHashing::hashed.model.matrix(~ ., test, hash.size = 2^20)
i <- which((rep(1, nrow(m.train)) %*% m.train)@x >= 0)
m.train <- m.train[,i]
m.test <- m.test[,i]
data.train <- xgboost::xgb.DMatrix(m.train, label = train$price)

result <- lapply(1:10, function(i) {
  params <- list(
    "objective" = "reg:linear",
    "max_depth" = rpois(1, 3) + 3,
    eta = runif(1, 0, 1),
    min_child_weight = rpois(1, 1) + 1,
    subsample = runif(1, 0.5, 1),
    colsample_bytree = runif(1, 0.5, 1),
    lambda = ifelse(runif(1, 0, 1) > 0.5, 0, rexp(1)),
    lambda_bias = ifelse(runif(1, 0, 1) > 0.5, 0, rexp(1))#,
    )
  g0 <- xgboost::xgb.cv(
    params = params,
    data = data.train,
    nrounds = 1000,
    nfold = 5,
    showsd = TRUE,
    early_stopping_rounds = 5,
    prediction = TRUE
    )
  g <- xgboost::xgb.train(
    params = params,
    data = data.train,
    nrounds = g0$best_iteration,
    )
  list(g = g, g0 = g0)
})

saveRDS(result, file = "result-try1.Rds")

m2.train <- lapply(result, function(.) {
  .$g0$pred
}) %>%
  do.call(what = cbind)
m2.test <- lapply(result, function(.) {
  predict(.$g, m.test)
}) %>%
  do.call(what = cbind)

g2 <- lm(train$price ~ m2.train)
p2 <- m2.test %*% (g2$coef %>% tail(-1)) + g2$coef[1]
# 
# # g$best_iteration
submission <- data.table::fread("submit.csv")
submission$predict <- p2
# # # readLines("submit.csv", n = 6)
write.csv(submission, file = "try1.csv", quote = FALSE, row.names = FALSE)
