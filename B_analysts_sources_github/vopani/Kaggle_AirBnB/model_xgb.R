## setting working directory
path <- "./Kaggle_AirBnB"
setwd(path)


## loading libraries
library(bit64)
library(data.table)
library(xgboost)


## loading data
sessions <- fread("./sessions.csv")
X_test <- fread("./test_users.csv")
X_train <- fread("./train_users_2.csv")

train_lr <- fread("./train_lr.csv")
test_lr <- fread("./test_lr.csv")


## cleaning data

# keeping users from 2014 onwards
X_train <- subset(X_train, as.integer(substr(date_account_created,1,4)) >= 2014)
X_test$country_destination <- "NDF"

# combining browsers
X_train$first_browser[X_train$first_browser %in% c("Arora","Avant Browser","Camino","CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic","Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron","Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb","Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser","RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless","TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser")] <- "Other"
X_test$first_browser[X_test$first_browser %in% c("Arora","Avant Browser","Camino","CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic","Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron","Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb","Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser","RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless","TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser")] <- "Other"

X_train$first_browser[X_train$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
X_test$first_browser[X_test$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"

X_train$first_browser[X_train$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
X_test$first_browser[X_test$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"

X_train$first_browser[X_train$first_browser %in% c("IE","IE Mobile")] <- "IE"
X_test$first_browser[X_test$first_browser %in% c("IE","IE Mobile")] <- "IE"

X_train$first_browser[X_train$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
X_test$first_browser[X_test$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"

X_train$first_browser[X_train$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Firefox"
X_test$first_browser[X_test$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Firefox"

# cleaning age
X_train$age[is.na(X_train$age)] <- -1
X_test$age[is.na(X_test$age)] <- -1

for (i in 1920:1968)
{
  print(i)
  X_train$age[X_train$age == i] <- 2014 - X_train$age[X_train$age == i]
  X_test$age[X_test$age == i] <- 2014 - X_test$age[X_test$age == i]
}

# merging logistic regression predictions
X_train <- merge(X_train, train_lr, all.x=T, by="id")
X_test <- merge(X_test, test_lr, all.x=T, by="id")

X_train$pred_lr[is.na(X_train$pred_lr)] <- -1
X_test$pred_lr[is.na(X_test$pred_lr)] <- -1


# features engineering from sessions data
names(sessions)[1] <- "id"
sessions <- subset(sessions, id %in% c(unique(train$id), unique(test$id)))
sessions$count <- 1

# one-hot encoding action, action_type and action_detail
sessions_action <- dcast(sessions, id ~ action, mean, value.var="count")
sessions_action_type <- dcast(sessions, id ~ action_type, mean, value.var="count")
sessions_action_detail <- dcast(sessions, id ~ action_detail, mean, value.var="count")

sessions_action[is.na(sessions_action)] <- 0
sessions_action_type[is.na(sessions_action_type)] <- 0
sessions_action_detail[is.na(sessions_action_detail)] <- 0

# merging some similar events into a single one
sessions_action_detail <- sessions_action_detail[, ":="(coupon = ceiling((apply_coupon+apply_coupon_click+apply_coupon_click_success+apply_coupon_error+coupon_code_click+coupon_field_focus)/10),
                                                        cancel = ceiling((cancellation_policies+cancellation_policy+cancellation_policy_click)/10),
                                                        guest = ceiling((guest_cancellation+guest_itinerary+guest_receipt)/10),
                                                        host = ceiling((host_guarantee+host_home+host_refund_guest+host_respond+host_respond_page+host_standard_suspension)/10),
                                                        listing = ceiling((listing_descriptions+listing_recommendations+listing_reviews+listing_reviews_page+manage_listing+view_listing)/10),
                                                        message = ceiling((lookup_message_thread+message_inbox+message_post+message_thread+message_to_host_change+message_to_host_focus)/10))]

sessions_action <- sessions_action[, ":="(agree_terms = ceiling((agree_terms_check+agree_terms_uncheck)/10),
                                          google_translate = ceiling((ajax_google_translate_description+ajax_google_translate_reviews)/10),
                                          payout = ceiling((ajax_payout_edit+ajax_payout_options_by_country)/10),
                                          photo_widget = ceiling((ajax_photo_widget+ajax_photo_widget_form_iframe)/10),
                                          banner = ceiling((ajax_referral_banner_experiment_type+ajax_referral_banner_type)/10),
                                          create = ceiling((create+create_ach+create_multiple+create_paypal)/10),
                                          complete = ceiling((complete+complete_redirect+complete_status)/10),
                                          department = ceiling((department+departments)/10),
                                          edit = ceiling((edit+edit_verification)/10),
                                          email = ceiling((email_itinerary_colorbox+email_share+email_wishlist)/10),
                                          friends = ceiling((friends+friends_new)/10),
                                          home_safety = ceiling((home_safety_landing+home_safety_terms)/10),
                                          jumio = ceiling((jumio+jumio_redirect+jumio_token)/10),
                                          kba = ceiling((kba+kba_update)/10),
                                          listing = ceiling((listing+listings)/10),
                                          message_to_host = ceiling((message_to_host_change+message_to_host_focus)/10),
                                          multi = ceiling((multi+multi_message)/10),
                                          qt = ceiling((qt_reply_v2+qt_with+qt2)/10),
                                          reviews = ceiling((reviews+reviews_new)/10),
                                          similar_listings = ceiling((similar_listings+similar_listings_v2)/10),
                                          show = ceiling((show+show_code)/10),
                                          social = ceiling((social+social_connections)/10),
                                          terms = ceiling((terms+terms_and_conditions)/10),
                                          transaction_history = ceiling((transaction_history+transaction_history_paginated)/10),
                                          travel_plans = ceiling((travel_plans_current+travel_plans_previous)/10),
                                          update = ceiling((update_cached+update_friends_display+update_hide_from_search_engines+update_notifications)/10),
                                          agree_terms_check = NULL,
                                          agree_terms_uncheck = NULL,
                                          ajax_google_translate_description = NULL,
                                          ajax_google_translate_reviews = NULL,
                                          ajax_payout_edit = NULL,
                                          ajax_payout_options_by_country = NULL,
                                          ajax_photo_widget = NULL,
                                          ajax_photo_widget_form_iframe = NULL,
                                          ajax_referral_banner_experiment_type = NULL,
                                          ajax_referral_banner_type = NULL,
                                          cancellation_policies = NULL,
                                          cancellation_policy_click = NULL,
                                          create_ach = NULL,
                                          create_multiple = NULL,
                                          create_paypal = NULL,
                                          complete_redirect = NULL,
                                          complete_status = NULL,
                                          departments = NULL,
                                          edit_verification = NULL,
                                          email_itinerary_colorbox = NULL,
                                          email_share = NULL,
                                          email_wishlist = NULL,
                                          friends_new = NULL,
                                          home_safety_landing = NULL,
                                          home_safety_terms = NULL,
                                          jumio_redirect = NULL,
                                          jumio_token = NULL,
                                          kba_update = NULL,
                                          listings = NULL,
                                          message_to_host_change = NULL,
                                          message_to_host_focus = NULL,
                                          multi_message = NULL,
                                          qt_reply_v2 = NULL,
                                          qt_with = NULL,
                                          qt2 = NULL,
                                          reviews_new = NULL,
                                          similar_listings_v2 = NULL,
                                          show_code = NULL,
                                          social_connections = NULL,
                                          terms_and_conditions = NULL,
                                          transaction_history_paginated = NULL,
                                          travel_plans_current = NULL,
                                          travel_plans_previous = NULL,
                                          update_cached = NULL,
                                          update_friends_display = NULL,
                                          update_hide_from_search_engines = NULL,
                                          update_notifications = NULL
)]

# merging features with train and test data
X_train <- merge(X_train, sessions_action, all.x=T, by="id")
X_test <- merge(X_test, sessions_action, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_type, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_type, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_detail, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_detail, all.x=T, by="id")

# removing duplicate columns
X_train <- X_train[, colnames(unique(as.matrix(X_train), MARGIN=2)), with=F]
X_test <- X_test[, names(X_train), with=F]

# extracting ids and target
train_ids <- X_train$id
test_ids <- X_test$id

target <- as.numeric(as.factor(X_train$country_destination)) - 1

# feature engineering from raw features
X_train <- X_train[, ':='(year_account = as.integer(substr(date_account_created,1,4)),
                          month_account = as.integer(substr(date_account_created,6,7)),
                          day_account = as.integer(substr(date_account_created,9,10)),
                          weekday_account = weekdays(as.Date(paste0(substr(date_account_created,1,4),"-",substr(date_account_created,6,7),"-",substr(date_account_created,9,10)))),
                          year_active = as.integer(substr(timestamp_first_active,1,4)),
                          month_active = as.integer(substr(timestamp_first_active,5,6)),
                          day_active = as.integer(substr(timestamp_first_active,7,8)),
                          weekday_active = weekdays(as.Date(paste0(substr(timestamp_first_active,1,4),"-",substr(timestamp_first_active,5,6),"-",substr(timestamp_first_active,7,8)))),
                          age = ifelse(age < 15 | age > 100, -1, age),
                          id = NULL,
                          date_account_created = NULL,
                          timestamp_first_active = NULL,
                          date_first_booking = NULL,
                          country_destination = NULL)]

X_test <- X_test[, ':='(year_account = as.integer(substr(date_account_created,1,4)),
                        month_account = as.integer(substr(date_account_created,6,7)),
                        day_account = as.integer(substr(date_account_created,9,10)),
                        weekday_account = weekdays(as.Date(paste0(substr(date_account_created,1,4),"-",substr(date_account_created,6,7),"-",substr(date_account_created,9,10)))),
                        year_active = as.integer(substr(timestamp_first_active,1,4)),
                        month_active = as.integer(substr(timestamp_first_active,5,6)),
                        day_active = as.integer(substr(timestamp_first_active,7,8)),
                        weekday_active = weekdays(as.Date(paste0(substr(timestamp_first_active,1,4),"-",substr(timestamp_first_active,5,6),"-",substr(timestamp_first_active,7,8)))),
                        age = ifelse(age < 15 | age > 100, -1, age),
                        id = NULL,
                        date_account_created = NULL,
                        timestamp_first_active = NULL,
                        date_first_booking = NULL,
                        country_destination = NULL)]

# onehot encoding categorical variables
source("./Codes/categorical.R")
X_data <- encode_categories(X_train, X_test, onehot="all")

# preparing final datasets
X_train <- X_data$train
X_test <- X_data$test

X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0


## xgboost

# cross validation (uncomment the following and run if required)
#set.seed(23)
#model_xgb <- xgb.cv(data=as.matrix(X_train), label=as.matrix(target), objective="multi:softprob", num_class=12, nfold=5, nrounds=200, eta=0.05, max_depth=5, subsample=0.9, colsample_bytree=0.5, min_child_weight=5, eval_metric='mlogloss')

# CV: 0.900348

# model building
set.seed(23)
model_xgb <- xgboost(as.matrix(X_train), as.matrix(target), objective="multi:softprob", num_class=12, nrounds=200, eta=0.05, max_depth=5, subsample=0.9, colsample_bytree=0.5, min_child_weight=5, eval_metric='mlogloss')

# model scoring
pred <- predict(model_xgb, as.matrix(X_test))
pred_matrix <- as.data.frame(matrix(pred, nrow(X_test), 12, byrow=T))

# extracting top-5 countries
submit <- as.data.frame(t(apply(pred_matrix, 1, function(y) order(-y)[1:5])))
submit$id <- test_ids

# creating submission file
submit <- melt(submit, id_vars='id')
submit <- merge(submit, data.frame("value"=seq(1,12),"country"=c("AU","CA","DE","ES","FR","GB","IT","NDF","NL","other","PT","US")))

# saving submission
submit <- submit[order(submit$variable),c("id","country")]
write.csv(submit, "./submit.csv", row.names=F)

