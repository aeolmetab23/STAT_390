library(tidyverse)
library(xgboost)
library(caret)
library(doParallel)

# setup parallel processing 
cl <- makePSOCKcluster(16)
registerDoParallel(cl)

# load data
data <- read.csv("data/covid_cleaner.csv") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date != "2024-01-07", date != "2024-01-01",
         date >= "2020-03-08")

# used for date filtering:
# data %>% 
#   group_by(date) %>% 
#   summarize(avg_cases = sum(new_cases)) %>% 
#   arrange(date) %>% 
#   print(n = 20)

# splitting the data - 70% split
n_distinct(data$date) * .7 # this is 140, corresponds to 2022-11-06
train <- data %>% 
  filter(date <= "2022-11-06")
test <- data %>% 
  filter(date > "2022-11-06")

############### formatting training data and testing data as matrices
outcome <- train$new_cases
# outcome_test <- test$new_cases
# convert train to matrix
xgtrain <- as.matrix(train %>% select(-c(date, tests_units, iso_code, continent, location, new_cases)))
# create xgb.DMatrix 
# xgtrain <- xgb.DMatrix(data = train_matrix, label = outcome)

# convert test to matrix
xgtest <- as.matrix(test %>% select(-c(date, tests_units, iso_code, continent, location, new_cases)))
# # create xgb.DMatrix 
# xgtest <- xgb.DMatrix(data = test_matrix, label = outcome_test)


# cross validation
xgb_train_control <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE
)

# parameter set
xgb_grid <- expand.grid(
  list(
    nrounds = seq(100, 200),
    max_depth = c(6, 15, 20), 
    colsample_bytree = 1, 
    eta = 0.3, #learning rate
    gamma = 0,
    min_child_weight = 1,  
    subsample = 1)
)


# setting up model
# xg_model <- xgboost(params = xgb_grid, )  < to try later
xgb_model <- caret::train(
  x = xgtrain,
  y = outcome,
  trControl = xgb_train_control,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 4
)


best_params <- xgb_model$bestTune

preds <- caret::predict.train(xgb_model, xgtest)
test$Preds <- preds
df_withpreds <- bind_rows(train, test)

xgb_importance <- xgb.importance(
  feature_names = colnames(train %>% select(-c(date, tests_units, iso_code, continent, location, new_cases))),
  model = xgb_model$finalModel
)


# Write out results
save(xgb_model, best_params, df_withpreds, xgb_importance, file = "results/xgb_tuned.rda")


stopCluster(cl)
