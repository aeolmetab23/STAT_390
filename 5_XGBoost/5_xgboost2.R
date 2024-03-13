library(tidyverse)
library(xgboost)
library(caret)
library(doParallel)
library(tibbletime)

# setup parallel processing 
cl <- makePSOCKcluster(16)
registerDoParallel(cl)

# roll function
mean_roll_3 <- rollify(mean, window = 3)

# load data
data <- read.csv("data/covid_cleaner.csv") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date != "2024-01-07", date != "2024-01-01",
         date >= "2020-03-08") %>% 
  # add in additional lag and roll statistics
  mutate(new_cases_3week_lag = lag(new_cases, n=3, default=NA),
         new_cases_3week_roll = mean_roll_3(new_cases)) %>% 
  # get rid of irrelevant columns
  select(-c(tests_per_case, population, human_development_index, aged_70_older, female_smokers,
            male_smokers, life_expectancy, cardiovasc_death_rate, extreme_poverty))

# used for date filtering:
# data %>% 
#   group_by(date) %>% 
#   summarize(avg_cases = sum(new_cases)) %>% 
#   arrange(date) %>% 
#   print(n = 20)

# splitting the data - 80% split
n_distinct(data$date) * .8 # this is 160, corresponds to 2023-03-26
train <- data %>% 
  filter(date <= "2023-03-26")
test <- data %>% 
  filter(date > "2023-03-26")

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
    nrounds = seq(50, 120),
    max_depth = c(4, 6, 8, 10, 12), 
    colsample_bytree = 1, 
    eta = c(.25,0.3,.35), #learning rate
    gamma = 0,
    min_child_weight = 1,  
    subsample = 1)
)


# setting up model
# xg_model <- xgboost(params = xgb_grid, )  < to try later
xgb_model2 <- caret::train(
  x = xgtrain,
  y = outcome,
  trControl = xgb_train_control,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 4
)


best_params2 <- xgb_model2$bestTune

preds <- caret::predict.train(xgb_model2, xgtest)
test$Preds <- preds
df_withpreds2 <- bind_rows(train, test)

xgb_importance2 <- xgb.importance(
  feature_names = colnames(train %>% select(-c(date, tests_units, iso_code, continent, location, new_cases))),
  model = xgb_model2$finalModel
)


# Write out results
save(xgb_model2, best_params2, df_withpreds2, xgb_importance2, file = "results/xgb_tuned2.rda")


stopCluster(cl)
