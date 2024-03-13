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
sd_roll_3 <- rollify(sd, window = 3)

# load data
data <- read.csv("data/covid_cleaner.csv") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date != "2024-01-07", date != "2024-01-01",
         date >= "2020-03-08") %>% 
  # add in additional lag and roll statistics
  mutate(new_cases_3week_lag = lag(new_cases, n=3, default=NA),
         new_cases_3week_roll = mean_roll_3(new_cases),
         new_cases_3week_sd_roll = sd_roll_3(new_cases)) %>% 
  # get rid of irrelevant columns
  select(-c(tests_per_case, population, human_development_index, aged_70_older, female_smokers,
            male_smokers, life_expectancy, cardiovasc_death_rate, extreme_poverty, gdp_per_capita,
            median_age, population_density, people_fully_vaccinated))

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
    nrounds = seq(80, 160),
    max_depth = seq(2,10), 
    colsample_bytree = 1, 
    eta = seq(.3,.5,.02), #learning rate
    gamma = 0,
    min_child_weight = 1,  
    subsample = 1)
)

# setting up model
xgb_model3 <- caret::train(
  x = xgtrain,
  y = outcome,
  trControl = xgb_train_control,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 4
)

best_params3 <- xgb_model3$bestTune

preds <- caret::predict.train(xgb_model3, xgtest)
test$Preds <- preds
df_withpreds3 <- bind_rows(train, test)

xgb_importance3 <- xgb.importance(
  feature_names = colnames(train %>% select(-c(date, tests_units, iso_code, continent, location, new_cases))),
  model = xgb_model3$finalModel
)


# Write out results
save(xgb_model3, best_params3, df_withpreds3, xgb_importance3, file = "results/xgb_tuned3.rda")


stopCluster(cl)
