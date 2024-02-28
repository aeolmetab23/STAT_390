
# XGBoost Setup -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(xgboost)
library(caret)
library(doMC)
library(tictoc)

# Load data
covid <- read_csv(here::here("Models/model_data/covid_cleaner.csv"))

# Filter Dates
covid_clean <- covid %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date < "2024-01-01", date >= "2020-03-08")

# Split Data
splits <- initial_time_split(covid_clean, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Folding(Resampling)
folds <- vfold_cv(train, v = 5, repeats = 3, strata = new_cases)

## Recipe
recipe <- recipe(new_cases ~ ., data = train) %>%
  update_role(date, tests_units, iso_code, continent, location, new_role = "id") %>% 
  step_impute_knn(neighbors = 5) %>% 
  step_normalize(all_numeric_predictors())

recipe %>%
  prep() %>%
  bake(new_data = NULL) %>%
  View()

# define model engine and workflow
bt_model <-
  boost_tree(learn_rate = tune(),
             min_n = tune(),
             mtry = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

# Check tuning parameters
hardhat::extract_parameter_set_dials(bt_model)

# set-up tuning grid ----
bt_params <- hardhat::extract_parameter_set_dials(bt_model) %>%
  update(mtry = mtry(range = c(2, 10)))

# define tuning grid
bt_grid <- grid_regular(bt_params, levels = 5)

bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(recipe)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("Boosted Tree")

covid_tune <- tune_grid(
  bt_workflow,
  resamples = folds,
  grid = bt_grid,
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE,
                         parallel_over = "everything"),
  metrics = metric_set(rmse, mase, mae)
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

bt_tictoc <- tibble(
  model = time_log[[1]]$msg,
  runtime = time_log[[1]]$toc - time_log[[1]]$tic)


# Write out results
save(covid_tune, bt_workflow, bt_tictoc, splits, file = "results/model_2.rda")

## Winning model fit

autoplot(covid_tune, metric = "rmse")
show_best(covid_tune, metric = "rmse")[1,]

covid_wflow <- bt_workflow %>%
  finalize_workflow(select_best(covid_tune, metric = "rmse"))

covid_fit <- fit(covid_wflow, train)

## Tibble of predicted and actual values
covid_pred <- test %>%
  bind_cols(predict(covid_fit, test)) %>%
  select(new_cases, .pred)

## Metric Set
covid_metrics <- metric_set(rmse, mase, mae)

# Apply Metrics
covid_metrics(covid_pred, truth = new_cases, estimate = .pred)

