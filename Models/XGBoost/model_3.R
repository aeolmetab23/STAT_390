
# XGBoost Setup -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(xgboost)
library(caret)
library(doMC)
library(tictoc)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
registerDoMC(cores = 8)

# Load data
covid <- read_csv(here::here("Models/model_data/covid_cleaner.csv"))

# Filter Dates
covid_clean <- covid %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date < "2024-01-01", date >= "2020-03-08")

# Train and Test
train <- covid_clean %>% 
  filter(date <= "2023-03-26")

test <- covid_clean %>% 
  filter(date > "2023-03-26")

# Folding(Resampling)
folds <- vfold_cv(train, v = 10, repeats = 5, strata = new_cases)

## Recipe
recipe_3 <- recipe(new_cases ~ ., data = train) %>%
  update_role(date, tests_units, iso_code, continent, location, new_role = "id") %>% 
  step_impute_knn(neighbors = 5) %>% 
  step_corr(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_nzv(all_predictors())

recipe_3 %>%
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

bt_workflow_3 <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(recipe_3)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("Boosted Tree")

covid_tune_3 <- tune_grid(
  bt_workflow_3,
  resamples = folds,
  grid = bt_grid,
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE,
                         parallel_over = "everything"),
  metrics = metric_set(rmse, mase, mae)
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

bt_tictoc_3 <- tibble(
  model = time_log[[1]]$msg,
  runtime = time_log[[1]]$toc - time_log[[1]]$tic)


# Write out results
save(covid_tune_3, bt_workflow_3, bt_tictoc_3, file = "Models/XGBoost/results/model_3.rda")


