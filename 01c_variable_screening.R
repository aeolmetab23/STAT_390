
# Variable Screening C ----------------------------------------------------

# Changing outcome to log(new_deaths), and logging all predictors

# Load package(s)
library(tidymodels)
library(tidyverse)
library(patchwork)
library(readr)
library(stacks)
library(caret)
library(skimr)
library(ggfortify)
library(doMC)
library(randomForest)
library(tictoc)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
registerDoMC(cores = 8)

# Load Cleaned Date
## Correlated columns, any values per million, thousand, hundred, and only rows with new cases reported
load("data/covid_clean.rda")

## Split Data
covid_split <- covid_clean %>% 
  mutate(
    new_deaths_log = log(new_deaths+1)
  ) %>% 
  filter(
    !is.na(new_deaths)
  ) %>% 
  initial_split(prop = 0.8, strata = new_deaths_log)

## Training and Testing
### Smaller Sized
covid_train <- training(covid_split) %>% 
  slice_sample(prop = 0.4)

covid_test <- testing(covid_split)

# V-Folds
covid_folds <- vfold_cv(
  covid_train, v = 5, repeats = 3, strata = new_deaths_log)

skim_without_charts(covid_train)

# Recipe
recipe <- recipe(new_deaths_log ~ ., data = covid_train) %>%
  update_role(where(is.character), date, new_deaths, new_role = "id_variable") %>% 
  step_corr(all_numeric_predictors())  %>%  # manually 
  step_nzv(all_numeric_predictors())  %>%  # manually
  step_impute_knn(all_predictors())  %>% 
  step_other(all_nominal_predictors())  %>% 
  step_dummy(all_nominal_predictors())  %>% 
  step_log(all_numeric_predictors(), offset = 1) %>% 
  step_normalize(all_numeric_predictors())

# recipe %>%
#   prep() %>%
#   bake(new_data = covid_train) %>%
#   View()

# Random Forest Variable Reduction ----
rf_model <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('ranger', importance = "impurity") %>%
  set_mode('regression')

## Grid ----
rf_params <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(range = c(2, 10)))
rf_grid <- grid_regular(rf_params, levels = 5)

## Workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe)

## Tuning ----
# Tune grid 
# clear and start timer
tic.clearlog()
tic("Random Forest Variable Screening (Cleaned, step_log)")

rf_tune_log <- rf_workflow %>% 
  tune_grid(
    metrics = metric_set(rmse, rsq, mae),  
    resamples = covid_folds,
    grid = rf_grid,
    control = control_grid(save_pred = TRUE,
                           save_workflow = TRUE,
                           parallel_over = "everything"))

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

var_screen_log_toc <- tibble(model = time_log[[1]]$msg,
                      runtime = time_log[[1]]$toc - time_log[[1]]$tic)

save(rf_tune_log, var_screen_log_toc, file = "results/rf_tune_log.rda")

load("results/rf_tune_log.rda")

## RMSE Fitting ----
autoplot(rf_tune_log, metric = "rmse")

rf_workflow_final <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune_log, metric = "rmse"))

rf_fit_rmse_log <- fit(rf_workflow_final, data = covid_train)

#### Estimated Parameters
parameter_graph_rmse_log <- rf_fit_rmse_log %>%
  extract_fit_parsnip() %>%
  vip::vip(scale = TRUE)
parameter_graph_rmse_log

parameter_table_rmse_log <- rf_fit_rmse_log %>%
  extract_fit_parsnip() %>%
  vip::vi(scale = TRUE) %>%
  as_tibble()
parameter_table_rmse_log

save(rf_fit_rmse_log, parameter_graph_rmse_log,
     parameter_table_rmse_log, file = "results/var_screen_rmse_log.rda")

## RSQ Fitting ----
autoplot(rf_tune_log, metric = "rsq")

rf_workflow_final <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune_log, metric = "rsq"))

rf_fit_rsq_log <- fit(rf_workflow_final, data = covid_train)

#### Estimated Parameters
parameter_graph_rsq_log <- rf_fit_rsq_log %>%
  extract_fit_parsnip() %>%
  vip::vip(scale = TRUE)
parameter_graph_rsq_log

parameter_table_rsq_log <- rf_fit_rsq_log %>%
  extract_fit_parsnip() %>%
  vip::vi(scale = TRUE) %>%
  as_tibble()
parameter_table_rsq_log

save(rf_fit_rsq_log, parameter_graph_rsq_log,
     parameter_table_rsq_log, file = "results/var_screen_rsq_log.rda")


## MAE Fitting ----
autoplot(rf_tune_log, metric = "mae")

rf_workflow_final <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune_log, metric = "mae"))

rf_fit_mae_log <- fit(rf_workflow_final, data = covid_train)

#### Estimated Parameters
parameter_graph_mae_log <- rf_fit_mae_log %>%
  extract_fit_parsnip() %>%
  vip::vip(scale = TRUE)
parameter_graph_mae_log

parameter_table_mae_log <- rf_fit_mae_log %>%
  extract_fit_parsnip() %>%
  vip::vi(scale = TRUE) %>%
  as_tibble()
parameter_table_mae_log

save(rf_fit_mae_log, parameter_graph_mae_log,
     parameter_table_mae_log, file = "results/var_screen_mae_log.rda")


# ----

# Examine on Testing Data ----
## Did not fit model on the entire training data as this was for feature selection
### still worth seeing some early results

## RMSE
covid_metric <- metric_set(rmse)

predict(rf_fit_rmse_log, new_data = covid_test) %>%
  bind_cols(covid_test %>% select(new_deaths_log)) %>%
  covid_metric(truth = new_deaths_log, estimate = .pred) # 0.538

## RSQ
covid_metric <- metric_set(rsq)

predict(rf_fit_rsq_log, new_data = covid_test) %>%
  bind_cols(covid_test %>% select(new_deaths_log)) %>%
  covid_metric(truth = new_deaths_log, estimate = .pred) # 0.941

## MAE
covid_metric <- metric_set(mae)

predict(rf_fit_mae_log, new_data = covid_test) %>%
  bind_cols(covid_test %>% select(new_deaths_log)) %>%
  covid_metric(truth = new_deaths_log, estimate = .pred) # 0.320
