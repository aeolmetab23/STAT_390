
# Variable Screening B ----------------------------------------------------

# Using data without correlated columns
## any values per million, thousand, hundred
### and only rows with new cases reported

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
  initial_split(prop = 0.8, strata = new_cases)

covid_test <- testing(covid_split)

# Smaller Sized
model_data <- training(covid_split) %>% 
  slice_sample(prop = 0.3)

model_folds <- vfold_cv(
  model_data, v = 5, repeats = 3, strata = new_cases)

skim_without_charts(model_data)

# Recipe
recipe <- recipe(new_cases ~ ., data = model_data) %>%
  update_role(where(is.character), date, new_role = "id_variable") %>% 
  step_corr(all_numeric_predictors())  %>%  # manually 
  step_nzv(all_numeric_predictors())  %>%  # manually
  step_impute_knn(all_predictors())  %>% 
  step_other(all_nominal_predictors())  %>% 
  step_dummy(all_nominal_predictors())  %>% 
  step_normalize(all_numeric_predictors())

# recipe %>%
#   prep() %>%
#   bake(new_data = model_data) %>%
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
tic("Random Forest Variable Screening (CLEANED)")

rf_tune_clean <- rf_workflow %>% 
  tune_grid(
    metrics = metric_set(rmse, rsq, mae),  
    resamples = model_folds,
    grid = rf_grid,
    control = control_grid(save_pred = TRUE,
                           save_workflow = TRUE,
                           parallel_over = "everything"))

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

var_screen_toc <- tibble(model = time_log[[1]]$msg,
                      runtime = time_log[[1]]$toc - time_log[[1]]$tic)

save(rf_tune_clean, var_screen_toc, file = "results/rf_tune_clean.rda")

## RMSE Fitting ----
autoplot(rf_tune_clean, metric = "rmse")

rf_workflow_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune_clean, metric = "rmse"))

rf_fit_rmse_clean <- fit(rf_workflow_final, data = model_data)

#### Estimated Parameters
parameter_graph_rmse_clean <- rf_fit_rmse_clean %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

parameter_table_rmse_clean <- rf_fit_rmse_clean %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  as_tibble()

save(rf_fit_rmse_clean, parameter_graph_rmse_clean,
     parameter_table_rmse_clean, file = "results/var_screen_rmse_clean.rda")

## RSQ Fitting ----
autoplot(rf_tune_clean, metric = "rsq")

rf_workflow_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune_clean, metric = "rsq"))

rf_fit_rsq_clean <- fit(rf_workflow_final, data = model_data)

#### Estimated Parameters
parameter_graph_rsq_clean <- rf_fit_rsq_clean %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

parameter_graph_rsq_clean

parameter_table_rsq_clean <- rf_fit_rsq_clean %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  as_tibble()

parameter_table_rsq_clean

save(rf_fit_rsq_clean, parameter_graph_rsq_clean,
     parameter_table_rsq_clean, file = "results/var_screen_rsq_clean.rda")


## MAE Fitting ----
autoplot(rf_tune_clean, metric = "mae")

rf_workflow_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune_clean, metric = "mae"))

rf_fit_mae_clean <- fit(rf_workflow_final, data = model_data)

#### Estimated Parameters
parameter_graph_mae_clean <- rf_fit_mae_clean %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

parameter_graph_mae_clean

parameter_table_mae_clean <- rf_fit_mae_clean %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  as_tibble()

parameter_table_mae_clean

save(rf_fit_mae_clean, parameter_graph_mae_clean,
     parameter_table_mae_clean, file = "results/var_screen_mae_clean.rda")


# ----

# Examine on Testing Data ----
## Did not fit model on the entire training data as this was for feature selection
### still worth seeing some early results

## RMSE
covid_metric <- metric_set(rmse)

predict(rf_fit_rmse_clean, new_data = covid_test) %>% 
  bind_cols(covid_test %>% select(new_cases)) %>% 
  covid_metric(truth = new_cases, estimate = .pred) # 120374 - Way too high

## RSQ
covid_metric <- metric_set(rsq)

predict(rf_fit_rsq_clean, new_data = covid_test) %>% 
  bind_cols(covid_test %>% select(new_cases)) %>% 
  covid_metric(truth = new_cases, estimate = .pred) # 0.516 - Moderate Effect Size, want > 0.7

## MAE
covid_metric <- metric_set(mae)

predict(rf_fit_mae_clean, new_data = covid_test) %>% 
  bind_cols(covid_test %>% select(new_cases)) %>% 
  covid_metric(truth = new_cases, estimate = .pred) # 7197 - Ideally want this lower too
