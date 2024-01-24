# Load package(s)
library(tidymodels)
library(tidyverse)
library(patchwork)
library(readr)
library(stacks)
library(kableExtra)
library(caret)
library(skimr)
library(ggfortify)
library(doMC)
library(Boruta)
library(randomForest)
library(tictoc)


# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
registerDoMC(cores = 8)

## Split Data
covid_split <- cv19_complete %>% 
  filter(new_cases > 0) %>% 
  initial_split(prop = 0.8, strata = new_cases)

# Smaller Sized
model_data <- training(covid_split) %>% 
  slice_sample(prop = 0.4)

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
  update(mtry = mtry(range = c(2, 8)))
rf_grid <- grid_regular(rf_params, levels = 5)

## Workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe)

## Tuning ----
# Tune grid 
# clear and start timer
tic.clearlog()
tic("Random Forest Variable Screening")

rf_tune <- rf_workflow %>% 
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

save(rf_tune, file = "results/rf_tune.rda")

## Fitting ----
### RMSE
rf_workflow_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

rf_fit <- fit(rf_workflow_final, data = model_data)

#### Estimated Parameters
parameter_graph_rmse <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

parameter_graph_rmse

parameter_table_rmse <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  as_tibble()

parameter_table_rmse

save(parameter_graph_rmse, parameter_table_rmse, file = "results/var_screen_rmse.rda")

### RSQ
rf_workflow_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rsq"))

rf_fit <- fit(rf_workflow_final, data = model_data)

#### Estimated Parameters
parameter_graph_rsq <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

parameter_graph_rsq

parameter_table_rsq <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  as_tibble()

parameter_table_rsq

save(parameter_graph_rsq, parameter_table_rsq, file = "results/var_screen_rsq.rda")

### MAE
rf_workflow_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "mae"))

rf_fit <- fit(rf_workflow_final, data = model_data)

#### Estimated Parameters
parameter_graph_mae <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

parameter_graph_mae

parameter_table_mae <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  as_tibble()

parameter_table_mae

save(parameter_graph_mae, parameter_table_mae, file = "results/var_screen_mae.rda")
