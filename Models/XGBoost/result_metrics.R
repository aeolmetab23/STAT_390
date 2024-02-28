library(tidyverse)
library(tidymodels)
library(xgboost)
library(caret)
library(doMC)
library(tictoc)

# Write out results
load("Models/XGBoost/results/model_3.rda")
load("Models/XGBoost/results/splits.rda")

train <- training(splits)
test <- testing(splits)

## Winning model fit
autoplot(covid_tune_3, metric = "rmse")
show_best(covid_tune_3, metric = "rmse")[1,]

covid_wflow <- bt_workflow_3 %>%
  finalize_workflow(select_best(covid_tune_3, metric = "rmse"))

covid_fit <- fit(covid_wflow, train)

## Tibble of predicted and actual values
covid_pred <- test %>%
  bind_cols(predict(covid_fit, test)) %>%
  select(new_cases, .pred)

## Metric Set
covid_metrics <- metric_set(rmse, mase, mae)

# Apply Metrics
covid_metrics(covid_pred, truth = new_cases, estimate = .pred)
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard    76613.  
# 2 mase    standard        1.18
# 3 mae     standard     7124. 
