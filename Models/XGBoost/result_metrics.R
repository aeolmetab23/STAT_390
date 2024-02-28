library(tidyverse)
library(tidymodels)
library(xgboost)
library(caret)
library(doMC)
library(tictoc)

# Write out results
load("Models/XGBoost/results/model_3.rda")

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