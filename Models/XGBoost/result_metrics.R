
# Analysis ----------------------------------------------------------------

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
  mutate(date = lubridate::ymd(date),
         new_cases_log = log(new_cases + 1)) %>% 
  filter(date < "2024-01-01", date >= "2020-03-08")

# Train and Test
train <- covid_clean %>% 
  filter(date <= "2023-03-26")

test <- covid_clean %>% 
  filter(date > "2023-03-26")

# Write out results
load("Models/XGBoost/results/model_1.rda")
load("Models/XGBoost/results/model_2.rda")
load("Models/XGBoost/results/model_3.rda")
load("Models/XGBoost/results/model_4.rda")
load("Models/XGBoost/results/model_5.rda")

# Analysis ----
model_set <- as_workflow_set(
  "Model 1" = covid_tune_1,
  "Model 2" = covid_tune_2,
  "Model 3" = covid_tune_3,
  "Model 4" = covid_tune_4
  )
  
## RMSE
model_set %>% 
  autoplot(metric = "rmse", select_best = TRUE) +
  theme_minimal() +
  geom_text(aes(y = mean - 4500, label = wflow_id), angle = 90, hjust = 1) +
  ylim(c(15000, 29000)) +
  theme(legend.position = "none")

## MASE
model_set %>% 
  autoplot(metric = "mase", select_best = TRUE) +
  theme_minimal() +
  geom_text(aes(y = mean - 0.02, label = wflow_id), angle = 90, hjust = 1) +
  ylim(c(0, 0.5)) +
  theme(legend.position = "none")

## MAE
model_set %>% 
  autoplot(metric = "mae", select_best = TRUE) +
  theme_minimal() +
  geom_text(aes(y = mean - 300, label = wflow_id), angle = 90, hjust = 1) +
  ylim(c(3000, 4000)) +
  theme(legend.position = "none")


model_results <- model_set %>% 
  group_by(wflow_id) %>% 
  mutate(best = map(result, show_best, metric = "rmse", n = 1)) %>% 
  select(best) %>% 
  unnest(cols = c(best))


# Computation time
model_times <- bind_rows(bt_tictoc_1,
                         bt_tictoc_2,
                         bt_tictoc_3,
                         bt_tictoc_4) %>% 
  mutate(wflow_id = c("Model 1",
                      "Model 2",
                      "Model 3",
                      "Model 4"))

result_table <- merge(model_results, model_times) %>% 
  select(model, wflow_id, mean, runtime) %>% 
  rename(rmse = mean) %>% 
  arrange(rmse)

result_table

save(result_table, file = "Models/XGBoost/results/result_table.rda")

# Model 3 ----
autoplot(covid_tune_3, metric = "rmse")
show_best(covid_tune_3, metric = "rmse")[1,]

covid_wflow <- bt_workflow_3 %>%
  finalize_workflow(select_best(covid_tune_3, metric = "rmse"))

covid_fit <- fit(covid_wflow, train)

## Tibble of predicted and actual values
covid_pred_3 <- test %>%
  select(date, location, new_cases, new_cases_log) %>% 
  bind_cols(predict(covid_fit, test)) %>%
  arrange(location, date)

## Metric Set
covid_metrics <- metric_set(rmse, mase, mae)

# Apply Metrics
pred_3_metrics <- covid_metrics(covid_pred_3, truth = new_cases, estimate = .pred)
pred_3_metrics

save(covid_pred_3, pred_3_metrics, file = "Models/XGBoost/results/preds_3.rda")
# Model 4 ----
autoplot(covid_tune_4, metric = "rmse")
show_best(covid_tune_4, metric = "rmse")[1,]

covid_wflow <- bt_workflow_4 %>%
  finalize_workflow(select_best(covid_tune_4, metric = "rmse"))

covid_fit <- fit(covid_wflow, train)

## Tibble of predicted and actual values
covid_pred_4 <- test %>%
  select(date, location, new_cases, new_cases_log) %>% 
  bind_cols(predict(covid_fit, test)) %>%
  arrange(location, date)

## Metric Set
covid_metrics <- metric_set(rmse, mase, mae)

# Apply Metrics
pred_4_metrics <- covid_metrics(covid_pred_4, truth = new_cases, estimate = .pred)
pred_4_metrics

save(covid_pred_4, pred_4_metrics, file = "Models/XGBoost/results/preds_4.rda")


# Model 5 ----
autoplot(covid_tune_5, metric = "rmse")
show_best(covid_tune_5, metric = "rmse")[1,]

covid_wflow <- bt_workflow_5 %>%
  finalize_workflow(select_best(covid_tune_5, metric = "rmse"))

covid_fit <- fit(covid_wflow, train)

## Tibble of predicted and actual values
covid_pred_5 <- test %>%
  select(date, location, new_cases, new_cases_log) %>% 
  bind_cols(predict(covid_fit, test)) %>%
  arrange(location, date)

## Metric Set
covid_metrics <- metric_set(rmse, mase, mae)

# Apply Metrics
pred_5_metrics <- covid_metrics(covid_pred_5, truth = new_cases_log, estimate = .pred)

save(covid_pred_5, pred_5_metrics, file = "Models/XGBoost/results/preds_5.rda")