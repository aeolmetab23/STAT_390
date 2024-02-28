
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

# Write out results
load("Models/XGBoost/results/model_1.rda")
load("Models/XGBoost/results/model_2.rda")
load("Models/XGBoost/results/model_3.rda")
load("Models/XGBoost/results/model_4.rda")
load("Models/XGBoost/results/splits.rda")

train <- training(splits)
test <- testing(splits)

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
  geom_text(aes(y = mean - 2500, label = wflow_id), angle = 90, hjust = 1) +
  ylim(c(10000, 20000)) +
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
  geom_text(aes(y = mean - 200, label = wflow_id), angle = 90, hjust = 1) +
  ylim(c(1500, 2500)) +
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

save(result_table_1, file = "model_info/result_table_1.rda")

result_table



# Model 1 ----

# Model 3 ----
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
