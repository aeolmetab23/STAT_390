
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
  "Model 4" = covid_tune_4,
  "Model 5" = covid_tune_5
  )
  
## RMSE
model_set %>% 
  autoplot(metric = "rmse", select_best = TRUE) +
  theme_minimal() +
  geom_text(aes(y = mean - 2500, label = wflow_id), angle = 90, hjust = 1) +
  # ylim(c(10000, 20000)) +
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
  # ylim(c(1500, 2500)) +
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
                         bt_tictoc_4,
                         bt_tictoc_5) %>% 
  mutate(wflow_id = c("Model 1",
                      "Model 2",
                      "Model 3",
                      "Model 4",
                      "Model 5"))

result_table <- merge(model_results, model_times) %>% 
  select(model, wflow_id, mean, runtime) %>% 
  rename(rmse = mean) %>% 
  arrange(rmse)

result_table

save(result_table, file = "Models/XGBoost/results/result_table.rda")



# Model 5 ----
autoplot(covid_tune_5, metric = "rmse")
show_best(covid_tune_5, metric = "rmse")[1,]

covid_wflow <- bt_workflow_5 %>%
  finalize_workflow(select_best(covid_tune_5, metric = "rmse"))

covid_fit <- fit(covid_wflow, train)

## Tibble of predicted and actual values
covid_pred <- test %>%
  select(date, location, new_cases, new_cases_log) %>% 
  bind_cols(predict(covid_fit, test)) %>%
  arrange(location, date)

colors <- c("Predicted" = "navyblue", "Actual" = "indianred")

covid_pred %>% 
  filter(location == "Sweden") %>% 
  ggplot() +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linetype = 2) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual")) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "Sweden Predictions") +
  scale_color_manual(values = colors)

## Metric Set
covid_metrics <- metric_set(rmse, mase, mae)

# Apply Metrics
covid_metrics(covid_pred, truth = new_cases_log, estimate = .pred)

# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard       1.06 
# 2 mase    standard       1.00 
# 3 mae     standard       0.543