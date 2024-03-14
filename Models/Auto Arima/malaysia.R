# Auto Arima: Malaysia -------------------------------------------------------

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
library(modeltime)
library(lubridate)
library(timetk)
library(feasts)
library(tsibble)
library(forecast)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
registerDoMC(cores = 8)

# Load Data
Malaysia <- read_csv(here::here("Models/model_data/uni_v2/Malaysia_uni.csv"))

# Mutate Columns
Malaysia <- Malaysia %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
Malaysia_ts <- as_tsibble(Malaysia, index = date)

# Splits
splits <- initial_time_split(Malaysia_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
Malaysia_fit <- auto.arima(train_ts, d = 1)

Malaysia_fit # order = c(1, 0, 3)

preds <- predict(Malaysia_fit, n.ahead = 42)$pred

Malaysia_Auto_Arima_Preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Malaysia_Auto_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "New Cases for Malaysia"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Malaysia_metrics <- Malaysia_Auto_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Malaysia_fit # order = c(1, 0, 3)

Malaysia_Auto_Arima <- pivot_wider(Malaysia_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Malaysia",
    p = 2,
    q = 1,
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Malaysia_Auto_Arima

save(Malaysia_Auto_Arima, Malaysia_Auto_Arima_Preds, file = "Models/Auto Arima/results/Malaysia_metrics.rda")
