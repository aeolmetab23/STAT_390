# Auto Arima: Mexico -------------------------------------------------------

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
Mexico <- read_csv(here::here("Models/model_data/uni_v2/Mexico_uni.csv"))

# Mutate Columns
Mexico <- Mexico %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
Mexico_ts <- as_tsibble(Mexico, index = date)

# Splits
splits <- initial_time_split(Mexico_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
Mexico_fit <- auto.arima(train_ts, d=1)

Mexico_fit # order = c(1, 0, 3)

preds <- predict(Mexico_fit, n.ahead = 42)$pred

Mexico_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Mexico_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Mexico Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Mexico_metrics <- Mexico_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Mexico_fit # order = c(3, 1, 1)

Mexico_Auto_Arima <- pivot_wider(Mexico_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Mexico",
    p = 3,
    q = 1,
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Mexico_Auto_Arima

save(Mexico_Auto_Arima, Mexico_preds, file = "Models/Auto Arima/results/Mexico_metrics.rda")
