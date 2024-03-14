# Auto Arima: Sweden -------------------------------------------------------

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
Sweden <- read_csv(here::here("Models/model_data/uni_v2/Sweden_uni.csv"))

# Mutate Columns
Sweden <- Sweden %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
Sweden_ts <- as_tsibble(Sweden, index = date)

# Splits
splits <- initial_time_split(Sweden_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
Sweden_fit <- auto.arima(train_ts, d = 1)

Sweden_fit # order = c(2, 1, 1)

preds <- predict(Sweden_fit, n.ahead = 42)$pred

Sweden_Auto_Arima_Preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Sweden_Auto_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Sweden Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Sweden_metrics <- Sweden_Auto_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Sweden_fit # order = c(0, 1, 4)

Sweden_Auto_Arima <- pivot_wider(Sweden_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Sweden",
    p = 2,
    q = 1,
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Sweden_Auto_Arima

save(Sweden_Auto_Arima, Sweden_Auto_Arima_Preds, file = "Models/Auto Arima/results/Sweden_metrics.rda")

# ggplot(Malaysia_preds) +
#   geom_line(mapping = aes(date, preds, color = "Predicted"), linewidth = 1) +
#   geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = 1) +
#   theme_minimal() +
#   labs(x = "Date",
#        y = "New Cases",
#        color = "",
#        title = "New Cases for Malaysia") +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = colors)
# 
# ggplot(Malaysia_preds) +
#   geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
#   geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 1) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "New Cases",
#     title = "New Cases for Malaysia"
#   )
