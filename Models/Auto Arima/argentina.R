# Auto Arima: Argentina -------------------------------------------------------

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
argentina <- read_csv(here::here("Models/model_data/uni_v2/Argentina_uni.csv"))

# Mutate Columns
argentina <- argentina %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
argentina_ts <- as_tsibble(argentina, index = date)

# Splits
splits <- initial_time_split(argentina_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
argentina_fit <- auto.arima(train_ts, d = 1)

argentina_fit # order = c(1, 1, 4)

preds <- predict(argentina_fit, n.ahead = 42)$pred

Argentina_Auto_Arima_Preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Argentina_Auto_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Argentina Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

argentina_metrics <- Argentina_Auto_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

argentina_fit # order = c(1, 0, 3)

argentina_Auto_Arima <- pivot_wider(argentina_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Argentina",
    p = 0,
    q = 5,
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
argentina_Auto_Arima

save(argentina_Auto_Arima, Argentina_Auto_Arima_Preds, file = "Models/Auto Arima/results/Argentina_metrics.rda")
