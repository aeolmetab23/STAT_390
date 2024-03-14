# Auto Arima: UK -------------------------------------------------------

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
UK <- read_csv(here::here("Models/model_data/uni_v2/United Kingdom_uni.csv"))

# Mutate Columns
UK <- UK %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
UK_ts <- as_tsibble(UK, index = date)

# Splits
splits <- initial_time_split(UK_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
UK_fit <- auto.arima(train_ts, d = 1)

UK_fit # order = c(1, 0, 3)

preds <- predict(UK_fit, n.ahead = 42)$pred

UK_Auto_Arima_Preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(UK_Auto_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "UK Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

UK_metrics <- UK_Auto_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

UK_fit # order = c(0, 1, 1)

UK_Auto_Arima <- pivot_wider(UK_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "UK",
    p = 0,
    q = 1,
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
UK_Auto_Arima

save(UK_Auto_Arima, UK_Auto_Arima_Preds, file = "Models/Auto Arima/results/UK_metrics.rda")
