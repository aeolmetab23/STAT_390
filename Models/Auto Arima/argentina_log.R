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
Argentina <- read_csv(here::here("Models/model_data/uni_v2/Argentina_uni.csv"))

# Mutate Columns
Argentina <- Argentina %>% 
  mutate(
    date = ymd(date),
    new_cases_log = log(new_cases + 1)
  ) %>% 
  arrange(date)

# Convert to Time Series Tibble
Argentina_ts <- as_tsibble(Argentina, index = date)

# Splits
splits <- initial_time_split(Argentina_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases_log)

# Run Model
Argentina_fit <- auto.arima(train_ts, d = 1)

Argentina_fit # order = c(1, 0, 3)

preds <- predict(Argentina_fit, n.ahead = 42)$pred

Argentina_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...4") %>% 
  mutate(
    pred = exp(preds) - 1
  )

ggplot(Argentina_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = pred), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Argentina Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Argentina_metrics <- Argentina_preds %>% 
  covid_metrics(new_cases_log, estimate = preds)

Argentina_fit # order = c(1, 1, 2)

Argentina_Auto_Arima <- pivot_wider(Argentina_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Argentina",
    p = 1,
    q = 2,
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Argentina_Auto_Arima

# save(Argentina_Auto_Arima, Argentina_preds, file = "Models/Auto Arima/results/Argentina_metrics.rda")

colors <- c("Predicted" = "skyblue3", "Actual" = "indianred")

ggplot(Argentina_preds) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = 1) +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Cases for Argentina") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)
