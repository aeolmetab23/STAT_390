# Arima: Italy -------------------------------------------------------

# Load package(s)
library(tidymodels)
library(tidyverse)
library(patchwork)
library(readr)
library(stacks)
library(caret)
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
ita <- read_csv(here::here("Models/model_data/uni_v2/Italy_uni.csv"))

# Mutate Columns
ita <- ita %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
ita_ts <- as_tsibble(ita, index = date)

# splitting the data - 70% split
splits <- initial_time_split(ita_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Now you can use gg_tsdisplay with the converted tsibble object
train %>% feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

ita_fit <- Arima(train_ts, order=c(2,1,2))

checkresiduals(ita_fit) # Q* = 13.577, df = 6, p-value = 0.03474

ita_fit$aic # 4002.56

# Grid Search
ps <- seq(0:4)
qs <- seq(0:4)

## Create result tibble
results <- tibble(
  p = c(),
  q = c(),
  aic = c())

## Run Loop
for (p in ps) {
  for (q in qs) {
    fit <- Arima(train_ts, order = c(p,1,q))
    aic <- fit$aic
    results <- bind_rows(results, tibble(p = p, q = q, aic = aic)) %>% 
      arrange(aic)
  }
}
results # Best Result: p = 5, q = 3

# Fit model with p = 5 and q = 3
ita_final_fit <- Arima(train_ts, order = c(5, 1, 3))
ita_final_fit %>% forecast() %>% autoplot()
ita_forecast <- forecast(ita_final_fit, 42)
autoplot(ita_forecast)

preds <- predict(ita_final_fit, n.ahead = 42)$pred

ita_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(ita_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Italy Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

italy_metrics <- ita_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = ita_preds$new_cases, y_true = ita_preds$preds) # 0.8121392

italy_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard    117446. 
# 2 mase    standard        21.8
# 3 mae     standard    111501. 

save(italy_metrics, ita_preds, file = "Models/Arima/results/Italy_metrics.rda")

