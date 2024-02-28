# Arima: Sweden -------------------------------------------------------

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
swd <- read_csv(here::here("Models/model_data/uni_v2/Sweden_uni.csv"))

# Mutate Columns
swd <- swd %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
swd_ts <- as_tsibble(swd, index = date)

# splitting the data - 70% split
splits <- initial_time_split(swd_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Now you can use gg_tsdisplay with the converted tsibble object
train %>% feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

swd_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(swd_fit)
# Q* = 2.1625, df = 5, p-value = 0.8262

swd_fit$aic # 3554.075

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
results # Best Result: p = 2, q = 3

# Fit model with p = 2 and q = 3
swd_final_fit <- Arima(train_ts, order=c(2, 1, 3))
swd_final_fit %>% forecast() %>% autoplot()
swd_forecast <- forecast(swd_final_fit, 42)
autoplot(swd_forecast)

preds <- predict(swd_final_fit, n.ahead = 42)$pred

swd_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(swd_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Sweden Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

sweden_metrics <- swd_preds %>% 
  covid_metrics(new_cases, estimate = preds)

sweden_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     13679. 
# 2 mase    standard        51.2
# 3 mae     standard     13377. 

save(sweden_metrics, swd_preds, file = "Models/Auto Arima/results/Sweden_metrics.rda")

