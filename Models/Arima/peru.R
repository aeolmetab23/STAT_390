# Arima: Peru -------------------------------------------------------

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
peru <- read_csv(here::here("Models/model_data/uni_v2/Peru_uni.csv"))

# Mutate Columns
peru <- peru %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
peru_ts <- as_tsibble(peru, index = date)

# splitting the data - 70% split
splits <- initial_time_split(peru_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Now you can use gg_tsdisplay with the converted tsibble object
train %>% feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

peru_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(peru_fit) # Q* = 7.0825, df = 5, p-value = 0.2146

peru_fit$aic # 3690.353

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
results # Best Result: p = 1, q = 3

# Fit model with p = 1 and q = 3
peru_final_fit <- Arima(train_ts, order=c(1, 1, 3))
peru_final_fit %>% forecast() %>% autoplot()
peru_forecast <- forecast(peru_final_fit, 42)
autoplot(peru_forecast)

preds <- predict(peru_final_fit, n.ahead = 42)$pred

peru_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(peru_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Peru Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

peru_metrics <- peru_preds %>% 
  covid_metrics(new_cases, estimate = preds)

peru_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     23859. 
# 2 mase    standard        44.7
# 3 mae     standard     23466. 

save(peru_metrics, peru_preds, file = "Models/Arima/results/Peru_metrics.rda")

