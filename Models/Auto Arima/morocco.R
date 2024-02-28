# Arima: Morocco -------------------------------------------------------

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
morocco <- read_csv(here::here("Models/model_data/uni_v2/Morocco_uni.csv"))

# Mutate Columns
morocco <- morocco %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
morocco_ts <- as_tsibble(morocco, index = date)

# splitting the data - 70% split
splits <- initial_time_split(morocco_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Now you can use gg_tsdisplay with the converted tsibble object
train %>% feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

morocco_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(morocco_fit) # Q* = 4.2145, df = 5, p-value = 0.519

morocco_fit$aic # 3176.355

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
morocco_final_fit <- Arima(train_ts, order=c(2, 1, 3))
morocco_final_fit %>% forecast() %>% autoplot()
morocco_forecast <- forecast(morocco_final_fit, 42)
autoplot(morocco_forecast)

preds <- predict(morocco_final_fit, n.ahead = 42)$pred

morocco_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(morocco_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Morocco Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Morocco_metrics <- morocco_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Morocco_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard       6660.
# 2 mase    standard        152.
# 3 mae     standard       6488.

save(Morocco_metrics, morocco_preds, file = "Models/Auto Arima/results/Morocco_metrics.rda")

