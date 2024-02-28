# Arima: Argentina -------------------------------------------------------

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
library(randomForest)
library(tictoc)
library(modeltime)
library(xgboost)
library(lubridate)
library(timetk)
library(prophet)
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
arg <- read_csv(here::here("Models/model_data/uni_v2/Argentina_uni.csv"))

# Mutate Columns
arg <- arg %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
arg_ts <- as_tsibble(arg, index = date)

# Split
splits <- initial_time_split(arg_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Plot Lags
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# Make into a TS
train_ts <- ts(train$new_cases, frequency = 52)

# Make initial model
arg_fit <- Arima(train_ts, order=c(2,1,1))

# Residuals
checkresiduals(arg_fit) # Q* = 17.26, df = 5, p-value = 0.004033

# Alkaline
arg_fit$aic # 3940.017

# Grid Search
p_loop <- seq(0:4)
q_loop <- seq(0:4)

# Create result tibble
results <- tibble(p = c(), q = c(), aic = c())

# Run Loop
for (p in p_loop) {
  for (q in q_loop) {
    fit <- Arima(train_ts, order = c(p,1,q))
    aic <- fit$aic
    results <- bind_rows(results, tibble(p = p, q = q, aic = aic)) %>% 
      arrange(aic)
  }
}
results

# running model with best result from grid search (p=1, q=5)
arg_final_fit <- Arima(train_ts, order=c(1,1,4))

arg_final_fit %>%
  forecast() %>%
  autoplot()

arg_forecast <- forecast(arg_final_fit, 42)

autoplot(arg_forecast)

preds <- predict(arg_final_fit, n.ahead = 42)$pred

arg_preds <- bind_cols(test, preds)

arg_preds <- arg_preds %>% 
  rename("preds" = "...3")

ggplot(arg_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Argentina Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Argentina_metrics <- arg_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Argentina_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard      53510.
# 2 mase    standard        297.
# 3 mae     standard      52340.

save(Argentina_metrics, arg_preds, file = "Models/Arima/results/Argentina_metrics.rda")

