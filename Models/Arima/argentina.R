# Arima: Argentina -------------------------------------------------------

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

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# Make into a TS
train_ts <- ts(train$new_cases, frequency = 52)

# Make initial model
arg_fit <- Arima(train_ts, order=c(2,1,1))

# Residuals
checkresiduals(arg_fit) # Q* = 17.26, df = 5, p-value = 0.004033

# Alkaline
arg_fit$aic # 2969.246

# Grid Search
p_loop <- seq(0:4)
q_loop <- seq(0:4)

# Create result tibble
results <- tibble(p = c(), q = c(), aic = c())

# Run Loop
for (p in p_loop) {
  for (q in q_loop) {
    fit <- Arima(train_ts, order = c(p, 1, q))
    aic <- fit$aic
    results <- bind_rows(results, tibble(p = p, q = q, aic = aic)) %>% 
      arrange(aic)
  }
}
results

# running model with best result from grid search (p=1, q=4)
arg_final_fit <- Arima(train_ts, order=c(results$p[1], 1, results$q[1]))

arg_final_fit %>%
  forecast() %>%
  autoplot()

arg_forecast <- forecast(arg_final_fit, 42)

autoplot(arg_forecast)

preds <- predict(arg_final_fit, n.ahead = 42)$pred

Argentina_Arima_Preds <- bind_cols(test, preds)

Argentina_Arima_Preds <- Argentina_Arima_Preds %>% 
  rename("preds" = "...3")

ggplot(Argentina_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Argentina Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Argentina_metrics <- Argentina_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)
  
Argentina_Arima <- pivot_wider(Argentina_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Argentina",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Argentina_Arima

save(Argentina_Arima, Argentina_Arima_Preds, file = "Models/Arima/results/Argentina_metrics.rda")

