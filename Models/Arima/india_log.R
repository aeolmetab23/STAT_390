# Arima: India -------------------------------------------------------

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
ind <- read_csv(here::here("Models/model_data/uni_v2/India_uni.csv"))

# Mutate Columns
ind <- ind %>% 
  mutate(
    date = ymd(date),
    new_cases_log = log(new_cases + 1)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
ind_ts <- as_tsibble(ind, index = date)

# splitting the data - 70% split
splits <- initial_time_split(ind_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases_log, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases_log)

ind_fit <- Arima(train_ts, order=c(2,1,1))

checkresiduals(ind_fit)
# Q* = 18.382, df = 7, p-value = 0.01036

ind_fit$aic # 4227.352

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
results # Best Result: p = 2, q = 4

# Fit model with p = 1 and q = 4
ind_final_fit <- Arima(train_ts, order=c(results$p[1], 1, results$q[1]))

ind_final_fit %>%
  forecast() %>%
  autoplot()

ind_forecast <- forecast(ind_final_fit, 42)

autoplot(ind_forecast)

preds <- predict(ind_final_fit, n.ahead = 42)$pred

India_Arima_Preds <- bind_cols(test, preds) %>% 
  rename(preds = "...4") %>% 
  mutate(
    pred = exp(preds) - 1
  )

ggplot(India_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = pred), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "India Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

india_metrics <- India_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = pred)

India_Arima <- pivot_wider(india_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "India",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
India_Arima

# save(India_Arima, India_Arima_Preds, file = "Models/Arima/results/India_metrics.rda")

# n = 3
# exp(log(n+1)) - 1
