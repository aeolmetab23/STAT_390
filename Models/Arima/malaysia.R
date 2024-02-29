# Arima: Malaysia -------------------------------------------------------

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
mal <- read_csv(here::here("Models/model_data/uni_v2/Malaysia_uni.csv"))

# Mutate Columns
mal <- mal %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
mal_ts <- as_tsibble(mal, index = date)

# Splits
splits <- initial_time_split(mal_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

mal_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(mal_fit) # Q* = 18.122, df = 5, p-value = 0.002797

mal_fit$aic # 3455.621

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
results # Best Result: p = 4, q = 3, aic = 3441

# Fit model with p = 4 and q = 3
mal_final_fit <- Arima(train_ts, order=c(4, 1, 3))

mal_final_fit %>% forecast() %>% autoplot()

mal_forecast <- forecast(mal_final_fit, 42)

autoplot(mal_forecast)

preds <- predict(mal_final_fit, n.ahead = 42)$pred

Malaysia_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(Malaysia_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Malaysia Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

Malaysia_metrics <- Malaysia_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Malaysia_Arima <- pivot_wider(Malaysia_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Malaysia",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Malaysia_Arima

save(Malaysia_Arima, Malaysia_preds, file = "Models/Arima/results/Malaysia_metrics.rda")
