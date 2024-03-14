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

# Splits
splits <- initial_time_split(ita_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

ita_fit <- Arima(train_ts, order=c(2,1,2))

checkresiduals(ita_fit) # Q* = 13.577, df = 6, p-value = 0.03474

ita_fit$aic # 4002.56

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
results # Best Result: p = 5, q = 3

# Fit model with p = 5 and q = 3
ita_final_fit <- Arima(train_ts, order=c(results$p[1], 1, results$q[1]))

ita_final_fit %>%
  forecast() %>%
  autoplot()

ita_forecast <- forecast(ita_final_fit, 42)

autoplot(ita_forecast)

preds <- predict(ita_final_fit, n.ahead = 42)$pred

Italy_Arima_Preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(Italy_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Italy Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

italy_metrics <- Italy_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Italy_Arima <- pivot_wider(italy_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Italy",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Italy_Arima

save(Italy_Arima, Italy_Arima_Preds, file = "Models/Arima/results/Italy_metrics.rda")

