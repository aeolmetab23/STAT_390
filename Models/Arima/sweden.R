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

# Splits
splits <- initial_time_split(swd_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# Remove Date
train_ts <- as.ts(train$new_cases)

swd_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(swd_fit)
# Q* = 2.1625, df = 5, p-value = 0.8262

swd_fit$aic # 3554.075

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
results # Best Result: p = 2, q = 3

# Fit model with p = 2 and q = 3
swd_final_fit <- Arima(train_ts, order=c(results$p[1], 1, results$q[1]))

swd_final_fit %>%
  forecast() %>%
  autoplot()

swd_forecast <- forecast(swd_final_fit, 42)

autoplot(swd_forecast)

preds <- predict(swd_final_fit, n.ahead = 42)$pred

Sweden_Arima_Preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(Sweden_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Sweden Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

sweden_metrics <- Sweden_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Sweden_Arima <- pivot_wider(sweden_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Sweden",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Sweden_Arima

save(Sweden_Arima, Sweden_Arima_Preds, file = "Models/Arima/results/Sweden_metrics.rda")

# ggplot(Sweden_Arima_Preds) +
#   geom_line(mapping = aes(date, preds, color = "Predicted"), linewidth = 1) +
#   geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = 1) +
#   theme_minimal() +
#   labs(x = "Date",
#        y = "New Cases",
#        color = "",
#        title = "New Cases for Sweden") +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = colors)

