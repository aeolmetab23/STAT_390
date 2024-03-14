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
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Converting to Time Series Tibble
peru_ts <- as_tsibble(peru, index = date)

# Splits
splits <- initial_time_split(peru_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# Remove Date
train_ts <- as.ts(train$new_cases)

peru_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(peru_fit) # Q* = 7.0825, df = 5, p-value = 0.2146

peru_fit$aic # 3690.353

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
results # Best Result: p = 1, q = 3

# Fit model with p = 1 and q = 3
peru_final_fit <- Arima(train_ts, order=c(results$p[1], 1, results$q[1]))

peru_final_fit %>% forecast() %>% autoplot()

peru_forecast <- forecast(peru_final_fit, 42)

autoplot(peru_forecast)

preds <- predict(peru_final_fit, n.ahead = 42)$pred

Peru_Arima_Preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(Peru_Arima_Preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Peru Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

peru_metrics <- Peru_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Peru_Arima <- pivot_wider(peru_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Peru",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Peru_Arima

save(Peru_Arima, Peru_Arima_Preds, file = "Models/Arima/results/Peru_metrics.rda")

# colors <- c("Predicted" = "skyblue3", "Actual" = "indianred")
# 
# ggplot(Peru_Arima_Preds) +
#   geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
#   geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "New Cases",
#     title = "Peru Arima",
#     subtitle = "Predicited vs. Actual New Cases"
#   )
# 
# 
# ggplot(Peru_Arima_Preds) +
#   geom_line(mapping = aes(date, preds, color = "Predicted"), linewidth = 1) +
#   geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = 1) +
#   theme_minimal() +
#   labs(x = "Date",
#        y = "New Cases",
#        color = "",
#        title = "New Cases for Peru") +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = colors)

