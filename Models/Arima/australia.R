# Arima: Australia -------------------------------------------------------

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
aus <- read_csv(here::here("data/country_data/univariate/Australia_uni.csv"))

# Mutate Columns
aus <- aus %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
aus_ts <- as_tsibble(aus, index = date)

# Splits
splits <- initial_time_split(aus_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

aus_fit <- Arima(train_ts, order=c(4,1,4))

checkresiduals(aus_fit)
# Q* = 4.1097
# df = 3
# p-value = 0.2499

aus_fit$aic # 4039.112

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

# running model with best result from grid search (p=2, q=2)
aus_final_fit <- Arima(train_ts, order=c(results$p[1], 1, results$q[1]))

aus_final_fit %>%
  forecast() %>%
  autoplot()

aus_forecast <- forecast(fit, 42)

autoplot(aus_forecast)

preds <- predict(aus_final_fit, n.ahead = 42)$pred

Australia_Arima_Preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Australia_Arima_Preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal()+
  labs(
    x = "Date",
    y = "New Cases",
    title = "Australia Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Australia_metrics <- Australia_Arima_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Australia_Arima <- pivot_wider(Australia_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Australia",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
Australia_Arima

save(Australia_Arima, Australia_Arima_Preds, file = "Models/Arima/results/Australia_metrics.rda")

# colors <- c("Predicted" = "skyblue3", "Actual" = "indianred")
# 
# ggplot() +
#   geom_line(aus_ts, mapping = aes(date, new_cases, color = "Actual"), linewidth = 1) +
#   geom_line(Australia_Arima_Preds, mapping = aes(date, preds, color = "Predicted"), linewidth = 1) +
#   # geom_line(Australia_Arima_Preds, mapping = aes(date, new_cases, color = "Actual"), linewidth = 1) +
#   theme_minimal() +
#   labs(x = "Date",
#        y = "New Cases",
#        color = "",
#        title = "New Cases for Australia") +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = colors)

owid

