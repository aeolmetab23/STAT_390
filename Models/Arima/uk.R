# Arima: United Kingdom -------------------------------------------------------

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
uk <- read_csv(here::here("Models/model_data/uni_v2/United Kingdom_uni.csv"))

# Mutate Columns
uk <- uk %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Tsibble
uk_ts <- as_tsibble(uk, index = date)

# Splits
splits <- initial_time_split(uk_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay 
gg_tsdisplay(train, y = new_cases, plot_type = "partial")

# Remove Dates
train_ts <- as.ts(train$new_cases)

uk_fit <- Arima(train_ts, order=c(2,1,3))

checkresiduals(uk_fit) # Q* = 7.0642, df = 5, p-value = 0.2159

uk_fit$aic # 4191.372

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
results # Best Result: p = 1, q = 2, aic = 4188

# Fit model with p = 1 and q = 2
uk_final_fit <- Arima(train_ts, order=c(1, 1, 2))

uk_final_fit %>%
  forecast() %>%
  autoplot()

uk_forecast <- forecast(uk_final_fit, 42)

autoplot(uk_forecast)

preds <- predict(uk_final_fit, n.ahead = 42)$pred

uk_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(uk_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "United Kingdom Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

uk_metrics <- uk_preds %>% 
  covid_metrics(new_cases, estimate = preds)

UK_Arima <- pivot_wider(uk_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "United Kingdom",
    p = results$p[1],
    q = results$q[1]
  ) %>% 
  select(
    location, p, q, rmse, mase, mae, .estimator
  )
UK_Arima

save(UK_Arima, uk_preds, file = "Models/Arima/results/UK_metrics.rda")

