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

# Now you can use gg_tsdisplay with the converted tsibble object
train %>% feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

aus_fit <- Arima(train_ts, order=c(4,1,4))

checkresiduals(aus_fit)
# Q* = 4.1097
# df = 3
# p-value = 0.2499

aus_fit$aic # 4035.741

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
aus_final_fit <- Arima(train_ts, order=c(1,1,5))

aus_final_fit %>%
  forecast() %>%
  autoplot()

aus_forecast <- forecast(fit, 42)

autoplot(aus_forecast)

preds <- predict(aus_final_fit, n.ahead = 42)$pred

aus_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(aus_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal()+
  labs(
    x = "Date",
    y = "New Cases",
    title = "Australia Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Australia_metrics <- aus_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Australia_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard    20219.  
# 2 mase    standard        6.31
# 3 mae     standard    17503. 

save(Australia_metrics, aus_preds, file = "Models/Arima/results/Australia_metrics.rda")

