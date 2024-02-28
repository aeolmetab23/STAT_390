# Arima: Mexico -------------------------------------------------------

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
mex <- read_csv(here::here("Models/model_data/uni_v2/Mexico_uni.csv"))

# Mutate Columns
mex <- mex %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
mex_ts <- as_tsibble(mex, index = date)

# splitting the data - 70% split
splits <- initial_time_split(mex_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Now you can use gg_tsdisplay with the converted tsibble object
train %>% feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

mex_fit <- Arima(train_ts, order=c(3,1,2))

checkresiduals(mex_fit)
# Q* = 6.4455, df = 5, p-value = 0.2652

mex_fit$aic # 3745.909

# Grid Search
ps <- seq(0:4)
qs <- seq(0:4)

## Create result tibble
results <- tibble(
  p = c(),
  q = c(),
  aic = c())

## Run Loop
for (p in ps) {
  for (q in qs) {
    fit <- Arima(train_ts, order = c(p,1,q))
    aic <- fit$aic
    results <- bind_rows(results, tibble(p = p, q = q, aic = aic)) %>% 
      arrange(aic)
  }
}
results # Best Result: p = 1, q = 4

# Fit model with p = 1 and q = 4
mex_final_fit <- Arima(train_ts, order=c(1, 1, 4))
mex_final_fit %>% forecast() %>% autoplot()
mex_forecast <- forecast(mex_final_fit, 42)
autoplot(mex_forecast)

preds <- predict(mex_final_fit, n.ahead = 42)$pred

mex_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(mex_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Mexico Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

mexico_metrics <- mex_preds %>% 
  covid_metrics(new_cases, estimate = preds)

mexico_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     38298. 
# 2 mase    standard        35.1
# 3 mae     standard     37296. 

save(mexico_metrics, mex_preds, file = "Models/Arima/results/Mexico_metrics.rda")

