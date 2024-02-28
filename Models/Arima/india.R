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
    date = ymd(date)
  ) %>% 
  arrange(date)

# Converting to Time Series Tibble
ind_ts <- as_tsibble(ind, index = date)

# splitting the data - 70% split
splits <- initial_time_split(ind_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Use gg_tsdisplay with the converted tsibble object
train %>% 
  gg_tsdisplay(y = new_cases, plot_type = "partial")

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

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

# Fit model with p = 2 and q = 4
ind_final_fit <- Arima(train_ts, order=c(2, 1, 4))

ind_final_fit %>%
  forecast() %>%
  autoplot()

ind_forecast <- forecast(ind_final_fit, 42)

autoplot(ind_forecast)

preds <- predict(ind_final_fit, n.ahead = 42)$pred

ind_preds <- bind_cols(test, preds) %>% 
  rename(preds = "...3")

ggplot(ind_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "India Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

india_metrics <- ind_preds %>% 
  covid_metrics(new_cases, estimate = preds)

india_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard    232278. 
# 2 mase    standard        60.6
# 3 mae     standard    222624. 

save(india_metrics, ind_preds, file = "Models/Arima/results/India_metrics.rda")

