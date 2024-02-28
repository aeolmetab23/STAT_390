# Auto Arima: Australia -------------------------------------------------------

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
australia <- read_csv(here::here("Models/model_data/uni_v2/Australia_uni.csv"))

# Mutate Columns
australia <- australia %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
australia_ts <- as_tsibble(australia, index = date)

# Splits
splits <- initial_time_split(australia_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
australia_fit <- auto.arima(train_ts)

australia_fit # order = c(1, 0, 3)

preds <- predict(australia_fit, n.ahead = 42)$pred

australia_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(australia_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Australia Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

australia_metrics <- australia_preds %>% 
  covid_metrics(new_cases, estimate = preds)

australia_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard      53684.
# 2 mase    standard        299.
# 3 mae     standard      52543.

save(australia_metrics, australia_preds, file = "Models/Auto Arima/results/Australia_metrics.rda")
