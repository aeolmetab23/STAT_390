# Auto Arima: Argentina -------------------------------------------------------

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
arg <- read_csv(here::here("Models/model_data/uni_v2/Argentina_uni.csv"))

# Mutate Columns
arg <- arg %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Converting to Time Series Tibble
arg_ts <- as_tsibble(arg, index = date)

# splitting the data - 70% split
splits <- initial_time_split(arg_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
arg_fit <- auto.arima(train_ts)

arg_fit # order = c(1, 0, 3)

preds <- predict(arg_fit, n.ahead = 42)$pred

arg_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(arg_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Argentina Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

argentina_metrics <- arg_preds %>% 
  covid_metrics(new_cases, estimate = preds)

argentina_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard      53684.
# 2 mase    standard        299.
# 3 mae     standard      52543.

save(argentina_metrics, arg_preds, file = "Models/Auto Arima/results/Argentina_metrics.rda")
