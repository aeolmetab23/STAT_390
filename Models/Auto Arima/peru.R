# Auto Arima: Peru -------------------------------------------------------

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
Peru <- read_csv(here::here("Models/model_data/uni_v2/Peru_uni.csv"))

# Mutate Columns
Peru <- Peru %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
Peru_ts <- as_tsibble(Peru, index = date)

# Splits
splits <- initial_time_split(Peru_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
Peru_fit <- auto.arima(train_ts)

Peru_fit # order = c(1, 0, 3)

preds <- predict(Peru_fit, n.ahead = 42)$pred

Peru_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Peru_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Peru Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Peru_metrics <- Peru_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Peru_metrics

save(Peru_metrics, Peru_preds, file = "Models/Auto Arima/results/Peru_metrics.rda")
