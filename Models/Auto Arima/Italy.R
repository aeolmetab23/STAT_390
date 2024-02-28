# Auto Arima: Italy -------------------------------------------------------

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
Italy <- read_csv(here::here("Models/model_data/uni_v2/Italy_uni.csv"))

# Mutate Columns
Italy <- Italy %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
Italy_ts <- as_tsibble(Italy, index = date)

# Splits
splits <- initial_time_split(Italy_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
Italy_fit <- auto.arima(train_ts)

Italy_fit # order = c(1, 0, 3)

preds <- predict(Italy_fit, n.ahead = 42)$pred

Italy_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Italy_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "red4", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Italy Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Italy_metrics <- Italy_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Italy_metrics

save(Italy_metrics, Italy_preds, file = "Models/Auto Arima/results/Italy_metrics.rda")
