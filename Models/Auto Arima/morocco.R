# Auto Arima: Morocco -------------------------------------------------------

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
Morocco <- read_csv(here::here("Models/model_data/uni_v2/Morocco_uni.csv"))

# Mutate Columns
Morocco <- Morocco %>% 
  mutate(date = ymd(date)) %>% 
  arrange(date)

# Convert to Time Series Tibble
Morocco_ts <- as_tsibble(Morocco, index = date)

# Splits
splits <- initial_time_split(Morocco_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# removing dates - making data univariate
train_ts <- as.ts(train$new_cases)

# Run Model
Morocco_fit <- auto.arima(train_ts)

Morocco_fit # order = c(1, 0, 3)

preds <- predict(Morocco_fit, n.ahead = 42)$pred

Morocco_preds <- bind_cols(test, preds) %>% 
  rename("preds" = "...3")

ggplot(Morocco_preds) +
  geom_line(mapping = aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(mapping = aes(x = date, y = preds), color = "red3", linewidth = 2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "New Cases",
    title = "Morocco Auto Arima",
    subtitle = "Predicited vs. Actual New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

Morocco_metrics <- Morocco_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Morocco_metrics


save(Morocco_metrics, Morocco_preds, file = "Models/Auto Arima/results/Morocco_metrics.rda")
