
# Prophet: Sweden -------------------------------------------------------

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
library(tictoc)
library(modeltime)
library(xgboost)
library(lubridate)
library(timetk)
library(prophet)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
registerDoMC(cores = 8)

# Load Data
swd <- read_csv(here::here("Models/model_data/uni_v2/Sweden_uni.csv"))

# Mutate Columns
swd <- swd %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(swd, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
swd_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
swd_fit <- prophet(swd_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
swd_future <- make_future_dataframe(swd_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(swd_fit, swd_future)

# Plot Forecast ----
plot(swd_fit, forecast)

swd_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
swd_preds <- bind_cols(test, swd_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(swd_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Sweden Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

sweden_metrics <- swd_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = swd_preds$new_cases, y_true = swd_preds$preds) # 1.07716

sweden_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     22577. 
# 2 mase    standard        80.2
# 3 mae     standard     20942.
