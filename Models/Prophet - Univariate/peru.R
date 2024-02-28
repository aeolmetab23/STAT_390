
# Prophet: Peru -------------------------------------------------------

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
peru <- read_csv(here::here("Models/model_data/uni_v2/Peru_uni.csv"))

# Mutate Columns
peru <- peru %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(peru, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
peru_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
peru_fit <- prophet(peru_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
peru_future <- make_future_dataframe(peru_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(peru_fit, peru_future)

# Plot Forecast ----
plot(peru_fit, forecast)

peru_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
peru_preds <- bind_cols(test, peru_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(peru_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Peru Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

peru_metrics <- peru_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = peru_preds$new_cases, y_true = peru_preds$preds) # 0.9544694

peru_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     17222. 
# 2 mase    standard        27.4
# 3 mae     standard     14360. 
