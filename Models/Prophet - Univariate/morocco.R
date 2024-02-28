
# Prophet: Morocco -------------------------------------------------------

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
mor <- read_csv(here::here("Models/model_data/uni_v2/Morocco_uni.csv"))

# Mutate Columns
mor <- mor %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(mor, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
mor_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
mor_fit <- prophet(mor_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
mor_future <- make_future_dataframe(mor_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(mor_fit, mor_future)

# Plot Forecast ----
plot(mor_fit, forecast)

mor_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
mor_preds <- bind_cols(test, mor_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(mor_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Morocco Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

morocco_metrics <- mor_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = mor_preds$new_cases, y_true = mor_preds$preds) # 0.9783153

morocco_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard       5911.
# 2 mase    standard        101.
# 3 mae     standard       4326.
