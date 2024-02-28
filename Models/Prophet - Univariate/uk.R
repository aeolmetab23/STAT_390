
# Prophet: United Kingdom -------------------------------------------------------

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
uk <- read_csv(here::here("Models/model_data/uni_v2/United Kingdom_uni.csv"))

# Mutate Columns
uk <- uk %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(uk, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
uk_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
uk_fit <- prophet(uk_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
uk_future <- make_future_dataframe(uk_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(uk_fit, uk_future)

# Plot Forecast ----
plot(uk_fit, forecast)

uk_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
uk_preds <- bind_cols(test, uk_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(uk_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "United Kingdom Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

uk_metrics <- uk_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = uk_preds$new_cases, y_true = uk_preds$preds) # 1.059035

uk_metrics
# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     308475.
# 2 mase    standard        157.
# 3 mae     standard     296086.
