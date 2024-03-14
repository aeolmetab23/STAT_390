
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
Peru_Prophet_uni_Preds <- bind_cols(test, peru_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Plot Results ----
ggplot(Peru_Prophet_uni_Preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Peru Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

peru_metrics <- Peru_Prophet_uni_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

peru_metrics

Peru_Prophet_uni <- pivot_wider(peru_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Peru"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Peru_Prophet_uni

save(Peru_Prophet_uni, Peru_Prophet_uni_Preds, file = "Models/Prophet - Univariate/results/Peru_metrics.rda")