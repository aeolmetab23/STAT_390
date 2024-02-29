
# Prophet: Malaysia -------------------------------------------------------

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
mal <- read_csv(here::here("Models/model_data/uni_v2/Malaysia_uni.csv"))

# Mutate Columns
mal <- mal %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(mal, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
mal_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
mal_fit <- prophet(mal_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
mal_future <- make_future_dataframe(mal_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(mal_fit, mal_future)

# Plot Forecast ----
plot(mal_fit, forecast)

mal_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
mal_preds <- bind_cols(test, mal_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(mal_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Malaysia Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

malaysia_metrics <- mal_preds %>% 
  covid_metrics(new_cases, estimate = preds)

malaysia_metrics

Malaysia_Prophet_uni <- pivot_wider(malaysia_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Malaysia"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Malaysia_Prophet_uni

save(Malaysia_Prophet_uni, mal_preds, file = "Models/Prophet - Univariate/results/Malaysia_metrics.rda")
