
# Prophet: Australia -------------------------------------------------------

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
library(randomForest)
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
aus <- read_csv(here::here("data/country_data/univariate/Australia_uni.csv"))

# Mutate Columns
aus <- aus %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(aus, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
aus_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
aus_fit <- prophet(aus_prophet, weekly.seasonality=TRUE, yearly.seasonality=TRUE)

# Make future Dataframe ----
aus_future <- make_future_dataframe(aus_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(aus_fit, aus_future)

# Plot Forecast ----
plot(aus_fit, forecast)

aus_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
Australia_Prophet_uni_Preds <- bind_cols(test, aus_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Plot Results ----
ggplot(Australia_Prophet_uni_Preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Australia Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

australia_metrics <- Australia_Prophet_uni_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Australia_Prophet_uni <- pivot_wider(australia_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Australia"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Australia_Prophet_uni

save(Australia_Prophet_uni, Australia_Prophet_uni_Preds,
     file = "Models/Prophet - Univariate/results/Australia_metrics.rda")
