
# Prophet: Mexico -------------------------------------------------------

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
mex <- read_csv(here::here("data/country_data/univariate/Mexico_uni.csv"))

# Mutate Columns
mex <- mex %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(mex, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
mex_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
mex_fit <- prophet(mex_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
mex_future <- make_future_dataframe(mex_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(mex_fit, mex_future)

# Plot Forecast ----
plot(mex_fit, forecast)

mex_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
Mexico_Prophet_uni_Preds <- bind_cols(test, mex_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Plot Results ----
ggplot(Mexico_Prophet_uni_Preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Mexico Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

mexico_metrics <- Mexico_Prophet_uni_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

mexico_metrics

Mexico_Prophet_uni <- pivot_wider(mexico_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Mexico"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Mexico_Prophet_uni

save(Mexico_Prophet_uni, Mexico_Prophet_uni_Preds, file = "Models/Prophet - Univariate/results/Mexico_metrics.rda")
