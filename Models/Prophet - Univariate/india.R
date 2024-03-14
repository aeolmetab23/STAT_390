
# Prophet: Brazil -------------------------------------------------------

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
india <- read_csv(here::here("Models/model_data/uni_v2/India_uni.csv"))

# Mutate Columns
india <- india %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(india, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
india_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
india_fit <- prophet(india_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
india_future <- make_future_dataframe(india_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(india_fit, india_future)

# Plot Forecast ----
plot(india_fit, forecast)

india_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
India_Prophet_uni_Preds <- bind_cols(test, india_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Plot Results ----
ggplot(India_Prophet_uni_Preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "India Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

india_metrics <- India_Prophet_uni_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

india_metrics

India_Prophet_uni <- pivot_wider(india_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "India"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
India_Prophet_uni

save(India_Prophet_uni, India_Prophet_uni_Preds, file = "Models/Prophet - Univariate/results/India_metrics.rda")

