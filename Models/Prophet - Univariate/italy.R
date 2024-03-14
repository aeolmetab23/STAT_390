
# Prophet: Italy -------------------------------------------------------

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
itl <- read_csv(here::here("Models/model_data/uni_v2/Italy_uni.csv"))

# Mutate Columns
itl <- itl %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(itl, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
itl_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
itl_fit <- prophet(itl_prophet, weekly.seasonality=TRUE, yearly.seasonality = TRUE)

# Make future Dataframe ----
itl_future <- make_future_dataframe(itl_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(itl_fit, itl_future)

# Plot Forecast ----
plot(itl_fit, forecast)

itl_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
Italy_Prophet_uni_Preds <- bind_cols(test, itl_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Plot Results ----
ggplot(Italy_Prophet_uni_Preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 1) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Italy Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

italy_metrics <- Italy_Prophet_uni_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

italy_metrics

Italy_Prophet_uni <- pivot_wider(italy_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Italy"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Italy_Prophet_uni

save(Italy_Prophet_uni, Italy_Prophet_uni_Preds, file = "Models/Prophet - Univariate/results/Italy_metrics.rda")

