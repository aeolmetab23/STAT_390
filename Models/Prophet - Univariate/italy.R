
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
itl_fit <- prophet(itl_prophet, weekly.seasonality=TRUE)

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
itl_preds <- bind_cols(test, itl_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(itl_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Italy Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

italy_metrics <- itl_preds %>% 
  covid_metrics(new_cases, estimate = preds)

italy_metrics

# # A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard     94789. 
# 2 mase    standard        14.9
# 3 mae     standard     76114. 

save(italy_metrics, itl_preds, file = "Models/Prophet - Univariate/results/Italy_metrics.rda")
