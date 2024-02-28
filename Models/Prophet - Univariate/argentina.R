
# Prophet: Argentina -------------------------------------------------------

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
arg <- read_csv(here::here("Models/model_data/uni_v2/Argentina_uni.csv"))

# Mutate Columns
arg <- arg %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

# splitting the data - 80% split
splits <- initial_time_split(arg, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
arg_prophet <- train %>% 
  rename("ds" = date, "y" = new_cases)

# Fit Prophet Model ----
arg_fit <- prophet(arg_prophet, weekly.seasonality=TRUE)

# Make future Dataframe ----
arg_future <- make_future_dataframe(arg_fit, period = 42, freq = "week")

# Forecast the Future ----
forecast <- predict(arg_fit, arg_future)

# Plot Forecast ----
plot(arg_fit, forecast)

arg_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

# Bind Results and Test Data
arg_preds <- bind_cols(test, arg_future_preds) %>% 
  rename(preds = yhat)

# Plot Results ----
ggplot(arg_preds) +
  geom_line(aes(x = date, y = new_cases), color = "skyblue", linewidth = 2) +
  geom_line(aes(x = date, y = preds), color = "indianred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Argentina Forecast",
    subtitle = "Predicted vs. Actual Results",
    x = "Date",
    y = "New Cases"
  )

# Metrics ----
covid_metrics <- metric_set(rmse, mase, mae)

argentina_metrics <- arg_preds %>% 
  covid_metrics(new_cases, estimate = preds)

argentina_metrics
# 1 rmse    standard     54913.  
# 2 mase    standard       259.
# 3 mae     standard     45620.  
# 4 mape    standard         1.035586

save(argentina_metrics, arg_preds, file = "Models/Prophet - Univariate/results/Argentina_metrics.rda")
