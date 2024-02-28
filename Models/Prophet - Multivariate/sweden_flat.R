
# Prophet: Sweden -------------------------------------------------------

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
library(diceR)
library(tsibble)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
registerDoMC(cores = 8)

# Load data
covid <- read_csv(here::here("Models/model_data/covid_cleaner.csv"))

# Filter Country
Sweden <- covid %>% 
  filter(location == "Sweden") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(Sweden) # hand washing facilities completely missing, 

Sweden <- Sweden %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Sweden_clean <- Sweden %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(Sweden_clean, is.numeric)
Sweden_clean_scaled <- scale(Sweden_clean[, numeric_columns])

# After scaling, ensure `Sweden_data_scaled` is still a data frame or matrix
Sweden_clean_scaled <- as.data.frame(Sweden_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Sweden_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
Sweden_cleaner <- Sweden_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Sweden_ts <- as_tsibble(Sweden_cleaner, index = date)

# view seasonality trends
ggplot(Sweden_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Sweden"
  )

ggplot(Sweden_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Sweden"
  )

# Split Data
splits <- initial_time_split(Sweden_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns to fit Prophet ----
Sweden_train <- train %>% rename("ds" = date, "y" = new_cases)

# model
Sweden_model <- prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
)

# adding external regressors
Sweden_model = add_regressor(Sweden_model,"new_deaths", standardize = FALSE)
Sweden_model = add_regressor(Sweden_model,"icu_patients", standardize = FALSE)
Sweden_model = add_regressor(Sweden_model,'positive_rate', standardize = FALSE)
Sweden_model = add_regressor(Sweden_model,'new_vaccinations', standardize = FALSE)
Sweden_model = add_regressor(Sweden_model,'stringency_index', standardize = FALSE)
Sweden_model = add_regressor(Sweden_model,'excess_mortality', standardize = FALSE)

# Sweden_model <- add_country_holidays(Sweden_model, country_name = "Sweden")
## "Holidays in Sweden are not currently supported"

# model fitting with training data
Sweden_fit = fit.prophet(Sweden_model, df = Sweden_train)

# making future df
future_Sweden <- make_future_dataframe(Sweden_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_Sweden$new_deaths = head(Sweden_ts$new_deaths, nrow(future_Sweden))
future_Sweden$icu_patients = head(Sweden_ts$icu_patients, nrow(future_Sweden))
future_Sweden$positive_rate = head(Sweden_ts$positive_rate, nrow(future_Sweden))
future_Sweden$new_vaccinations = head(Sweden_ts$new_vaccinations, nrow(future_Sweden))
future_Sweden$stringency_index = head(Sweden_ts$stringency_index, nrow(future_Sweden))
future_Sweden$excess_mortality = head(Sweden_ts$excess_mortality, nrow(future_Sweden))

# Forecasting
Sweden_forecast <- predict(Sweden_fit, future_Sweden)

# Prophet Plots
dyplot.prophet(Sweden_fit, Sweden_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(Sweden_fit, Sweden_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
Sweden_future_preds <- Sweden_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Sweden_preds <- bind_cols(test, Sweden_future_preds) %>% 
  rename(preds = yhat)

# Metrics
# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

Sweden_metrics <- Sweden_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = Sweden_preds$new_cases, y_true = Sweden_preds$preds) # 1.234136

Sweden_metrics
# 1 rmse    standard     17558.  
# 2 mase    standard        48.8
# 3 mae     standard     12741.  
# 4 mape    standard         1.234136

