
# Prophet: uk -------------------------------------------------------

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
uk <- covid %>% 
  filter(location == "United Kingdom") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(uk) # hand washing facilities completely missing, 

uk <- uk %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
uk_clean <- uk %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(uk_clean, is.numeric)
uk_clean_scaled <- scale(uk_clean[, numeric_columns])

# After scaling, ensure `uk_data_scaled` is still a data frame or matrix
uk_clean_scaled <- as.data.frame(uk_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(uk_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
uk_cleaner <- uk_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

uk_ts <- as_tsibble(uk_cleaner, index = date)

# view seasonality trends
ggplot(uk_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: uk"
  )

ggplot(uk_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: uk"
  )

# Split Data
splits <- initial_time_split(uk_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns to fit Prophet ----
uk_train <- train %>% rename("ds" = date, "y" = new_cases)

# model
uk_model <- prophet(df = NULL, growth = "linear", yearly.seasonality = FALSE,
                    weekly.seasonality = TRUE, daily.seasonality = FALSE,
                    seasonality.mode = "additive", fit = TRUE)

# adding external regressors
uk_model = add_regressor(uk_model,"new_deaths", standardize = FALSE)
uk_model = add_regressor(uk_model,"icu_patients", standardize = FALSE)
uk_model = add_regressor(uk_model,'positive_rate', standardize = FALSE)
uk_model = add_regressor(uk_model,'new_vaccinations', standardize = FALSE)
uk_model = add_regressor(uk_model,'stringency_index', standardize = FALSE)
uk_model = add_regressor(uk_model,'excess_mortality', standardize = FALSE)

# uk_model <- add_country_holidays(uk_model, country_name = "uk")
## "Holidays in uk are not currently supported"

# model fitting with training data
uk_fit = fit.prophet(uk_model, df = uk_train)

# making future df
future_uk <- make_future_dataframe(uk_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_uk$new_deaths = head(uk_ts$new_deaths, nrow(future_uk))
future_uk$icu_patients = head(uk_ts$icu_patients, nrow(future_uk))
future_uk$positive_rate = head(uk_ts$positive_rate, nrow(future_uk))
future_uk$new_vaccinations = head(uk_ts$new_vaccinations, nrow(future_uk))
future_uk$stringency_index = head(uk_ts$stringency_index, nrow(future_uk))
future_uk$excess_mortality = head(uk_ts$excess_mortality, nrow(future_uk))

# Forecasting
uk_forecast <- predict(uk_fit, future_uk)

# Prophet Plots
dyplot.prophet(uk_fit, uk_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(uk_fit, uk_forecast, weekly_start = 0)

# Predict Future Values
uk_future_preds <- uk_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

uk_preds <- bind_cols(test, uk_future_preds) %>% 
  rename(preds = yhat)

# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

uk_metrics <- uk_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = uk_preds$new_cases, y_true = uk_preds$preds) # 1.434112

uk_metrics
# 1 rmse    standard     71463.  
# 2 mase    standard        30.6
# 3 mae     standard     57645.  
# 4 mape    standard         1.434112

