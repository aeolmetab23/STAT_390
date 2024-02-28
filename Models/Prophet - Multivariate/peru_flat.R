
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
peru <- covid %>% 
  filter(location == "Peru") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(peru) # hand washing facilities completely missing, 

peru <- peru %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
peru_clean <- peru %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(peru_clean, is.numeric)
peru_clean_scaled <- scale(peru_clean[, numeric_columns])

# After scaling, ensure `peru_data_scaled` is still a data frame or matrix
peru_clean_scaled <- as.data.frame(peru_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(peru_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
peru_cleaner <- peru_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

peru_ts <- as_tsibble(peru_cleaner, index = date)

# view seasonality trends
ggplot(peru_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Peru"
  )

ggplot(peru_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Peru"
  )

# Split Data
splits <- initial_time_split(peru_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns to fit Prophet ----
peru_train <- train %>% rename("ds" = date, "y" = new_cases)

# model
peru_model <- prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
)

# adding external regressors
peru_model = add_regressor(peru_model,"new_deaths", standardize = FALSE)
peru_model = add_regressor(peru_model,"icu_patients", standardize = FALSE)
peru_model = add_regressor(peru_model,'positive_rate', standardize = FALSE)
peru_model = add_regressor(peru_model,'new_vaccinations', standardize = FALSE)
peru_model = add_regressor(peru_model,'stringency_index', standardize = FALSE)
peru_model = add_regressor(peru_model,'excess_mortality', standardize = FALSE)

# peru_model <- add_country_holidays(peru_model, country_name = "Peru")
## "Holidays in Peru are not currently supported"

# model fitting with training data
peru_fit = fit.prophet(peru_model, df = peru_train)

# making future df
future_peru <- make_future_dataframe(peru_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_peru$new_deaths = head(peru_ts$new_deaths, nrow(future_peru))
future_peru$icu_patients = head(peru_ts$icu_patients, nrow(future_peru))
future_peru$positive_rate = head(peru_ts$positive_rate, nrow(future_peru))
future_peru$new_vaccinations = head(peru_ts$new_vaccinations, nrow(future_peru))
future_peru$stringency_index = head(peru_ts$stringency_index, nrow(future_peru))
future_peru$excess_mortality = head(peru_ts$excess_mortality, nrow(future_peru))

# Forecasting
peru_forecast <- predict(peru_fit, future_peru)

# Prophet Plots
dyplot.prophet(peru_fit, peru_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(peru_fit, peru_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
peru_future_preds <- peru_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

peru_preds <- bind_cols(test, peru_future_preds) %>% 
  rename(preds = yhat)

# Metrics
# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

peru_metrics <- peru_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = peru_preds$new_cases, y_true = peru_preds$preds) # 1.405161

peru_metrics
# 1 rmse    standard     14469.  
# 2 mase    standard        19.9
# 3 mae     standard     10446.  
# 4 mape    standard         1.033532

