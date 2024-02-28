
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
library(diceR)

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
italy <- covid %>% 
  filter(location == "Italy") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(italy) # hand washing facilities completely missing, 

italy <- italy %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
italy_clean <- italy %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(italy_clean, is.numeric)
italy_clean_scaled <- scale(italy_clean[, numeric_columns])

# After scaling, ensure `italy_data_scaled` is still a data frame or matrix
italy_clean_scaled <- as.data.frame(italy_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(italy_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
italy_cleaner <- italy_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

italy_ts <- as_tsibble(italy_cleaner, index = date)

# view seasonality trends
ggplot(italy_ts) +
  geom_col(aes(x = week, y = new_cases, fill = week)) +
  theme_minimal()

ggplot(italy_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal()

# Split Data
splits <- initial_time_split(italy_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
italy_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
italy_model <- (prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
))

# adding external regressors
italy_model = add_regressor(italy_model,"new_deaths", standardize = FALSE)
italy_model = add_regressor(italy_model,"hosp_patients", standardize = FALSE)
italy_model = add_regressor(italy_model,"icu_patients", standardize = FALSE)
italy_model = add_regressor(italy_model,'new_vaccinations', standardize = FALSE)
italy_model = add_regressor(italy_model,'stringency_index', standardize = FALSE)
italy_model = add_regressor(italy_model,'excess_mortality', standardize = FALSE)

# italy_model <- add_country_holidays(italy_model, country_name = "Italy")
## "Holidays in Italy are not currently supported"

# model fitting with training data
italy_fit = fit.prophet(italy_model, df = italy_train)

# making future df
future_italy <- make_future_dataframe(italy_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_italy$new_deaths = head(italy_ts$new_deaths, nrow(future_italy))
future_italy$hosp_patients = head(italy_ts$hosp_patients, nrow(future_italy))
future_italy$icu_patients = head(italy_ts$icu_patients, nrow(future_italy))
future_italy$new_vaccinations = head(italy_ts$new_vaccinations, nrow(future_italy))
future_italy$stringency_index = head(italy_ts$stringency_index, nrow(future_italy))
future_italy$excess_mortality = head(italy_ts$excess_mortality, nrow(future_italy))

####### forecasting
italy_forecast <- predict(italy_fit, future_italy) 

##### plots
dyplot.prophet(italy_fit, italy_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(italy_fit, italy_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
italy_future_preds <- italy_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

italy_preds <- bind_cols(test, italy_future_preds) %>% 
  rename(preds = yhat)

# Metrics
MAPE(y_pred = italy_preds$new_cases, y_true = italy_preds$preds) # 1.173277
mase_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds) # 14.14057
