
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
mexico <- covid %>% 
  filter(location == "Mexico") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(mexico) # hand washing facilities completely missing, 

mexico <- mexico %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
mexico_clean <- mexico %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(mexico_clean, is.numeric)
mexico_clean_scaled <- scale(mexico_clean[, numeric_columns])

# After scaling, ensure `mexico_data_scaled` is still a data frame or matrix
mexico_clean_scaled <- as.data.frame(mexico_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(mexico_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
mexico_cleaner <- mexico_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

mexico_ts <- as_tsibble(mexico_cleaner, index = date)

# view seasonality trends
ggplot(mexico_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Mexico"
  )

ggplot(mexico_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Mexico"
  )

# Split Data
splits <- initial_time_split(mexico_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns to fit Prophet ----
mexico_train <- train %>% rename("ds" = date, "y" = new_cases)

# model
mexico_model <- prophet(
  df = NULL,
  growth = "linear",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
)

# adding external regressors
mexico_model = add_regressor(mexico_model,"new_deaths", standardize = FALSE)
mexico_model = add_regressor(mexico_model,"icu_patients", standardize = FALSE)
mexico_model = add_regressor(mexico_model,'positive_rate', standardize = FALSE)
mexico_model = add_regressor(mexico_model,'new_vaccinations', standardize = FALSE)
mexico_model = add_regressor(mexico_model,'stringency_index', standardize = FALSE)
mexico_model = add_regressor(mexico_model,'excess_mortality', standardize = FALSE)

# mexico_model <- add_country_holidays(mexico_model, country_name = "Mexico")
## "Holidays in Mexico are not currently supported"

# model fitting with training data
mexico_fit = fit.prophet(mexico_model, df = mexico_train)

# making future df
future_mexico <- make_future_dataframe(mexico_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_mexico$new_deaths = head(mexico_ts$new_deaths, nrow(future_mexico))
future_mexico$icu_patients = head(mexico_ts$icu_patients, nrow(future_mexico))
future_mexico$positive_rate = head(mexico_ts$positive_rate, nrow(future_mexico))
future_mexico$new_vaccinations = head(mexico_ts$new_vaccinations, nrow(future_mexico))
future_mexico$stringency_index = head(mexico_ts$stringency_index, nrow(future_mexico))
future_mexico$excess_mortality = head(mexico_ts$excess_mortality, nrow(future_mexico))

# Forecasting
mexico_forecast <- predict(mexico_fit, future_mexico)

# Prophet Plots
dyplot.prophet(mexico_fit, mexico_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(mexico_fit, mexico_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
mexico_future_preds <- mexico_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

mexico_preds <- bind_cols(test, mexico_future_preds) %>% 
  rename(preds = yhat)

# Metrics
# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

mexico_metrics <- mexico_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = mexico_preds$new_cases, y_true = mexico_preds$preds) # 1.59022

mexico_metrics
# 1 rmse    standard     33639.  
# 2 mase    standard        25.5
# 3 mae     standard     27121.  
# 4 mape    standard         1.59022

