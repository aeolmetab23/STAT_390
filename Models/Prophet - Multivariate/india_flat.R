
# Prophet: India -------------------------------------------------------

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
India <- covid %>% 
  filter(location == "India") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(India) # hand washing facilities completely missing, 

India <- India %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
India_clean <- India %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(India_clean, is.numeric)
India_clean_scaled <- scale(India_clean[, numeric_columns])

# After scaling, ensure `India_data_scaled` is still a data frame or matrix
India_clean_scaled <- as.data.frame(India_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(India_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 3)

# Replace the original column with the imputed column, unscaling if necessary
India_cleaner <- India_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

India_ts <- as_tsibble(India_cleaner, index = date)

# view seasonality trends
ggplot(India_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: India"
  )

ggplot(India_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: India"
  )

# Split Data
splits <- initial_time_split(India_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
India_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
India_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                           weekly.seasonality = TRUE, daily.seasonality = FALSE,
                           seasonality.mode = "additive", fit = TRUE)

# Add Regressors
India_model = add_regressor(India_model,"new_deaths", standardize = FALSE)
India_model = add_regressor(India_model,"icu_patients", standardize = FALSE)
India_model = add_regressor(India_model,'positive_rate', standardize = FALSE)
India_model = add_regressor(India_model,'new_vaccinations', standardize = FALSE)
India_model = add_regressor(India_model,'stringency_index', standardize = FALSE)

# India_model <- add_country_holidays(India_model, country_name = "India")
## "Holidays in India are not currently supported"

# Fit Model
India_fit = fit.prophet(India_model, df = India_train)

# Future df
future_India <- make_future_dataframe(India_fit, periods = 42, freq = "week")

# Future Regressors
future_India$new_deaths = head(India_ts$new_deaths, nrow(future_India))
future_India$icu_patients = head(India_ts$icu_patients, nrow(future_India))
future_India$positive_rate = head(India_ts$positive_rate, nrow(future_India))
future_India$new_vaccinations = head(India_ts$new_vaccinations, nrow(future_India))
future_India$stringency_index = head(India_ts$stringency_index, nrow(future_India))

# Forecast
India_forecast <- predict(India_fit, future_India)

# Prophet Plots
dyplot.prophet(India_fit, India_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(India_fit, India_forecast, weekly_start = 0)

# Predict Future Values
India_future_preds <- India_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

India_Prophet_multi_flat_Preds <- bind_cols(test, India_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

India_metrics <- India_Prophet_multi_flat_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

India_metrics

India_Prophet_multi_flat <- pivot_wider(India_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "India"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
India_Prophet_multi_flat

save(India_Prophet_multi_flat, India_Prophet_multi_flat_Preds,
     file = "Models/Prophet - Multivariate/results/India_flat_metrics.rda")

