
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
Mexico <- covid %>% 
  filter(location == "Mexico") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(Mexico) # hand washing facilities completely missing, 

Mexico <- Mexico %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Mexico_clean <- Mexico %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(Mexico_clean, is.numeric)
Mexico_clean_scaled <- scale(Mexico_clean[, numeric_columns])

# After scaling, ensure `Mexico_data_scaled` is still a data frame or matrix
Mexico_clean_scaled <- as.data.frame(Mexico_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Mexico_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 4)

# Replace the original column with the imputed column, unscaling if necessary
Mexico_cleaner <- Mexico_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Mexico_ts <- as_tsibble(Mexico_cleaner, index = date)

# view seasonality trends
ggplot(Mexico_ts, aes(x = factor(month), y = new_cases)) +
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

ggplot(Mexico_ts) +
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
splits <- initial_time_split(Mexico_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Mexico_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Mexico_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                       weekly.seasonality = TRUE, daily.seasonality = FALSE,
                       seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Mexico_model = add_regressor(Mexico_model,"new_deaths", standardize = FALSE)
Mexico_model = add_regressor(Mexico_model,"icu_patients", standardize = FALSE)
Mexico_model = add_regressor(Mexico_model,'positive_rate', standardize = FALSE)
Mexico_model = add_regressor(Mexico_model,'new_vaccinations', standardize = FALSE)
Mexico_model = add_regressor(Mexico_model,'stringency_index', standardize = FALSE)
Mexico_model = add_regressor(Mexico_model,'excess_mortality', standardize = FALSE)

# Mexico_model <- add_country_holidays(Mexico_model, country_name = "Mexico")
## "Holidays in Mexico are not currently supported"

# Fit Model
Mexico_fit = fit.prophet(Mexico_model, df = Mexico_train)

# Future df
future_Mexico <- make_future_dataframe(Mexico_fit, periods = 42, freq = "week")

# Future Regressors
future_Mexico$new_deaths = head(Mexico_ts$new_deaths, nrow(future_Mexico))
future_Mexico$icu_patients = head(Mexico_ts$icu_patients, nrow(future_Mexico))
future_Mexico$positive_rate = head(Mexico_ts$positive_rate, nrow(future_Mexico))
future_Mexico$new_vaccinations = head(Mexico_ts$new_vaccinations, nrow(future_Mexico))
future_Mexico$stringency_index = head(Mexico_ts$stringency_index, nrow(future_Mexico))
future_Mexico$excess_mortality = head(Mexico_ts$excess_mortality, nrow(future_Mexico))

# Forecast
Mexico_forecast <- predict(Mexico_fit, future_Mexico)

# Prophet Plots
dyplot.prophet(Mexico_fit, Mexico_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Mexico_fit, Mexico_forecast, weekly_start = 0)

# Predict Future Values
Mexico_future_preds <- Mexico_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Mexico_Prophet_multi_flat_Preds <- bind_cols(test, Mexico_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Mexico_metrics <- Mexico_Prophet_multi_flat_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Mexico_metrics

Mexico_Prophet_multi_flat <- pivot_wider(Mexico_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Mexico"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Mexico_Prophet_multi_flat

save(Mexico_Prophet_multi_flat, Mexico_Prophet_multi_flat_Preds,
     file = "Models/Prophet - Multivariate/results/Mexico_flat_metrics.rda")

