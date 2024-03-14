
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
Peru <- covid %>% 
  filter(location == "Peru") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(Peru) # hand washing facilities completely missing, 

Peru <- Peru %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Peru_clean <- Peru %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(Peru_clean, is.numeric)
Peru_clean_scaled <- scale(Peru_clean[, numeric_columns])

# After scaling, ensure `Peru_data_scaled` is still a data frame or matrix
Peru_clean_scaled <- as.data.frame(Peru_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Peru_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
Peru_cleaner <- Peru_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Peru_ts <- as_tsibble(Peru_cleaner, index = date)

# view seasonality trends
ggplot(Peru_ts, aes(x = factor(month), y = new_cases)) +
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

ggplot(Peru_ts) +
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
splits <- initial_time_split(Peru_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Peru_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Peru_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                       weekly.seasonality = TRUE, daily.seasonality = FALSE,
                       seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Peru_model = add_regressor(Peru_model,"new_deaths", standardize = FALSE)
Peru_model = add_regressor(Peru_model,"icu_patients", standardize = FALSE)
Peru_model = add_regressor(Peru_model,'positive_rate', standardize = FALSE)
Peru_model = add_regressor(Peru_model,'new_vaccinations', standardize = FALSE)
Peru_model = add_regressor(Peru_model,'stringency_index', standardize = FALSE)
Peru_model = add_regressor(Peru_model,'excess_mortality', standardize = FALSE)

# Peru_model <- add_country_holidays(Peru_model, country_name = "Peru")
## "Holidays in Peru are not currently supported"

# Fit Model
Peru_fit = fit.prophet(Peru_model, df = Peru_train)

# Future df
future_Peru <- make_future_dataframe(Peru_fit, periods = 42, freq = "week")

# Future Regressors
future_Peru$new_deaths = head(Peru_ts$new_deaths, nrow(future_Peru))
future_Peru$icu_patients = head(Peru_ts$icu_patients, nrow(future_Peru))
future_Peru$positive_rate = head(Peru_ts$positive_rate, nrow(future_Peru))
future_Peru$new_vaccinations = head(Peru_ts$new_vaccinations, nrow(future_Peru))
future_Peru$stringency_index = head(Peru_ts$stringency_index, nrow(future_Peru))
future_Peru$excess_mortality = head(Peru_ts$excess_mortality, nrow(future_Peru))

# Forecast
Peru_forecast <- predict(Peru_fit, future_Peru)

# Prophet Plots
dyplot.prophet(Peru_fit, Peru_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Peru_fit, Peru_forecast, weekly_start = 0)

# Predict Future Values
Peru_future_preds <- Peru_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Peru_Prophet_multi_flat_Preds <- bind_cols(test, Peru_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Peru_metrics <- Peru_Prophet_multi_flat_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Peru_metrics

Peru_Prophet_multi_flat <- pivot_wider(Peru_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Peru"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Peru_Prophet_multi_flat

save(Peru_Prophet_multi_flat, Peru_Prophet_multi_flat_Preds,
     file = "Models/Prophet - Multivariate/results/Peru_flat_metrics.rda")

# # view seasonality trends
ggplot(Peru_Prophet_multi_flat_Preds) +
  geom_line(aes(date, y = preds), show.legend = F, color = "skyblue") +
  geom_line(aes(date, y = new_cases), show.legend = F, color = "indianred") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Argentina"
  )

