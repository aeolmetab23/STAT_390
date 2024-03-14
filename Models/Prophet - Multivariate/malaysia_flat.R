
# Prophet: Malaysia -------------------------------------------------------

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
Malaysia <- covid %>% 
  filter(location == "Malaysia") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(Malaysia) # hand washing facilities completely missing, 

Malaysia <- Malaysia %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Malaysia_clean <- Malaysia %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(Malaysia_clean, is.numeric)
Malaysia_clean_scaled <- scale(Malaysia_clean[, numeric_columns])

# After scaling, ensure `Malaysia_data_scaled` is still a data frame or matrix
Malaysia_clean_scaled <- as.data.frame(Malaysia_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Malaysia_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 2)

# Replace the original column with the imputed column, unscaling if necessary
Malaysia_cleaner <- Malaysia_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Malaysia_ts <- as_tsibble(Malaysia_cleaner, index = date)

# view seasonality trends
ggplot(Malaysia_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Malaysia"
  )

ggplot(Malaysia_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Malaysia"
  )

# Split Data
splits <- initial_time_split(Malaysia_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Malaysia_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Malaysia_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                       weekly.seasonality = TRUE, daily.seasonality = FALSE,
                       seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Malaysia_model = add_regressor(Malaysia_model,"new_deaths", standardize = FALSE)
Malaysia_model = add_regressor(Malaysia_model,"icu_patients", standardize = FALSE)
Malaysia_model = add_regressor(Malaysia_model,'positive_rate', standardize = FALSE)
Malaysia_model = add_regressor(Malaysia_model,'new_vaccinations', standardize = FALSE)
Malaysia_model = add_regressor(Malaysia_model,'stringency_index', standardize = FALSE)
Malaysia_model = add_regressor(Malaysia_model,'excess_mortality', standardize = FALSE)

# Malaysia_model <- add_country_holidays(Malaysia_model, country_name = "Malaysia")
## "Holidays in Malaysia are not currently supported"

# Fit Model
Malaysia_fit = fit.prophet(Malaysia_model, df = Malaysia_train)

# Future df
future_Malaysia <- make_future_dataframe(Malaysia_fit, periods = 42, freq = "week")

# Future Regressors
future_Malaysia$new_deaths = head(Malaysia_ts$new_deaths, nrow(future_Malaysia))
future_Malaysia$icu_patients = head(Malaysia_ts$icu_patients, nrow(future_Malaysia))
future_Malaysia$positive_rate = head(Malaysia_ts$positive_rate, nrow(future_Malaysia))
future_Malaysia$new_vaccinations = head(Malaysia_ts$new_vaccinations, nrow(future_Malaysia))
future_Malaysia$stringency_index = head(Malaysia_ts$stringency_index, nrow(future_Malaysia))
future_Malaysia$excess_mortality = head(Malaysia_ts$excess_mortality, nrow(future_Malaysia))

# Forecast
Malaysia_forecast <- predict(Malaysia_fit, future_Malaysia)

# Prophet Plots
dyplot.prophet(Malaysia_fit, Malaysia_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Malaysia_fit, Malaysia_forecast, weekly_start = 0)

# Predict Future Values
Malaysia_future_preds <- Malaysia_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Malaysia_Prophet_multi_flat_Preds <- bind_cols(test, Malaysia_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Malaysia_metrics <- Malaysia_Prophet_multi_flat_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Malaysia_metrics

Malaysia_Prophet_multi_flat <- pivot_wider(Malaysia_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Malaysia"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Malaysia_Prophet_multi_flat

save(Malaysia_Prophet_multi_flat, Malaysia_Prophet_multi_flat_Preds,
     file = "Models/Prophet - Multivariate/results/Malaysia_flat_metrics.rda")

