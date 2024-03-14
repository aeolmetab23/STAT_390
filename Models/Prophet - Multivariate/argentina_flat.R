
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
Argentina <- covid %>% 
  filter(location == "Argentina") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(Argentina) # hand washing facilities completely missing, 

Argentina <- Argentina %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Argentina_clean <- Argentina %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(Argentina_clean, is.numeric)
Argentina_clean_scaled <- scale(Argentina_clean[, numeric_columns])

# After scaling, ensure `Argentina_data_scaled` is still a data frame or matrix
Argentina_clean_scaled <- as.data.frame(Argentina_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Argentina_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 5)

# Replace the original column with the imputed column, unscaling if necessary
Argentina_cleaner <- Argentina_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Argentina_ts <- as_tsibble(Argentina_cleaner, index = date)

# view seasonality trends
ggplot(Argentina_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Argentina"
  )

ggplot(Argentina_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Argentina"
  )

# Split Data
splits <- initial_time_split(Argentina_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Argentina_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Argentina_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = TRUE,
                           weekly.seasonality = TRUE, daily.seasonality = FALSE,
                           seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Argentina_model = add_regressor(Argentina_model,"new_deaths", standardize = FALSE)
Argentina_model = add_regressor(Argentina_model,"icu_patients", standardize = FALSE)
Argentina_model = add_regressor(Argentina_model,'positive_rate', standardize = FALSE)
Argentina_model = add_regressor(Argentina_model,'new_vaccinations', standardize = FALSE)
Argentina_model = add_regressor(Argentina_model,'stringency_index', standardize = FALSE)
Argentina_model = add_regressor(Argentina_model,'excess_mortality', standardize = FALSE)

# Argentina_model <- add_country_holidays(Argentina_model, country_name = "Argentina")
## "Holidays in Argentina are not currently supported"

# Fit Model
Argentina_fit = fit.prophet(Argentina_model, df = Argentina_train)

# Future df
future_Argentina <- make_future_dataframe(Argentina_fit, periods = 42, freq = "week")

# Future Regressors
future_Argentina$new_deaths = head(Argentina_ts$new_deaths, nrow(future_Argentina))
future_Argentina$icu_patients = head(Argentina_ts$icu_patients, nrow(future_Argentina))
future_Argentina$positive_rate = head(Argentina_ts$positive_rate, nrow(future_Argentina))
future_Argentina$new_vaccinations = head(Argentina_ts$new_vaccinations, nrow(future_Argentina))
future_Argentina$stringency_index = head(Argentina_ts$stringency_index, nrow(future_Argentina))
future_Argentina$excess_mortality = head(Argentina_ts$excess_mortality, nrow(future_Argentina))

# Forecast
Argentina_forecast <- predict(Argentina_fit, future_Argentina)

# Prophet Plots
dyplot.prophet(Argentina_fit, Argentina_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Argentina_fit, Argentina_forecast, weekly_start = 0)

# Predict Future Values
Argentina_future_preds <- Argentina_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Argentina_Prophet_multi_flat_Preds <- bind_cols(test, Argentina_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(
    preds = ifelse(preds < 0, 0, preds)
  )

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Argentina_metrics <- Argentina_Prophet_multi_flat_Preds %>% 
  covid_metrics(new_cases, estimate = preds)

Argentina_metrics

Argentina_Prophet_multi_flat <- pivot_wider(Argentina_metrics, names_from = .metric, values_from = .estimate) %>% 
  mutate(
    location = "Argentina"
  ) %>% 
  select(
    location, rmse, mase, mae, .estimator
  )
Argentina_Prophet_multi_flat

save(Argentina_Prophet_multi_flat, Argentina_Prophet_multi_flat_Preds,
     file = "Models/Prophet - Multivariate/results/Argentina_flat_metrics.rda")

