
# Prophet: covid -------------------------------------------------------

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
covid <- covid %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(covid) # hand washing facilities completely missing, 

covid <- covid %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
covid_clean <- covid %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(covid_clean, is.numeric)
covid_clean_scaled <- scale(covid_clean[, numeric_columns])

# After scaling, ensure `covid_data_scaled` is still a data frame or matrix
covid_clean_scaled <- as.data.frame(covid_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(covid_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
covid_cleaner <- covid_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  ) %>% 
  arrange(location, date) %>% 
  rename("ds" = date, "y" = new_cases)

covid_ts <- covid_cleaner

# view seasonality trends
ggplot(covid_ts, aes(x = factor(month), y = y)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: covid"
  )

ggplot(covid_ts) +
  geom_line(aes(x = ds, y = y)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: covid"
  )

# Split Data
splits <- initial_time_split(covid_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
# covid_train <- train %>%
#   rename("ds" = date, "y" = new_cases)

# model
covid_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                           weekly.seasonality = TRUE, daily.seasonality = FALSE,
                           seasonality.mode = "additive", fit = TRUE)

# Add Regressors
covid_model = add_regressor(covid_model,"new_deaths", standardize = FALSE)
covid_model = add_regressor(covid_model,"icu_patients", standardize = FALSE)
covid_model = add_regressor(covid_model,'positive_rate', standardize = FALSE)
covid_model = add_regressor(covid_model,'new_vaccinations', standardize = FALSE)
covid_model = add_regressor(covid_model,'stringency_index', standardize = FALSE)
covid_model = add_regressor(covid_model,'excess_mortality', standardize = FALSE)

# covid_model <- add_country_holidays(covid_model, country_name = "covid")
## "Holidays in covid are not currently supported"

# Fit Model
covid_fit = fit.prophet(covid_model, df = train)

# Future df
future_covid <- make_future_dataframe(covid_fit, periods = 42, freq = "week")

# Future Regressors
future_covid$new_deaths = head(covid_ts$new_deaths, nrow(future_covid))
future_covid$icu_patients = head(covid_ts$icu_patients, nrow(future_covid))
future_covid$positive_rate = head(covid_ts$positive_rate, nrow(future_covid))
future_covid$new_vaccinations = head(covid_ts$new_vaccinations, nrow(future_covid))
future_covid$stringency_index = head(covid_ts$stringency_index, nrow(future_covid))
future_covid$excess_mortality = head(covid_ts$excess_mortality, nrow(future_covid))

# Forecast
covid_forecast <- predict(covid_fit, future_covid)

# Prophet Plots
dyplot.prophet(covid_fit, covid_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(covid_fit, covid_forecast, weekly_start = 0)

# Predict Future Values
covid_future_preds <- covid_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

covid_preds <- bind_cols(test, covid_future_preds) %>% 
  rename(preds = yhat)

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

covid_metrics <- covid_preds %>% 
  covid_metrics(new_cases, estimate = preds)

covid_metrics

save(covid_metrics, covid_preds, file = "Models/Prophet - Multivariate/results/covid_metrics.rda")

