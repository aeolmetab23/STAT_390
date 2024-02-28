
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
Italy <- covid %>% 
  filter(location == "Italy") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(Italy) # hand washing facilities completely missing, 

Italy <- Italy %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Italy_clean <- Italy %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(Italy_clean, is.numeric)
Italy_clean_scaled <- scale(Italy_clean[, numeric_columns])

# After scaling, ensure `Italy_data_scaled` is still a data frame or matrix
Italy_clean_scaled <- as.data.frame(Italy_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Italy_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
Italy_cleaner <- Italy_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Italy_ts <- as_tsibble(Italy_cleaner, index = date)

# view seasonality trends
ggplot(Italy_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Italy"
  )

ggplot(Italy_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Italy"
  )

# Split Data
splits <- initial_time_split(Italy_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Italy_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Italy_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                           weekly.seasonality = TRUE, daily.seasonality = FALSE,
                           seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Italy_model = add_regressor(Italy_model,"new_deaths", standardize = FALSE)
Italy_model = add_regressor(Italy_model,"icu_patients", standardize = FALSE)
Italy_model = add_regressor(Italy_model,'positive_rate', standardize = FALSE)
Italy_model = add_regressor(Italy_model,'new_vaccinations', standardize = FALSE)
Italy_model = add_regressor(Italy_model,'stringency_index', standardize = FALSE)
Italy_model = add_regressor(Italy_model,'excess_mortality', standardize = FALSE)

# Italy_model <- add_country_holidays(Italy_model, country_name = "Italy")
## "Holidays in Italy are not currently supported"

# Fit Model
Italy_fit = fit.prophet(Italy_model, df = Italy_train)

# Future df
future_Italy <- make_future_dataframe(Italy_fit, periods = 42, freq = "week")

# Future Regressors
future_Italy$new_deaths = head(Italy_ts$new_deaths, nrow(future_Italy))
future_Italy$icu_patients = head(Italy_ts$icu_patients, nrow(future_Italy))
future_Italy$positive_rate = head(Italy_ts$positive_rate, nrow(future_Italy))
future_Italy$new_vaccinations = head(Italy_ts$new_vaccinations, nrow(future_Italy))
future_Italy$stringency_index = head(Italy_ts$stringency_index, nrow(future_Italy))
future_Italy$excess_mortality = head(Italy_ts$excess_mortality, nrow(future_Italy))

# Forecast
Italy_forecast <- predict(Italy_fit, future_Italy)

# Prophet Plots
dyplot.prophet(Italy_fit, Italy_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Italy_fit, Italy_forecast, weekly_start = 0)

# Predict Future Values
Italy_future_preds <- Italy_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Italy_preds <- bind_cols(test, Italy_future_preds) %>% 
  rename(preds = yhat)

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Italy_metrics <- Italy_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Italy_metrics

save(Italy_metrics, Italy_preds, file = "Models/Prophet - Multivariate/results/Italy_metrics.rda")

