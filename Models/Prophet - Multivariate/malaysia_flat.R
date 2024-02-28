
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
malaysia <- covid %>% 
  filter(location == "Malaysia") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(malaysia) # hand washing facilities completely missing, 

malaysia <- malaysia %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
malaysia_clean <- malaysia %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(malaysia_clean, is.numeric)
malaysia_clean_scaled <- scale(malaysia_clean[, numeric_columns])

# After scaling, ensure `malaysia_data_scaled` is still a data frame or matrix
malaysia_clean_scaled <- as.data.frame(malaysia_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(malaysia_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
malaysia_cleaner <- malaysia_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

malaysia_ts <- as_tsibble(malaysia_cleaner, index = date)

# view seasonality trends
ggplot(malaysia_ts, aes(x = factor(month), y = new_cases)) +
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

ggplot(malaysia_ts) +
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
splits <- initial_time_split(malaysia_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns to fit Prophet ----
malaysia_train <- train %>% rename("ds" = date, "y" = new_cases)

# model
malaysia_model <- prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
)

# adding external regressors
malaysia_model = add_regressor(malaysia_model,"new_deaths", standardize = FALSE)
malaysia_model = add_regressor(malaysia_model,"icu_patients", standardize = FALSE)
malaysia_model = add_regressor(malaysia_model,'positive_rate', standardize = FALSE)
malaysia_model = add_regressor(malaysia_model,'new_vaccinations', standardize = FALSE)
malaysia_model = add_regressor(malaysia_model,'stringency_index', standardize = FALSE)
malaysia_model = add_regressor(malaysia_model,'excess_mortality', standardize = FALSE)

# malaysia_model <- add_country_holidays(malaysia_model, country_name = "Malaysia")
## "Holidays in Malaysia are not currently supported"

# model fitting with training data
malaysia_fit = fit.prophet(malaysia_model, df = malaysia_train)

# making future df
future_malaysia <- make_future_dataframe(malaysia_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_malaysia$new_deaths = head(malaysia_ts$new_deaths, nrow(future_malaysia))
future_malaysia$icu_patients = head(malaysia_ts$icu_patients, nrow(future_malaysia))
future_malaysia$positive_rate = head(malaysia_ts$positive_rate, nrow(future_malaysia))
future_malaysia$new_vaccinations = head(malaysia_ts$new_vaccinations, nrow(future_malaysia))
future_malaysia$stringency_index = head(malaysia_ts$stringency_index, nrow(future_malaysia))
future_malaysia$excess_mortality = head(malaysia_ts$excess_mortality, nrow(future_malaysia))

# Forecasting
malaysia_forecast <- predict(malaysia_fit, future_malaysia)

# Prophet Plots
dyplot.prophet(malaysia_fit, malaysia_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(malaysia_fit, malaysia_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
malaysia_future_preds <- malaysia_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

malaysia_preds <- bind_cols(test, malaysia_future_preds) %>% 
  rename(preds = yhat)

# Metrics
# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

malaysia_metrics <- malaysia_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = malaysia_preds$new_cases, y_true = malaysia_preds$preds) # 2.203182

malaysia_metrics
# 1 rmse    standard     11672.  
# 2 mase    standard         4.50
# 3 mae     standard     10565.  
# 4 mape    standard         2.203182

