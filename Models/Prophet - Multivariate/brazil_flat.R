
# Prophet: Brazil -------------------------------------------------------

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
brazil <- covid %>% 
  filter(location == "Brazil") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(brazil) # hand washing facilities completely missing, 

brazil <- brazil %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
brazil_clean <- brazil %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(brazil_clean, is.numeric)
brazil_clean_scaled <- scale(brazil_clean[, numeric_columns])

# After scaling, ensure `brazil_data_scaled` is still a data frame or matrix
brazil_clean_scaled <- as.data.frame(brazil_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(brazil_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
brazil_cleaner <- brazil_clean %>% 
  mutate(
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

brazil_ts <- as_tsibble(brazil_cleaner, index = date)

# EDA
ggplot(brazil_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Brazil"
  )

ggplot(brazil_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Brazil"
  )

# Split Data
splits <- initial_time_split(brazil_ts, prop = 0.8)
train <- training(splits)
test <- testing(splits)

# Rename Columns ----
brazil_train <- train %>% rename("ds" = date, "y" = new_cases)

# Create Model ----
brazil_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                        weekly.seasonality = TRUE, daily.seasonality = FALSE,
                        seasonality.mode = "additive", fit = TRUE)

# adding external regressors
brazil_model = add_regressor(brazil_model,"new_deaths", standardize = FALSE)
brazil_model = add_regressor(brazil_model,"icu_patients", standardize = FALSE)
brazil_model = add_regressor(brazil_model,'new_vaccinations', standardize = FALSE)
# brazil_model = add_regressor(brazil_model,'positive_rate', standardize = FALSE)
brazil_model = add_regressor(brazil_model,'stringency_index', standardize = FALSE)
# brazil_model = add_regressor(brazil_model,'excess_mortality', standardize = FALSE)


# brazil_model <- add_country_holidays(brazil_model, country_name = "Brazil")
## "Holidays in Brazil are not currently supported"

# model fitting with training data
brazil_fit = fit.prophet(brazil_model, df = brazil_train)

# making future df
future_brazil <- make_future_dataframe(brazil_fit, periods = 42, freq = "week")

# Extra Regressors
future_brazil$new_deaths = head(brazil_ts$new_deaths, nrow(future_brazil))
future_brazil$icu_patients = head(brazil_ts$icu_patients, nrow(future_brazil))
future_brazil$new_vaccinations = head(brazil_ts$new_vaccinations, nrow(future_brazil))
# future_brazil$positive_rate = head(brazil_ts$positive_rate, nrow(future_brazil))
future_brazil$stringency_index = head(brazil_ts$stringency_index, nrow(future_brazil))
# future_brazil$excess_mortality = head(brazil_ts$excess_mortality, nrow(future_brazil))

# Forecasting
brazil_forecast <- predict(brazil_fit, future_brazil)

# Prophet Plots
dyplot.prophet(brazil_fit, brazil_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(brazil_fit, brazil_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
brazil_future_preds <- brazil_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

brazil_preds <- bind_cols(test, brazil_future_preds) %>% 
  rename(preds = yhat)

# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

brazil_metrics <- brazil_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = brazil_preds$new_cases, y_true = brazil_preds$preds) # 0.8957904

brazil_metrics
# 1 rmse    standard     89729. 
# 2 mase    standard        35.2
# 3 mae     standard     82624.
# 4 mape    standard         0.8957904


