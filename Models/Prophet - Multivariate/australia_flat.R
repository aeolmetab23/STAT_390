
# Prophet: Australia -------------------------------------------------------

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
australia <- covid %>% 
  filter(location == "Australia") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(australia) # hand washing facilities completely missing, 

australia <- australia %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
australia_clean <- australia %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(australia_clean, is.numeric)
australia_clean_scaled <- scale(australia_clean[, numeric_columns])

# After scaling, ensure `australia_data_scaled` is still a data frame or matrix
australia_clean_scaled <- as.data.frame(australia_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(australia_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
australia_cleaner <- australia_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

australia_ts <- as_tsibble(australia_cleaner, index = date)

# view seasonality trends
ggplot(australia_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Australia"
  )

ggplot(australia_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Australia"
  )

# Split Data
splits <- initial_time_split(australia_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
australia_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
australia_model <- prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
)

# adding external regressors
australia_model = add_regressor(australia_model,"new_deaths", standardize = FALSE)
australia_model = add_regressor(australia_model,"hosp_patients", standardize = FALSE)
australia_model = add_regressor(australia_model,"icu_patients", standardize = FALSE)
australia_model = add_regressor(australia_model,'new_vaccinations', standardize = FALSE)
australia_model = add_regressor(australia_model,'stringency_index', standardize = FALSE)
australia_model = add_regressor(australia_model,'excess_mortality', standardize = FALSE)

# australia_model <- add_country_holidays(australia_model, country_name = "Australia")
## "Holidays in Australia are not currently supported"

# model fitting with training data
australia_fit = fit.prophet(australia_model, df = australia_train)

# making future df
future_australia <- make_future_dataframe(australia_fit, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_australia$new_deaths = head(australia_ts$new_deaths, nrow(future_australia))
future_australia$hosp_patients = head(australia_ts$hosp_patients, nrow(future_australia))
future_australia$icu_patients = head(australia_ts$icu_patients, nrow(future_australia))
future_australia$new_vaccinations = head(australia_ts$new_vaccinations, nrow(future_australia))
future_australia$stringency_index = head(australia_ts$stringency_index, nrow(future_australia))
future_australia$excess_mortality = head(australia_ts$excess_mortality, nrow(future_australia))

# Forecasting
australia_forecast <- predict(australia_fit, future_australia)

# Prophet Plots
dyplot.prophet(australia_fit, australia_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(australia_fit, australia_forecast,
                        uncertainty = TRUE, plot_cap = TRUE,
                        yearly_start = 0, render_plot = TRUE)

# Predict Future Values
australia_future_preds <- australia_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

australia_preds <- bind_cols(test, australia_future_preds) %>% 
  rename(preds = yhat)

# Metrics
library(MLmetrics)
MAPE(y_pred = australia_preds$new_cases, y_true = australia_preds$preds) # 0.8869082
mase_vec(truth = australia_preds$new_cases, estimate = australia_preds$preds) # 36.85562

