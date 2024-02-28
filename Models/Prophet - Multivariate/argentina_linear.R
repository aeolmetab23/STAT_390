
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
argentina <- covid %>% 
  filter(location == "Argentina") %>% 
  mutate(
    date = ymd(date)
    ) %>% 
  arrange(date)

skimr::skim_without_charts(argentina) # hand washing facilities completely missing, 

argentina <- argentina %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
argentina_clean <- argentina %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
    )

# Begin Imputation
numeric_columns <- sapply(argentina_clean, is.numeric)
argentina_clean_scaled <- scale(argentina_clean[, numeric_columns])

# After scaling, ensure `argentina_data_scaled` is still a data frame or matrix
argentina_clean_scaled <- as.data.frame(argentina_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(argentina_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
argentina_cleaner <- argentina_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

argentina_ts <- as_tsibble(argentina_cleaner, index = date)

# view seasonality trends
ggplot(argentina_ts, aes(x = factor(month), y = new_cases)) +
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

ggplot(argentina_ts) +
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
splits <- initial_time_split(argentina_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
argentina_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
argentina_model <- prophet(
  df = NULL,
  growth = "linear",
  yearly.seasonality = FALSE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "additive", # by observation
  fit = TRUE
)

# adding external regressors
argentina_model = add_regressor(argentina_model,"new_deaths", standardize = FALSE)
argentina_model = add_regressor(argentina_model,"icu_patients", standardize = FALSE)
argentina_model = add_regressor(argentina_model,'positive_rate', standardize = FALSE)
argentina_model = add_regressor(argentina_model,'new_vaccinations', standardize = FALSE)
argentina_model = add_regressor(argentina_model,'stringency_index', standardize = FALSE)
argentina_model = add_regressor(argentina_model,'excess_mortality', standardize = FALSE)

# argentina_model <- add_country_holidays(argentina_model, country_name = "Argentina")
## "Holidays in Argentina are not currently supported"

# Fit to Training
argentina_fit = fit.prophet(argentina_model, df = argentina_train)

# Make Future
future_argentina <- make_future_dataframe(argentina_fit, periods = 42, freq = "week")

# Columns for Extra Regressors
future_argentina$new_deaths = head(argentina_ts$new_deaths, nrow(future_argentina))
future_argentina$icu_patients = head(argentina_ts$icu_patients, nrow(future_argentina))
future_argentina$positive_rate = head(argentina_ts$positive_rate, nrow(future_argentina))
future_argentina$new_vaccinations = head(argentina_ts$new_vaccinations, nrow(future_argentina))
future_argentina$stringency_index = head(argentina_ts$stringency_index, nrow(future_argentina))
future_argentina$excess_mortality = head(argentina_ts$excess_mortality, nrow(future_argentina))

# Forecasting
argentina_forecast <- predict(argentina_fit, future_argentina)

# Prophet Plots
dyplot.prophet(argentina_fit, argentina_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(argentina_fit, argentina_forecast, weekly_start = 0)

# Predict Future Values
argentina_future_preds <- argentina_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

argentina_preds <- bind_cols(test, argentina_future_preds) %>% 
  rename(preds = yhat)

# Metrics
library(MLmetrics)

covid_metrics <- metric_set(rmse, mase, mae)

argentina_metrics <- argentina_preds %>% 
  covid_metrics(new_cases, estimate = preds)

MLmetrics::MAPE(y_pred = argentina_preds$new_cases, y_true = argentina_preds$preds) # 18.72072

argentina_metrics
# 1 rmse    standard     10448.  
# 2 mase    standard         4.54
# 3 mae     standard      8551.  
# 4 mape    standard        18.72072

