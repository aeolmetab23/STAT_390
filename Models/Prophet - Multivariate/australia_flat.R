
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
Australia <- covid %>% 
  filter(location == "Australia") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(Australia) # hand washing facilities completely missing, 

Australia <- Australia %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Australia_clean <- Australia %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(Australia_clean, is.numeric)
Australia_clean_scaled <- scale(Australia_clean[, numeric_columns])

# After scaling, ensure `Australia_data_scaled` is still a data frame or matrix
Australia_clean_scaled <- as.data.frame(Australia_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Australia_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
Australia_cleaner <- Australia_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Australia_ts <- as_tsibble(Australia_cleaner, index = date)

# view seasonality trends
ggplot(Australia_ts, aes(x = factor(month), y = new_cases)) +
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

ggplot(Australia_ts) +
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
splits <- initial_time_split(Australia_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Australia_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Australia_model <- prophet(df = NULL, growth = "flat", yearly.seasonality = FALSE,
                           weekly.seasonality = TRUE, daily.seasonality = FALSE,
                           seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Australia_model = add_regressor(Australia_model,"new_deaths", standardize = FALSE)
Australia_model = add_regressor(Australia_model,"icu_patients", standardize = FALSE)
Australia_model = add_regressor(Australia_model,'positive_rate', standardize = FALSE)
Australia_model = add_regressor(Australia_model,'new_vaccinations', standardize = FALSE)
Australia_model = add_regressor(Australia_model,'stringency_index', standardize = FALSE)
Australia_model = add_regressor(Australia_model,'excess_mortality', standardize = FALSE)

# Australia_model <- add_country_holidays(Australia_model, country_name = "Australia")
## "Holidays in Australia are not currently supported"

# Fit Model
Australia_fit = fit.prophet(Australia_model, df = Australia_train)

# Future df
future_Australia <- make_future_dataframe(Australia_fit, periods = 42, freq = "week")

# Future Regressors
future_Australia$new_deaths = head(Australia_ts$new_deaths, nrow(future_Australia))
future_Australia$icu_patients = head(Australia_ts$icu_patients, nrow(future_Australia))
future_Australia$positive_rate = head(Australia_ts$positive_rate, nrow(future_Australia))
future_Australia$new_vaccinations = head(Australia_ts$new_vaccinations, nrow(future_Australia))
future_Australia$stringency_index = head(Australia_ts$stringency_index, nrow(future_Australia))
future_Australia$excess_mortality = head(Australia_ts$excess_mortality, nrow(future_Australia))

# Forecast
Australia_forecast <- predict(Australia_fit, future_Australia)

# Prophet Plots
dyplot.prophet(Australia_fit, Australia_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Australia_fit, Australia_forecast, weekly_start = 0)

# Predict Future Values
Australia_future_preds <- Australia_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Australia_preds <- bind_cols(test, Australia_future_preds) %>% 
  rename(preds = yhat)

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Australia_metrics <- Australia_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Australia_metrics

save(Australia_metrics, Australia_preds, file = "Models/Prophet - Multivariate/results/Australia_metrics.rda")

