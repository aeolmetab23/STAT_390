
# Prophet: Morocco -------------------------------------------------------

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
Morocco <- covid %>% 
  filter(location == "Morocco") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(Morocco) # hand washing facilities completely missing, 

Morocco <- Morocco %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
Morocco_clean <- Morocco %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(Morocco_clean, is.numeric)
Morocco_clean_scaled <- scale(Morocco_clean[, numeric_columns])

# After scaling, ensure `Morocco_data_scaled` is still a data frame or matrix
Morocco_clean_scaled <- as.data.frame(Morocco_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(Morocco_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
Morocco_cleaner <- Morocco_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

Morocco_ts <- as_tsibble(Morocco_cleaner, index = date)

# view seasonality trends
ggplot(Morocco_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: Morocco"
  )

ggplot(Morocco_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: Morocco"
  )

# Split Data
splits <- initial_time_split(Morocco_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
Morocco_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
Morocco_model <- prophet(df = NULL, growth = "linear", yearly.seasonality = FALSE,
                      weekly.seasonality = TRUE, daily.seasonality = FALSE,
                      seasonality.mode = "additive", fit = TRUE)

# Add Regressors
Morocco_model = add_regressor(Morocco_model,"new_deaths", standardize = FALSE)
Morocco_model = add_regressor(Morocco_model,"icu_patients", standardize = FALSE)
Morocco_model = add_regressor(Morocco_model,'positive_rate', standardize = FALSE)
Morocco_model = add_regressor(Morocco_model,'new_vaccinations', standardize = FALSE)
Morocco_model = add_regressor(Morocco_model,'stringency_index', standardize = FALSE)

# Morocco_model <- add_country_holidays(Morocco_model, country_name = "Morocco")
## "Holidays in Morocco are not currently supported"

# Fit Model
Morocco_fit = fit.prophet(Morocco_model, df = Morocco_train)

# Future df
future_Morocco <- make_future_dataframe(Morocco_fit, periods = 42, freq = "week")

# Future Regressors
future_Morocco$new_deaths = head(Morocco_ts$new_deaths, nrow(future_Morocco))
future_Morocco$icu_patients = head(Morocco_ts$icu_patients, nrow(future_Morocco))
future_Morocco$positive_rate = head(Morocco_ts$positive_rate, nrow(future_Morocco))
future_Morocco$new_vaccinations = head(Morocco_ts$new_vaccinations, nrow(future_Morocco))
future_Morocco$stringency_index = head(Morocco_ts$stringency_index, nrow(future_Morocco))

# Forecast
Morocco_forecast <- predict(Morocco_fit, future_Morocco)

# Prophet Plots
dyplot.prophet(Morocco_fit, Morocco_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(Morocco_fit, Morocco_forecast, weekly_start = 0)

# Predict Future Values
Morocco_future_preds <- Morocco_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

Morocco_preds <- bind_cols(test, Morocco_future_preds) %>% 
  rename(preds = yhat)

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

Morocco_metrics <- Morocco_preds %>% 
  covid_metrics(new_cases, estimate = preds)

Morocco_metrics

save(Morocco_metrics, Morocco_preds, file = "Models/Prophet - Multivariate/results_linear/Morocco_metrics.rda")

