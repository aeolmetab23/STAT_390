
# Prophet: UK -------------------------------------------------------

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
UK <- covid %>% 
  filter(location == "United Kingdom") %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  arrange(date)

skimr::skim_without_charts(UK) # hand washing facilities completely missing, 

UK <- UK %>% 
  select(
    -c(handwashing_facilities)
  )

# Opt to change NA's to Zeros
UK_clean <- UK %>% 
  mutate(
    hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
    icu_patients = ifelse(is.na(icu_patients), 0, icu_patients),
    new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations)
  )

# Begin Imputation
numeric_columns <- sapply(UK_clean, is.numeric)
UK_clean_scaled <- scale(UK_clean[, numeric_columns])

# After scaling, ensure `UK_data_scaled` is still a data frame or matrix
UK_clean_scaled <- as.data.frame(UK_clean_scaled)

# Performing multiple imputation
library(mice)
imputed_data <- mice(UK_clean_scaled, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

# Replace the original column with the imputed column, unscaling if necessary
UK_cleaner <- UK_clean %>% 
  mutate(
    positive_rate = completed_data$positive_rate,
    excess_mortality = completed_data$excess_mortality,
    stringency_index = completed_data$stringency_index,
    people_vaccinated = completed_data$people_vaccinated
  )

UK_ts <- as_tsibble(UK_cleaner, index = date)

# view seasonality trends
ggplot(UK_ts, aes(x = factor(month), y = new_cases)) +
  geom_col(show.legend = F, fill = "#583c83") +
  theme_minimal() +
  scale_fill_viridis_c(option = "H") +
  labs(
    x = "Month",
    y = "New Cases",
    fill = "",
    title = "New Cases by Month",
    subtitle = "Location: UK"
  )

ggplot(UK_ts) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal() +
  labs(
    x = "",
    y = "New Cases",
    fill = "",
    title = "New Cases Since 2020",
    subtitle = "Location: UK"
  )

# Split Data
splits <- initial_time_split(UK_ts, prop = 0.8)

train <- training(splits)
test <- testing(splits)

# Rename Columns ----
UK_train <- train %>% 
  rename("ds" = date, "y" = new_cases)

# model
UK_model <- prophet(df = NULL, growth = "linear", yearly.seasonality = FALSE,
                    weekly.seasonality = TRUE, daily.seasonality = FALSE,
                    seasonality.mode = "additive", fit = TRUE)

# Add Regressors
UK_model = add_regressor(UK_model,"new_deaths", standardize = FALSE)
UK_model = add_regressor(UK_model,"icu_patients", standardize = FALSE)
UK_model = add_regressor(UK_model,'positive_rate', standardize = FALSE)
UK_model = add_regressor(UK_model,'new_vaccinations', standardize = FALSE)
UK_model = add_regressor(UK_model,'stringency_index', standardize = FALSE)
UK_model = add_regressor(UK_model,'excess_mortality', standardize = FALSE)

# UK_model <- add_country_holidays(UK_model, country_name = "UK")
## "Holidays in UK are not currently supported"

# Fit Model
UK_fit = fit.prophet(UK_model, df = UK_train)

# Future df
future_UK <- make_future_dataframe(UK_fit, periods = 42, freq = "week")

# Future Regressors
future_UK$new_deaths = head(UK_ts$new_deaths, nrow(future_UK))
future_UK$icu_patients = head(UK_ts$icu_patients, nrow(future_UK))
future_UK$positive_rate = head(UK_ts$positive_rate, nrow(future_UK))
future_UK$new_vaccinations = head(UK_ts$new_vaccinations, nrow(future_UK))
future_UK$stringency_index = head(UK_ts$stringency_index, nrow(future_UK))
future_UK$excess_mortality = head(UK_ts$excess_mortality, nrow(future_UK))

# Forecast
UK_forecast <- predict(UK_fit, future_UK)

# Prophet Plots
dyplot.prophet(UK_fit, UK_forecast, uncertainty = TRUE)

# Model Components
prophet_plot_components(UK_fit, UK_forecast, weekly_start = 0)

# Predict Future Values
UK_future_preds <- UK_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

UK_preds <- bind_cols(test, UK_future_preds) %>% 
  rename(preds = yhat)

# Metrics
covid_metrics <- metric_set(rmse, mase, mae)

UK_metrics <- UK_preds %>% 
  covid_metrics(new_cases, estimate = preds)

UK_metrics

save(UK_metrics, UK_preds, file = "Models/Prophet - Multivariate/results_linear/UK_metrics.rda")

