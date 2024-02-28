library(prophet)
library(tidyverse)
library(ggplot2)
library(rstan)
library(simputation)

# load data
italy_big <- read.csv("data/covid_cleaner.csv") %>% 
  filter(location == "Italy") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  # impute na's in relevant columns with 0s
  mutate(hosp_patients = ifelse(is.na(hosp_patients), 0, hosp_patients),
         new_vaccinations = ifelse(is.na(new_vaccinations), 0, new_vaccinations))

italy_big <- italy_big %>% 
  # knn impute stringency index
  mutate(stringency_index = impute_knn(italy_big, 
                                       column = "stringency_index", 
                                       formula = stringency_index ~ .)$stringency_index) %>% 
  # fixing excess mortality
  mutate(excess_mortality = impute_knn(italy_big, 
                                       column = "excess_mortality", 
                                       formula = excess_mortality ~ .)$excess_mortality)

italy_big <- as_tsibble(italy_big, index = date)

# view seasonality trends
ggplot(italy_big) +
  geom_col(aes(x = week, y = new_cases, fill = week)) +
  theme_minimal()

ggplot(italy_big) +
  geom_line(aes(x = date, y = new_cases)) +
  theme_minimal()

# splitting the data - 80% split
split_italy_big <- ts_split(ts.obj = italy_big, sample.out = 42)

italy_big_train <- split_italy_big$train
italy_big_test <- split_italy_big$test

# rename columns to match prophet syntax
italy_big_train <- italy_big_train %>% 
  rename(ds = date, y = new_cases)

# model
mpfit_italy <- (prophet(
  df = NULL,
  growth = "linear", # consider "flat" and "logistic" in next models
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL, # need to look into italian holidays 
  seasonality.mode = "additive", # by observation
  fit = TRUE
))

# adding external regressors
mpfit_italy = add_regressor(mpfit_italy,"new_deaths",standardize = FALSE)
mpfit_italy = add_regressor(mpfit_italy,"hosp_patients", standardize = FALSE)
mpfit_italy = add_regressor(mpfit_italy,'new_vaccinations', standardize = FALSE)
mpfit_italy = add_regressor(mpfit_italy,'stringency_index', standardize = FALSE)
mpfit_italy = add_regressor(mpfit_italy,'excess_mortality', standardize = FALSE)

# model fitting with training data
mpfit_italy = fit.prophet(mpfit_italy, df = italy_big_train)

# making future df
future_italy <- make_future_dataframe(mpfit_italy, periods = 42, freq = "week")

# build out columns of values for extra regressors
future_italy$new_deaths = head(italy_big$new_deaths ,nrow(future_italy))
future_italy$hosp_patients = head(italy_big$hosp_patients ,nrow(future_italy))
future_italy$new_vaccinations = head(italy_big$new_vaccinations, nrow(future_italy))
future_italy$stringency_index = head(italy_big$stringency_index, nrow(future_italy))
future_italy$excess_mortality = head(italy_big$excess_mortality, nrow(future_italy))

####### forecasting
mp_forecast <- predict(mpfit_italy, future_italy)

##### plots
dyplot.prophet(mpfit_italy, mp_forecast, uncertainty = TRUE)

# components of model
prophet_plot_components(
  mpfit_italy,
  mp_forecast,
  uncertainty = TRUE,
  plot_cap = TRUE,
  yearly_start = 0,
  render_plot = TRUE
)

# preds of future specifically
italy_mp_future_preds <- mp_forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

italy_mp_preds <- bind_cols(italy_big_test, italy_mp_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(preds = ifelse(preds < 0 , 0, preds))

# test statistics
rmse_vec(truth = italy_mp_preds$new_cases, estimate = italy_mp_preds$preds)
mase_vec(truth = italy_mp_preds$new_cases, estimate = italy_mp_preds$preds)
round(MAPE(y_pred = italy_mp_preds$new_cases, y_true = italy_mp_preds$preds), 6)
mae_vec(truth = italy_mp_preds$new_cases, estimate = italy_mp_preds$preds)




##################################### Experimenting now with growth as flat
mpfit_italy2 <- (prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "additive", # by observation
  fit = TRUE
))

mpfit_italy2 = add_regressor(mpfit_italy2,"new_deaths",standardize = FALSE)
mpfit_italy2 = add_regressor(mpfit_italy2,"hosp_patients", standardize = FALSE)
mpfit_italy2 = add_regressor(mpfit_italy2,'new_vaccinations', standardize = FALSE)
mpfit_italy2 = add_regressor(mpfit_italy2,'stringency_index', standardize = FALSE)
mpfit_italy2 = add_regressor(mpfit_italy2,'excess_mortality', standardize = FALSE)

# mpfit_italy2 <- add_country_holidays(mpfit_italy2, country_name = "Italy")

mpfit_italy2 = fit.prophet(mpfit_italy2, df = italy_big_train)

mp_forecast2 <- predict(mpfit_italy2, future_italy)

# preds of future specifically
italy_mp_future_preds2 <- mp_forecast2 %>% 
  select(yhat) %>% 
  tail(n = 42)

# plot
dyplot.prophet(mpfit_italy, mp_forecast, uncertainty = TRUE)

italy_mp_preds2 <- bind_cols(italy_big_test, italy_mp_future_preds2) %>% 
  rename(preds = yhat) %>% 
  mutate(preds = ifelse(preds < 0 , 0, preds))


# test statistics
rmse_vec(truth = italy_mp_preds$new_cases, estimate = italy_mp_preds$preds)
mase_vec(truth = italy_mp_preds$new_cases, estimate = italy_mp_preds$preds)
round(MAPE(y_pred = italy_mp_preds$new_cases, y_true = italy_mp_preds$preds), 6)
mae_vec(truth = italy_mp_preds$new_cases, estimate = italy_mp_preds$preds)
# performed significantly better with the flat growth rate parameter
# MAPE and MASE better without additional regressors. MASE best with first 3 regressors, MAPE worst
# components of model
prophet_plot_components(
  mpfit_italy2,
  mp_forecast2,
  uncertainty = TRUE,
  plot_cap = TRUE,
  yearly_start = 0,
  render_plot = TRUE
)

