library(prophet)
library(tidyverse)
library(ggplot2)
library(rstan)
library(simputation)
library(zoo)
library(yardstick)


#load data
data_big <- read.csv("data/covid_cleaner.csv") %>%
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date >= "2020-03-01", date < "2023-07-02") %>% 
  mutate(new_cases_1week_lag = ifelse(is.na(new_cases_1week_lag), 0, new_cases_1week_lag),
         new_cases_2week_lag = ifelse(is.na(new_cases_2week_lag), 0, new_cases_2week_lag),
         new_cases_6week_roll = ifelse(is.na(new_cases_6week_roll), 0, new_cases_6week_roll),
         new_cases_12week_roll = ifelse(is.na(new_cases_12week_roll), 0, new_cases_12week_roll)) %>%
  rename(ds = date, y = new_cases)

# >= 2022-10-30 for test set
# split data to perform knn on
data_big_test <- data_big %>%
  filter(ds >= "2022-10-30") 

data_big_test <- data_big_test %>% 
  mutate(icu_patients = impute_knn(data_big_test,
                                   column = "icu_patients",
                                   formula = icu_patients ~ new_cases_1week_lag +
                                     population + human_development_index + life_expectancy +
                                     handwashing_facilities + diabetes_prevalence + cardiovasc_death_rate +
                                     gdp_per_capita + aged_65_older + median_age + population_density +
                                     stringency_index)$icu_patients) %>%
  mutate(stringency_index = impute_knn(data_big_test,
                                   column = "stringency_index",
                                   formula = stringency_index ~ new_cases_1week_lag +
                                     population + human_development_index + life_expectancy +
                                     handwashing_facilities + diabetes_prevalence + cardiovasc_death_rate +
                                     gdp_per_capita + aged_65_older + median_age +
                                     population_density)$stringency_index)

data_big_train <- data_big %>%
  filter(ds < "2022-10-30") 

data_big_train <- data_big_train %>% 
  mutate(icu_patients = impute_knn(data_big_train,
                                   column = "icu_patients",
                                   formula = icu_patients ~ new_cases_1week_lag +
                                     population + human_development_index + life_expectancy +
                                     handwashing_facilities + diabetes_prevalence + cardiovasc_death_rate +
                                     gdp_per_capita + aged_65_older + median_age + population_density +
                                     stringency_index)$icu_patients) %>%
  mutate(stringency_index = impute_knn(data_big_train,
                                       column = "stringency_index",
                                       formula = stringency_index ~ new_cases_1week_lag +
                                         population + human_development_index + life_expectancy +
                                         handwashing_facilities + diabetes_prevalence + cardiovasc_death_rate +
                                         gdp_per_capita + aged_65_older + median_age +
                                         population_density)$stringency_index)

data_big <- bind_rows(data_big_train, data_big_test)

our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina",
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

prophet_mp_fits <- c()
components <- c()
prophet_mp_results2 <- tibble(country = c(), rmse = c(), mae = c(), mase = c())
# main loop
for (i in our_countries) {
  # get country data
  country <- data_big %>% 
    filter(location == i)
  
  # splitting the data - 80% split
  country_big_train <- country %>% filter(ds < "2022-10-30")
  country_big_test <- country %>% filter(ds >= "2022-10-30")
  
  # model
  mpfit_country <- (prophet(
    df = NULL,
    growth = "linear", # consider "flat" and "logistic" in next models
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    holidays = NULL, # need to look into holidays
    seasonality.mode = "additive", # by observation
    fit = TRUE
  ))
  
  # adding external regressors
  mpfit_country = add_regressor(mpfit_country,"new_deaths",standardize = FALSE)
  mpfit_country = add_regressor(mpfit_country,"new_cases_2week_lag", standardize = FALSE)
  mpfit_country = add_regressor(mpfit_country,'icu_patients', standardize = FALSE)
  mpfit_country = add_regressor(mpfit_country,'stringency_index', standardize = FALSE)
  
  # model fitting with training data
  mpfit_country = fit.prophet(mpfit_country, df = country_big_train)
  
  # making future df
  future_country <- make_future_dataframe(mpfit_country, periods = 35)
  
  # build out columns of values for extra regressors
  future_country$new_deaths = head(country$new_deaths, nrow(future_country))
  future_country$new_cases_2week_lag = head(country$new_cases_2week_lag ,nrow(future_country))
  future_country$icu_patients = head(country$icu_patients, nrow(future_country))
  future_country$stringency_index = head(country$stringency_index, nrow(future_country))
  
  ####### forecasting
  mp_forecast <- predict(mpfit_country, future_country)
  
  # preds of future specifically
  mp_future_preds <- mp_forecast %>% 
    select(yhat) %>% 
    tail(n = 35)
  
  country_mp_preds <- bind_cols(country_big_test, mp_future_preds) %>% 
    rename(preds = yhat) %>% 
    mutate(preds = ifelse(preds < 0 , 0, preds))
  
  # test statistics
  rmse <- rmse_vec(truth = country_mp_preds$y, estimate = country_mp_preds$preds)
  mase <- mase_vec(truth = country_mp_preds$y, estimate = country_mp_preds$preds)
  mae <- mae_vec(truth = country_mp_preds$y, estimate = country_mp_preds$preds)

  # components of model
  country_components <- prophet_plot_components(
    mpfit_country,
    mp_forecast,
    uncertainty = TRUE,
    plot_cap = TRUE,
    yearly_start = 0,
    render_plot = TRUE
  )
  
  
  # save out results
  prophet_mp_results2 <- prophet_mp_results2 %>% 
    bind_rows(tibble(country = i, rmse = rmse, mae = mae, mase = mase))
  prophet_mp_fits <- c(prophet_mp_fits, mpfit_country)
  components <- c(components, country_components)
  
  save(prophet_mp_results2, prophet_mp_fits, components, file = "results/prophet2_mp.rda")
  
}

load("results/prophet2_mp.rda")

components[[28]]
