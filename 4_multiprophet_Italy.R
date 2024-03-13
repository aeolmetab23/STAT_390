library(prophet)
library(tidyverse)
library(ggplot2)
library(rstan)
library(simputation)
library(patchwork)

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




################################### Peru
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


# splitting the data - 80% split
peru <- data_big %>% 
  filter(location == "Peru")
peru_big_train <- peru %>% filter(ds < "2022-10-30") 
peru_big_test <- peru %>% filter(ds >= "2022-10-30")

# model
mpfit_peru <- (prophet(
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
mpfit_peru = add_regressor(mpfit_peru,"new_deaths",standardize = FALSE)
mpfit_peru = add_regressor(mpfit_peru,"new_cases_2week_lag", standardize = FALSE)
mpfit_peru = add_regressor(mpfit_peru,'icu_patients', standardize = FALSE)
mpfit_peru = add_regressor(mpfit_peru,'stringency_index', standardize = FALSE)
mpfit_peru = add_regressor(mpfit_peru,"new_cases_1week_lag", standardize = FALSE)
mpfit_peru = add_regressor(mpfit_peru,"new_cases_6week_roll", standardize = FALSE)
mpfit_peru = add_regressor(mpfit_peru,"new_cases_12week_roll", standardize = FALSE)


# model fitting with training data
mpfit_peru = fit.prophet(mpfit_peru, df = peru_big_train)

# making future df
future_peru <- make_future_dataframe(mpfit_peru, periods = 35)

# build out columns of values for extra regressors
future_peru$new_deaths = head(peru$new_deaths, nrow(future_peru))
future_peru$new_cases_2week_lag = head(peru$new_cases_2week_lag ,nrow(future_peru))
future_peru$icu_patients = head(peru$icu_patients, nrow(future_peru))
future_peru$stringency_index = head(peru$stringency_index, nrow(future_peru))
future_peru$new_cases_1week_lag = head(peru$new_cases_1week_lag, nrow(future_peru))
future_peru$new_cases_6week_roll = head(peru$new_cases_6week_roll, nrow(future_peru))
future_peru$new_cases_12week_roll = head(peru$new_cases_12week_roll, nrow(future_peru))

####### forecasting
mp_forecast <- predict(mpfit_peru, future_peru)

# preds of future specifically
mp_future_preds <- mp_forecast %>% 
  select(yhat) %>% 
  tail(n = 35)

peru_mp_preds <- bind_cols(peru_big_test, mp_future_preds) %>% 
  rename(preds = yhat) %>% 
  mutate(preds = ifelse(preds < 0 , 0, preds))

# test statistics
rmse_vec(truth = peru_mp_preds$y, estimate = peru_mp_preds$preds)
mase_vec(truth = peru_mp_preds$y, estimate = peru_mp_preds$preds)
mae_vec(truth = peru_mp_preds$y, estimate = peru_mp_preds$preds)

# components of model
plot <- prophet_plot_components(
  mpfit_peru,
  mp_forecast,
  uncertainty = TRUE,
  plot_cap = TRUE,
  yearly_start = 0,
  render_plot = TRUE
)

(plot[[1]] + 
    theme_minimal() +
    labs(x = NULL, y = "Trend", title = "Peru Prophet Decomposition")) /
(plot[[2]] + theme_minimal() +
   labs(x = NULL, y = "Yearly")) /
(plot[[3]] + theme_minimal() +
   labs(x = NULL, y = "Extra Regressors Additive"))


# plot
ggplot(peru_mp_preds) +
  geom_line(aes(x = ds, y = y), color = "blue") +
  geom_line(aes(x = ds, y = preds), color = "red")


colors <- c("new_cases" = "blue", "preds" = "red")
ggplot(peru_mp_preds, aes(x = ds)) +
  geom_line(aes(y = y, color = "new_cases")) +
  geom_line(aes(y = preds, color = "preds")) +
  labs(y = "New Cases", x = NULL, title = "Peru Multivariate Prophet - Final",
       subtitle = "Predictions & Actuals") +
  scale_x_date(labels = date_format("%m-%Y")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(labels = c("New Cases", "Predictions"), values = colors) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.72,.91),
        panel.grid.major = element_blank())
