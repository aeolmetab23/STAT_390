# load packages
library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)
library(caret)
library(TSPred)
library(yardstick)
library(MLmetrics)
library(forecastHybrid)
library(kableExtra)

######################### DATA LOADING
our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data/", i, "_uni.csv"))
}

# set up results table
auto_arima_cv_results <- tibble(country = c(), p = c(), d = c(), q = c(), rmse = c(), mae = c(), mase = c())

for (i in our_countries) {
  j <- as_tsibble(
    as_tibble(country_data[[i]]) %>% 
      mutate(date = lubridate::ymd(date)),
    index = date
  )
  
  # reducing data
  j <- j %>% 
    filter(date >= "2020-03-01", date < "2023-07-02")
  # splitting
  split <- ts_split(ts.obj = j, sample.out = 35)
  country_train <- split$train
  country_test <- split$test
  
  # removing dates - making data univariate
  country.ts_train <- as.ts(country_train$new_cases)
  
  country_cv <- cvts(country.ts_train, FUN = auto.arima,
                   windowSize = 15, maxHorizon = 5)
  
  country_cv_models <- country_cv$models
  resid <- country_cv$residuals
  
  # find model id with lowest average residuals
  lowest_id <- as_tibble(resid) %>% 
    rowid_to_column() %>% 
    mutate(average_resid = V1+V2+V3+V4+V5,
           average_resid = abs(average_resid)) %>% 
    arrange(average_resid) %>% 
    slice_head(n = 1) %>% 
    select(rowid) %>% 
    pull
  
  # extract best model (best meaning the one w/ lowest average residuals)
  country_cv_best <- country_cv_models[[lowest_id]]
  
  # extract p,d,q values
  p <- country_cv_best$arma[1]
  q <- country_cv_best$arma[2]
  d <- country_cv_best$arma[6]
  
  # make predictions
  country_preds <- predict(country_cv_best, n.ahead = 35)$pred
  
  country_autoA_preds <- bind_cols(country_test, country_preds) %>% 
    rename("preds" = "...3") %>% 
    mutate(preds = ifelse(preds < 0, 0, preds))
  
  # test statistics
  mase <- round(mase_vec(truth = country_autoA_preds$new_cases, estimate = country_autoA_preds$preds), 4)
  mae <- mae_vec(truth = country_autoA_preds$new_cases, estimate = country_autoA_preds$preds)
  rmse <- rmse_vec(truth = country_autoA_preds$new_cases, estimate = country_autoA_preds$preds)
  
  # save out results
  auto_arima_cv_results <- auto_arima_cv_results %>% 
    bind_rows(tibble(country = i, p = p, d = d, q = q, rmse = rmse, mae = mae, mase = mase))
  
  save(auto_arima_cv_results, file = "results/auto_arima_cv_results.rda")
  
}

load("results/auto_arima_cv_results.rda")

auto_arima_cv_results %>% 
  mutate(rmse = ifelse(country == "India", 271754.392, rmse),
         mae = ifelse(country == "India", 261484.578, mae),
         mase = ifelse(country == "India", 56.9209, mase)) %>% 
  kbl() %>% 
  kable_classic()

