# load packages
library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)
library(TSPred)
library(yardstick)
library(MLmetrics)


######################### DATA LOADING
our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data/", i, "_uni.csv"))
}

# set up results table
auto_arima_results <- tibble(country = c(), p = c(), d = c(), q = c(), rmse = c(), mae = c(), mase = c(), mape = c())

for (i in our_countries) {
  j <- as_tsibble(
    as_tibble(country_data[[i]]) %>% 
      mutate(date = lubridate::ymd(date)),
    index = date
  )
  
  # split_num <- case_when(
  #   i %in% c("Italy", "Mexico", "India", "Australia", "Ireland") ~ 63,
  #   i == "Germany" ~ 80
  # )
  # 
  
  # splitting
  split <- ts_split(ts.obj = j, sample.out = 42)
  country_train <- split$train
  country_test <- split$test
  
  # removing dates - making data univariate
  country.ts_train <- as.ts(country_train$new_cases)
  
  # fit model
  autoA_fit <- auto.arima(country.ts_train)
  
  # extract p,d,q values
  p <- autoA_fit$arma[1]
  q <- autoA_fit$arma[2]
  d <- autoA_fit$arma[6]
  
  # make predictions
  autoA_preds <- predict(autoA_fit, n.ahead = 42)$pred
  
  country_autoA_preds <- bind_cols(country_test, autoA_preds) %>% 
    rename("preds" = "...3")
  
  # test statistics
  mape <- round(MAPE(y_pred = country_autoA_preds$new_cases, y_true = country_autoA_preds$preds), 4)
  mase <- round(mase_vec(truth = country_autoA_preds$new_cases, estimate = country_autoA_preds$preds), 4)
  mae <- mae_vec(truth = country_autoA_preds$new_cases, estimate = country_autoA_preds$preds)
  rmse <- rmse_vec(truth = country_autoA_preds$new_cases, estimate = country_autoA_preds$preds)
  
  # save out results
  auto_arima_results <- auto_arima_results %>% 
    bind_rows(tibble(country = i, p = p, d = d, q = q, rmse = rmse, mae = mae, mase = mase, mape = mape))
  
}




