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
arima_results <- tibble(country = c(), p = c(), q = c(), rmse = c(), mae = c(), mase = c(), mape = c())

for (i in our_countries){
  country <- as_tsibble(
    as_tibble(country_data[[i]]) %>% 
      mutate(date = lubridate::ymd(date)),
    index = date
  )
  
  # splitting - 80% split
  split <- ts_split(ts.obj = country, sample.out = 42)
  country_train <- split$train
  country_test <- split$test
  
  # removing dates - making data univariate
  country.ts_train <- as.ts(country_train$new_cases)
  ################### grid search
  ps <- seq(0:4)
  qs <- seq(0:4)
  country_results <- tibble(p = c(), q = c(), aic = c())
  
  for (p in ps) {
    for (q in qs) {
      fit <- Arima(country.ts_train, order = c(p,1,q), method = "ML")
      aic <- fit$aic
      country_results <- bind_rows(country_results, tibble(p = p, q = q, aic = aic)) %>% 
        arrange(aic)
    }
  }
  
  p_best <- country_results$p[1]
  q_best <- country_results$q[1]
  
  # running model with best result from grid search
  fit <- Arima(country.ts_train, order=c(p_best,1,q_best), method = "ML")
  
  preds <- predict(fit, n.ahead = 42)$pred
  
  country_preds <- bind_cols(country_test, preds) %>% 
    rename("preds" = "...3") %>% 
    mutate(preds = ifelse(preds < 0, 0, preds))
  
  # Root Mean Squared Error and Mean Absolute Scaled Error
  rmse <- rmse_vec(truth = country_preds$new_cases, estimate = country_preds$preds)
  mase <- mase_vec(truth = country_preds$new_cases, estimate = country_preds$preds)
  mape <- round(MAPE(y_pred = country_preds$new_cases, y_true = country_preds$preds), 6)
  mae <- mae_vec(truth = country_preds$new_cases, estimate = country_preds$preds)
  
  # save out results
  arima_results <- arima_results %>% 
    bind_rows(tibble(country = i, p = p_best, q = q_best, rmse = rmse, mae = mae, mase = mase, mape = mape))
  save(arima_results, file = "results/arima_results.rda")
}

load("results/arima_results.rda")

