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
arima_results3 <- tibble(country = c(), p = c(), q = c(), rmse = c(), mae = c(), mase = c())

for (i in our_countries){
  country <- as_tsibble(
    as_tibble(country_data[[i]]) %>% 
      mutate(date = lubridate::ymd(date)),
    index = date
  )
  
  # reducing data
  country <- country %>% 
    filter(date >= "2020-03-01", date < "2023-07-02")
  
  # splitting - 80% split
  split <- ts_split(ts.obj = country, sample.out = 35)
  country_train <- split$train
  country_test <- split$test
  
  # removing dates - making data univariate
  country.ts_train <- as.ts(country_train$new_cases)
  ################### grid search
  ps <- seq(0:4)
  qs <- seq(0:4)
  ds <- seq(0:2)
  country_results <- tibble(p = c(), d = c(), q = c(), aic = c())
  
  for (p in ps) {
    for (q in qs) {
      for (d in ds) {
        fit <- Arima(country.ts_train, order = c(p,d,q), method = "CSS")
        aic <- fit$aic
        country_results <- bind_rows(country_results, tibble(p = p, d = d, q = q, aic = aic)) %>% 
          arrange(aic)
      }
    }
  }
  
  p_best <- country_results$p[1]
  d_best <- country_results$d[1]
  q_best <- country_results$q[1]
  
  # running model with best result from grid search
  fit <- Arima(country.ts_train, order=c(p_best,d_best,q_best), method = "ML")
  
  preds <- predict(fit, n.ahead = 35)$pred
  
  country_preds <- bind_cols(country_test, preds) %>% 
    rename("preds" = "...3") %>% 
    mutate(preds = ifelse(preds < 0, 0, preds))
  
  # Root Mean Squared Error and Mean Absolute Scaled Error
  rmse <- rmse_vec(truth = country_preds$new_cases, estimate = country_preds$preds)
  mase <- mase_vec(truth = country_preds$new_cases, estimate = country_preds$preds)
  mae <- mae_vec(truth = country_preds$new_cases, estimate = country_preds$preds)
  
  # save out results
  arima_results3 <- arima_results3 %>% 
    bind_rows(tibble(country = i, p = p_best, d = d_best, q = q_best, rmse = rmse, mae = mae, mase = mase))
  save(arima_results3, file = "results/arima_results3.rda")
}

load("results/arima_results3.rda")

