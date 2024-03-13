library(prophet)
library(tidyverse)
library(ggplot2)
library(rstan)

######################### DATA LOADING
our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data/", i, "_uni.csv"))
}


# set up results table & save out model fits
prophet_u2_results <- tibble(country = c(), rmse = c(), mae = c(), mase = c(), mape = c())
prophet_fits <- list()

# main loop
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
  
  country_train_prophet <- country_train %>% 
    rename("ds" = date, "y" = new_cases)
  
  # cross validation
  prophet_results_country <- tibble(cps = c(), sps = c(), mae = c())
  cpss <- c(.002, .005, .007)
  spss <- c(.5, 1, 2)
  for (cp in cpss){
    for (sp in spss) {
      pro_fit <- prophet(country_train_prophet, 
                         changepoint.prior.scale = cp,
                         seasonality.prior.scale = sp)
      country_cv <- cross_validation(pro_fit, horizon = 70, units = "days")
      mae <- mae_vec(truth = country_cv$y, estimate = country_cv$yhat)
      prophet_results_country <- prophet_results_country %>% 
        bind_rows(tibble(cps = cp, sps = sp, mae = mae))
    }
  }
  
  # extracting best parameters from cv results
  best <- prophet_results_country %>% 
    arrange(mae) %>% 
    slice_head(n = 1)
  
  cps <- best %>% select(cps) %>% pull
  sps <- best %>% select(sps) %>% pull
  
  # fit model
  pro_fit <- prophet(country_train_prophet,
                     changepoint.prior.scale = cps,
                     seasonality.prior.scale = sps)
  
  # create future dataframe and make predictions
  country_future <- make_future_dataframe(pro_fit, period = 35, freq = "week")
  forecast <- predict(pro_fit, country_future)
  country_future_preds <- forecast %>% 
    select(yhat) %>% 
    tail(n = 35)
  
  country_pro_preds <- bind_cols(country_test, country_future_preds) %>% 
    rename(preds = yhat) %>% 
    mutate(preds = ifelse(preds < 0, 0, preds))
  
  # test statistics
  rmse <- rmse_vec(truth = country_pro_preds$new_cases, estimate = country_pro_preds$preds)
  mase <- mase_vec(truth = country_pro_preds$new_cases, estimate = country_pro_preds$preds)
  mape <- round(MAPE(y_pred = country_pro_preds$new_cases, y_true = country_pro_preds$preds), 6)
  mae <- mae_vec(truth = country_pro_preds$new_cases, estimate = country_pro_preds$preds)
  
  
  # save out results
  prophet_u2_results <- prophet_u2_results %>% 
    bind_rows(tibble(country = i, rmse = rmse, mae = mae, mase = mase, mape = mape))
  prophet_fits <- c(prophet_fits, pro_fit)
  
}

