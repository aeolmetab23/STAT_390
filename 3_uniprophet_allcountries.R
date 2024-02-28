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
prophet_results <- tibble(country = c(), rmse = c(), mae = c(), mase = c(), mape = c())
prophet_fits <- list()

# main loop
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
  
  country_train_prophet <- country_train %>% 
    rename("ds" = date, "y" = new_cases)
  
  # fit model
  pro_fit <- prophet(country_train_prophet)
  
  # create future dataframe and make predictions
  country_future <- make_future_dataframe(pro_fit, period = 42, freq = "week")
  forecast <- predict(pro_fit, country_future)
  country_future_preds <- forecast %>% 
    select(yhat) %>% 
    tail(n = 42)
  
  country_pro_preds <- bind_cols(country_test, country_future_preds) %>% 
    rename(preds = yhat)
  
  # test statistics
  rmse <- rmse_vec(truth = country_pro_preds$new_cases, estimate = country_pro_preds$preds)
  mase <- mase_vec(truth = country_pro_preds$new_cases, estimate = country_pro_preds$preds)
  mape <- round(MAPE(y_pred = country_pro_preds$new_cases, y_true = country_pro_preds$preds), 6)
  mae <- mae_vec(truth = country_pro_preds$new_cases, estimate = country_pro_preds$preds)
  
  
  # save out results
  prophet_results <- prophet_results %>% 
    bind_rows(tibble(country = i, rmse = rmse, mae = mae, mase = mase, mape = mape))
  prophet_fits <- c(prophet_fits, pro_fit)
  
}

