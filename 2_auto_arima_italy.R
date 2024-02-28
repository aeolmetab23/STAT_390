library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)


######################### DATA LOADING
our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data/", i, "_uni.csv"))
}

# Italy
italy <- as_tsibble(
  as_tibble(country_data[["Italy"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)

# splitting the data - 70% split
split_italy <- ts_split(ts.obj = italy, sample.out = 63)

italy_train <- split_italy$train
italy_test <- split_italy$test

# removing dates - making data univariate
italy.ts_train <- as.ts(italy_train$new_cases)


autoA_fit <- auto.arima(italy.ts_train)
autoA_fit
autoA_fit$arma

autoA_preds <- predict(autoA_fit, n.ahead = 63)$pred

italy_autoA_preds <- bind_cols(italy_test, autoA_preds) %>% 
  rename("preds" = "...3")

# Mean Absolute Percent Error and Mean Absolute Scaled Error
MAPE(y_pred = italy_autoA_preds$new_cases, y_true = italy_autoA_preds$preds)
mase_vec(truth = italy_autoA_preds$new_cases, estimate = italy_autoA_preds$preds)



