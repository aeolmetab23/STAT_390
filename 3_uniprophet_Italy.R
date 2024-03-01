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

italy <- as_tsibble(
  as_tibble(country_data[["Italy"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)

# splitting the data - 70% split
split_italy <- ts_split(ts.obj = italy, sample.out = 63)

italy_train <- split_italy$train
italy_test <- split_italy$test


######################### MODELING
italy_train_prophet <- italy_train %>% 
  rename("ds" = date, "y" = new_cases)

pro_fit <- prophet(italy_train_prophet)

italy_future <- make_future_dataframe(pro_fit, period = 63, freq = "week")
forecast <- predict(pro_fit, italy_future)
plot(pro_fit, forecast)

italy_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 63)

italy_pro_preds <- bind_cols(italy_test, italy_future_preds) %>% 
  rename(preds = yhat)

ggplot(italy_pro_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

# Mean Absolute Percent Error and Mean Absolute Scaled Error
MAPE(y_pred = italy_pro_preds$new_cases, y_true = italy_pro_preds$preds)
mase_vec(truth = italy_pro_preds$new_cases, estimate = italy_pro_preds$preds)
mae_vec(truth = italy_pro_preds$new_cases, estimate = italy_pro_preds$preds)



########################################## Uniprophet Malaysia
our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data/", i, "_uni.csv"))
}


malaysia <- as_tsibble(
  as_tibble(country_data[["Malaysia"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)

# splitting
split <- ts_split(ts.obj = malaysia, sample.out = 42)
malaysia_train <- split$train
malaysia_test <- split$test

malaysia_train_prophet <- malaysia_train %>% 
  rename("ds" = date, "y" = new_cases)

# fit model
pro_fit <- prophet(malaysia_train_prophet)

# create future df and make predictions
malaysia_future <- make_future_dataframe(pro_fit, period = 42, freq = "week")
forecast <- predict(pro_fit, malaysia_future)
malaysia_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 42)

malaysia_pro_preds <- bind_cols(malaysia_test, malaysia_future_preds) %>% 
  rename(preds = yhat)

# test statistics
rmse_vec(truth = malaysia_pro_preds$new_cases, estimate = malaysia_pro_preds$preds)
mase_vec(truth = malaysia_pro_preds$new_cases, estimate = malaysia_pro_preds$preds)
round(MAPE(y_pred = malaysia_pro_preds$new_cases, y_true = malaysia_pro_preds$preds), 6)
mae_vec(truth = malaysia_pro_preds$new_cases, estimate = malaysia_pro_preds$preds)

#plotting
plot(pro_fit, forecast)
dyplot.prophet(pro_fit, forecast, uncertainty = TRUE)
# components of model
prophet_plot_components(
  pro_fit,
  forecast,
  uncertainty = TRUE,
  plot_cap = TRUE,
  yearly_start = 0,
  render_plot = TRUE
)


######################## trying again with holidays
holidays <- data.frame(
  holiday = c("Chinese New Year", "Hari Raya", "Christmas", "New Year", "Chinese New Year", "Hari Raya", "Agong's Bday",
              "Malaysia Day"),
  ds = as.Date(c("2022-01-30", "2022-05-01", "2022-12-25", "2023-01-01", "2023-01-22", "2023-04-23", "2023-06-04
", "2023-09-17")),
  lower_window = 0,
  upper_window = 0
)

pro_fit2 <- (prophet(
  df = NULL,
  growth = "flat",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = holidays,
  fit = TRUE
))

pro_fit2 = fit.prophet(pro_fit2, df = malaysia_train_prophet)

forecast2 <- predict(pro_fit2, malaysia_future)
malaysia_future_preds2 <- forecast2 %>% 
  select(yhat) %>% 
  tail(n = 42)

malaysia_pro_preds2 <- bind_cols(malaysia_test, malaysia_future_preds2) %>% 
  rename(preds = yhat)

mase_vec(truth = malaysia_pro_preds2$new_cases, estimate = malaysia_pro_preds2$preds)

