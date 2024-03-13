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

# reducing data
italy <- italy %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")

# splitting the data - 80% split
split_italy <- ts_split(ts.obj = italy, sample.out = 35)

italy_train <- split_italy$train
italy_test <- split_italy$test


######################### MODELING
italy_train_prophet <- italy_train %>% 
  rename("ds" = date, "y" = new_cases) 

pro_fit <- prophet(italy_train_prophet)
pro_fit <- prophet(italy_train_prophet, 
                   growth = "flat", 
                   yearly.seasonality = TRUE,
                   )

prophet_results <- tibble(num = c(), range = c(), mae = c())
changepointsnum <- seq(0, 10, 1)
changepoint.range <- seq(.6,.9,.1)
for (j in changepoint.range){
  for (i in changepointsnum) {
    pro_fit <- prophet(italy_train_prophet, 
                       n.changepoints = i,
                       changepoint.range = j)
    italy_future <- make_future_dataframe(pro_fit, period = 35, freq = "week")
    forecast <- predict(pro_fit, italy_future)
    italy_future_preds <- forecast %>% 
      select(yhat) %>% 
      tail(n = 35)
    italy_pro_preds <- bind_cols(italy_test, italy_future_preds) %>% 
      rename(preds = yhat)  %>% 
      mutate(preds = ifelse(preds < 0, 0, preds))
    mae <- mae_vec(truth = italy_pro_preds$new_cases, estimate = italy_pro_preds$preds)
    prophet_results <- prophet_results %>% 
      bind_rows(tibble(num = i, range = j, mae = mae))
  }
}

pro_fit <- prophet(italy_train_prophet, n.changepoints = 2, changepoint.range = .6)

pro_fit_cv <- cross_validation(pro_fit, horizon = 8*7, units = "days", period = 16*7 ,initial = 32*7)

italy_future <- make_future_dataframe(pro_fit, period = 35, freq = "week")
forecast <- predict(pro_fit, italy_future)
plot(pro_fit, forecast)
plot(pro_fit, forecast, xlab="Week", ylab="New Cases") +
  add_changepoints_to_plot(pro_fit)
pro_fit$changepoints


italy_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 35)

italy_pro_preds <- bind_cols(italy_test, italy_future_preds) %>% 
  rename(preds = yhat)  %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

ggplot(italy_pro_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

########### with cv - junk
pro_fit_cv <- cross_validation(pro_fit, horizon = 8*7, units = "days", period = 16*7 ,initial = 32*7)
beteter <- fit.prophet(pro_fit_cv, italy_train_prophet)

italy_future <- make_future_dataframe(pro_fit_cv, period = 35, freq = "week")
forecast_cv <- predict(pro_fit_cv, italy_future)
plot(pro_fit_cv, forecast_cv)
plot(pro_fit_cv, forecast_cv, xlab="Week", ylab="New Cases") +
  add_changepoints_to_plot(pro_fit_cv)
pro_fit$changepoints


italy_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 35)

italy_pro_preds <- bind_cols(italy_test, italy_future_preds) %>% 
  rename(preds = yhat)  %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

ggplot(italy_pro_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

# Mean Absolute Scaled Error
mase_vec(truth = italy_pro_preds$new_cases, estimate = italy_pro_preds$preds)
mae_vec(truth = italy_pro_preds$new_cases, estimate = italy_pro_preds$preds)

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



###### cv attempt 2
prophet_results <- tibble(cps = c(), sps = c(), mae = c())
cpss <- c(.002, .005, .007, .1)
spss <- c(.1, 1, 3, 6, 10)
for (j in cpss){
  for (i in spss) {
    pro_fit <- prophet(italy_train_prophet, 
                       changepoint.prior.scale = j,
                       seasonality.prior.scale = i)
    italy_cv <- cross_validation(pro_fit, horizon = 70, units = "days")
    mae <- mae_vec(truth = italy_cv$y, estimate = italy_cv$yhat)
    prophet_results <- prophet_results %>% 
      bind_rows(tibble(cps = j, sps = i, mae = mae))
  }
}

best <- prophet_results %>% 
  arrange(mae) %>% 
  slice_head(n = 1)

best %>% select(cps) %>% pull
best %>% select(sps) %>% pull


# with winning parameters
pro_fit_best <- prophet(italy_train_prophet, 
                        changepoint.prior.scale = .005, 
                        seasonality.prior.scale = 1)

italy_future <- make_future_dataframe(pro_fit_best, period = 35, freq = "week")
forecast <- predict(pro_fit_best, italy_future)
plot(pro_fit_best, forecast)
plot(pro_fit_best, forecast, xlab="Week", ylab="New Cases") +
  add_changepoints_to_plot(pro_fit_best)
pro_fit_best$changepoints

italy_future_preds <- forecast %>% 
  select(yhat) %>% 
  tail(n = 35)

italy_pro_preds <- bind_cols(italy_test, italy_future_preds) %>% 
  rename(preds = yhat)  %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

ggplot(italy_pro_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

# Mean Absolute Scaled Error
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

