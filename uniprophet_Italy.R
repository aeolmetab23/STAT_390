library(prophet)
library(tidyverse)
library(ggplot2)
library(rstan)

######################### DATA LOADING
our_countries <- c("Italy", "Mexico", "India", "Germany", "Australia",
                   "Japan", "Ireland", "Denmark", "Brazil", "Egypt")

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














