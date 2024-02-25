library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)
library(TSPred)
library(yardstick)
library(MLmetrics)

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

mexico <- as_tsibble(
  as_tibble(country_data[["Germany"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)
 
# splitting the data - 70% split
split_italy <- ts_split(ts.obj = italy, sample.out = 63)

italy_train <- split_italy$train
italy_test <- split_italy$test

# plotting ACF and PACF
italy_train %>% 
  feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")


# removing dates - making data univariate
italy.ts_train <- as.ts(italy_train$new_cases)

italy.ts_train %>% 
  ggtsdisplay(main = "")

fit <- Arima(italy.ts_train, order=c(4,1,4))
checkresiduals(fit)
fit$aic

# messing around with plotting
plotarimapred(ts.cont = italy.ts_train, fit.arima = fit, xlim = c(130, 147))
predict(fit, italy.ts_train, start = 130, end = 147)
predict(fit)
fit %>% forecast() %>% autoplot()


################### grid search
ps <- seq(0:4)
qs <- seq(0:4)
results <- tibble(p = c(), q = c(), aic = c())

for (p in ps) {
  for (q in qs) {
    fit <- Arima(italy.ts_train, order = c(p,1,q))
    aic <- fit$aic
    results <- bind_rows(results, tibble(p = p, q = q, aic = aic)) %>% 
      arrange(aic)
  }
}

# running model with best result from grid search (p=1, q=5)
fit <- Arima(italy.ts_train, order=c(1,1,5))
fit %>% forecast() %>% autoplot()
fc_Arima <- forecast(fit, 52)
autoplot(fc_Arima)

preds <- predict(fit, n.ahead = 63)$pred

italy_preds <- bind_cols(italy_test, preds) %>% 
  rename("preds" = "...3")

ggplot(italy_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

# Mean Absolute Percent Error and Mean Absolute Scaled Error
MAPE(y_pred = italy_preds$new_cases, y_true = italy_preds$preds)
mase_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)


fit2 <- Arima(italy.ts_train, order=c(4,1,2))
fit2 %>% forecast() %>% autoplot()
preds2 <- predict(fit2, n.ahead = 63)$pred
italy_preds2 <- bind_cols(italy_test, preds2) %>% 
  rename("preds" = "...3")
ggplot(italy_preds2) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

MAPE(y_pred = italy_preds2$new_cases, y_true = italy_preds2$preds)
mase_vec(truth = italy_preds2$new_cases, estimate = italy_preds2$preds)





