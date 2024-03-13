library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)
library(TSPred)
library(yardstick)
library(MLmetrics)
library(scales)

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

# splitting the data - 80% split - 35 in test set
split_italy <- ts_split(ts.obj = italy, sample.out = 35)

italy_train <- split_italy$train
italy_test <- split_italy$test

# plotting ACF and PACF
italy_train %>% 
  feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")


# removing dates - making data univariate
italy.ts_train <- as.ts(italy_train$new_cases)

italy.ts_train %>% 
  ggtsdisplay(main = "", theme = theme_minimal())

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
italy_results <- tibble(p = c(), q = c(), aic = c())

for (p in ps) {
  for (q in qs) {
    fit <- Arima(italy.ts_train, order = c(p,1,q))
    aic <- fit$aic
    italy_results <- bind_rows(italy_results, tibble(p = p, q = q, aic = aic)) %>% 
      arrange(aic)
  }
}

italy_results$p[1]
italy_results$q[1]

# running model with best result from grid search (p=1, q=5)
fit <- Arima(italy.ts_train, order=c(1,1,5))
fit %>% forecast() %>% autoplot()
fc_Arima <- forecast(fit, 52)
autoplot(fc_Arima)

preds <- predict(fit, n.ahead = 35)$pred

italy_preds <- bind_cols(italy_test, preds) %>% 
  rename("preds" = "...3")

ggplot(italy_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

# Mean Absolute Percent Error and Mean Absolute Scaled Error
MAPE(y_pred = italy_preds$new_cases, y_true = italy_preds$preds)
mase_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)
# RMSE
rmse_vec(italy_preds$new_cases, italy_preds$preds)


fit2 <- Arima(italy.ts_train, order=c(4,1,2))
fit2 %>% forecast() %>% autoplot()
preds2 <- predict(fit2, n.ahead = 42)$pred
italy_preds2 <- bind_cols(italy_test, preds2) %>% 
  rename("preds" = "...3")
ggplot(italy_preds2) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

MAPE(y_pred = italy_preds2$new_cases, y_true = italy_preds2$preds)
mase_vec(truth = italy_preds2$new_cases, estimate = italy_preds2$preds)
mae_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)
rmse_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)



###################### With Cross Validation


#Fit an AR(2) model to each rolling origin subset
arima_func <- function(x, h){forecast(Arima(x, order=c(4,1,4)), h=h)}
cvfit <- tsCV(italy_train, arima_func, h=1, window=10)







###################################### Examining Morocco
morocco <- as_tsibble(
  as_tibble(country_data[["Morocco"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)
# reducing data
morocco <- morocco %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")
# splitting the data - 80% split
split_morocco <- ts_split(ts.obj = morocco, sample.out = 35)
morocco_train <- split_morocco$train
morocco_test <- split_morocco$test
# removing dates - making data univariate
morocco.ts_train <- as.ts(morocco_train$new_cases)
fit <- Arima(morocco.ts_train, order=c(1,1,1), method = "CSS")
preds <- predict(fit, n.ahead = 35)$pred
morocco_preds <- bind_cols(morocco_test, preds) %>% 
  rename("preds" = "...3")
colors <- c("new_cases" = "blue", "preds" = "red")
ggplot(morocco_preds, aes(x = date)) +
  geom_line(aes(y = new_cases, color = "new_cases")) +
  geom_line(aes(y = preds, color = "preds")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(labels = c("New Cases", "Predictions"), values = colors) +
  labs(x = NULL, y = "New Cases", title = "New Cases in Morocco") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.72,.91),
        panel.grid.major = element_blank())

###################################### Examining India
india <- as_tsibble(
  as_tibble(country_data[["India"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)
# reducing data
india <- india %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")
# splitting the data - 80% split
split_india <- ts_split(ts.obj = india, sample.out = 35)
india_train <- split_india$train
india_test <- split_india$test
# removing dates - making data univariate
india.ts_train <- as.ts(india_train$new_cases)
fit <- Arima(india.ts_train, order=c(1,1,1), method = "CSS")
preds <- predict(fit, n.ahead = 35)$pred
india_preds <- bind_cols(india_test, preds) %>% 
  rename("preds" = "...3")
ggplot(india_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

###################################### Examining Peru
peru <- as_tsibble(
  as_tibble(country_data[["Peru"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)
# reducing data
peru <- peru %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")
# splitting the data - 80% split
split_peru <- ts_split(ts.obj = peru, sample.out = 35)
peru_train <- split_peru$train
peru_test <- split_peru$test
# removing dates - making data univariate
peru.ts_train <- as.ts(india_train$new_cases)
fit <- Arima(peru.ts_train, order=c(1,1,1), method = "CSS")
preds <- predict(fit, n.ahead = 35)$pred
peru_preds <- bind_cols(peru_test, preds) %>% 
  rename("preds" = "...3")
ggplot(peru_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")

###################################### Examining Malaysia
malaysia <- as_tsibble(
  as_tibble(country_data[["Malaysia"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)
# reducing data
malaysia <- malaysia %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")
# splitting the data - 80% split
split_malaysia <- ts_split(ts.obj = malaysia, sample.out = 35)
malaysia_train <- split_malaysia$train
malaysia_test <- split_malaysia$test
# removing dates - making data univariate
malaysia.ts_train <- as.ts(malaysia_train$new_cases)
fit <- Arima(malaysia.ts_train, order=c(1,1,1))
preds <- predict(fit, n.ahead = 35)$pred
malaysia_preds <- bind_cols(malaysia_test, preds) %>% 
  rename("preds" = "...3")
ggplot(malaysia_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red")





