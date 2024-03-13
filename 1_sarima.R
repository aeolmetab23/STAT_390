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

italy <- as_tsibble(
  as_tibble(country_data[["Italy"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)

# splitting the data - 80% split
split_italy <- ts_split(ts.obj = italy, sample.out = 42)
italy_train <- split_italy$train
italy_test <- split_italy$test

# removing dates - making data univariate
italy.ts_train <- as.ts(italy_train$new_cases)

italy.ts_train %>% 
  ggtsdisplay(main = "")

################### grid search
ps <- seq(0:3)
qs <- seq(0:3)
Ps <- seq(0:3)
Qs <- seq(0:3)
results <- tibble(p = c(), q = c(), P = c(), Q = c(), aic = c())
for (p in ps) {
  for (q in qs) {
    for(P in Ps) {
      for(Q in Qs) {
        fit <- Arima(italy.ts_train, 
                     order = c(p,1,q), 
                     seasonal = list(order=c(P,1,Q), period=12),
                     method = "ML")
        aic <- fit$aic
        results <- bind_rows(results, tibble(p = p, q = q, P = P, Q = Q, aic = aic)) %>% 
          arrange(aic)
      }
    }
  }
}

# running model with best result from grid search (p=1, q=2, P=2, Q=1)
fit <- Arima(italy.ts_train, 
                    order = c(1,1,2), 
                    seasonal = list(order=c(2,1,1), period=12),
                    method = "ML")
# making predictions
fit %>% forecast() %>% autoplot()
preds <- predict(fit, n.ahead = 42)$pred
italy_preds <- bind_cols(italy_test, preds) %>% 
  rename("preds" = "...3") %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

ggplot(italy_preds) +
  geom_line(aes(x = date, y = new_cases), color = "blue") +
  geom_line(aes(x = date, y = preds), color = "red") +
  labs(y = "New Cases", x = NULL, title = "New COVID-19 Cases in Italy") +
  scale_x_date(labels = date_format("%m-%Y")) +
  theme_minimal()

# error metrics
MAPE(y_pred = italy_preds$new_cases, y_true = italy_preds$preds)
mase_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)
mae_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)
rmse_vec(truth = italy_preds$new_cases, estimate = italy_preds$preds)


####################### For Morocco
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

morocco.ts_train %>% 
  ggtsdisplay(main = "")

################### grid search
ps <- seq(0:3)
qs <- seq(0:3)
Ps <- seq(0:3)
Qs <- seq(0:3)
results <- tibble(p = c(), q = c(), P = c(), Q = c(), aic = c())
for (p in ps) {
  for (q in qs) {
    for(P in Ps) {
      for(Q in Qs) {
        fit <- Arima(morocco.ts_train, 
                     order = c(p,1,q), 
                     seasonal = list(order=c(P,1,Q), period=4),
                     method = "CSS")
        aic <- fit$aic
        results <- bind_rows(results, tibble(p = p, q = q, P = P, Q = Q, aic = aic)) %>% 
          arrange(aic)
      }
    }
  }
}

# running model with best result from grid search (p=1, q=3, P=1, Q=1)
fit <- Arima(morocco.ts_train, 
             order = c(1,1,1), 
             seasonal = list(order=c(1,1,1), period=12),
             method = "CSS")
# making predictions
fit %>% forecast() %>% autoplot()
preds <- predict(fit, n.ahead = 35)$pred
morocco_preds <- bind_cols(morocco_test, preds) %>% 
  rename("preds" = "...3") %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

colors <- c("new_cases" = "blue", "preds" = "red")
ggplot(morocco_preds, aes(x = date)) +
  geom_line(aes(y = new_cases, color = "new_cases")) +
  geom_line(aes(y = preds, color = "preds")) +
  labs(y = "New Cases", x = NULL, title = "Morocco SARIMA Predictions & Actuals") +
  scale_x_date(labels = date_format("%m-%Y")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(labels = c("New Cases", "Predictions"), values = colors) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.72,.91),
        panel.grid.major = element_blank())

# error metrics
mase_vec(truth = morocco_preds$new_cases, estimate = morocco_preds$preds)
mae_vec(truth = morocco_preds$new_cases, estimate = morocco_preds$preds)
rmse_vec(truth = morocco_preds$new_cases, estimate = morocco_preds$preds)
