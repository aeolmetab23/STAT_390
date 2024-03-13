library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)
library(caret)
library(TSPred)
library(yardstick)
library(MLmetrics)
library(forecastHybrid)
library(scales)

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

# reducing data
italy <- italy %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")

# splitting the data - 80% split
split_italy <- ts_split(ts.obj = italy, sample.out = 35)

italy_train <- split_italy$train
italy_test <- split_italy$test

# removing dates - making data univariate
italy.ts_train <- as.ts(italy_train$new_cases)

italy_cv <- cvts(italy.ts_train, FUN = auto.arima,
                 windowSize = 15, maxHorizon = 5)

italy_cv_models <- italy_cv$models
resid <- italy_cv$residuals

lowest_id <- as_tibble(resid) %>% 
  rowid_to_column() %>% 
  mutate(average_resid = V1+V2+V3+V4+V5,
         average_resid = abs(average_resid)) %>% 
  arrange(average_resid) %>% 
  slice_head(n = 1) %>% 
  select(rowid) %>% 
  pull

italy_cv_best <- italy_cv_models[[lowest_id]]

italy_cv_preds <- predict(italy_cv_best, n.ahead = 35)$pred

italy_autoA_preds <- bind_cols(italy_test, italy_cv_preds) %>% 
  rename("preds" = "...3") %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

# Mean Absolute Scaled Error
mase_vec(truth = italy_autoA_preds$new_cases, estimate = italy_autoA_preds$preds)

################## UK
uk <- as_tsibble(
  as_tibble(country_data[["United Kingdom"]]) %>% 
    mutate(date = lubridate::ymd(date)),
  index = date
)

# reducing data
uk <- uk %>% 
  filter(date >= "2020-03-01", date < "2023-07-02")

# splitting the data - 80% split
split_uk <- ts_split(ts.obj = uk, sample.out = 35)

uk_train <- split_uk$train
uk_test <- split_uk$test

# removing dates - making data univariate
uk.ts_train <- as.ts(uk_train$new_cases)

uk_cv <- cvts(uk.ts_train, FUN = auto.arima,
                 windowSize = 15, maxHorizon = 5)

uk_cv_models <- uk_cv$models
uk_resid <- uk_cv$residuals

as_tibble(uk_resid) %>% 
  rowid_to_column() %>% 
  mutate(average_resid = V1+V2+V3+V4+V5,
         average_resid = abs(average_resid)) %>% 
  arrange(average_resid) %>% 
  slice_head(n = 1)

uk_cv_best <- uk_cv_models[[10]]

# coefficients for aarima model
uk_cv_best$arma

uk_cv_preds <- predict(uk_cv_best, n.ahead = 35)$pred

uk_autoA_preds <- bind_cols(uk_test, uk_cv_preds) %>% 
  rename("preds" = "...3") %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

# Mean Absolute Scaled Error
mase_vec(truth = uk_autoA_preds$new_cases, estimate = uk_autoA_preds$preds)


################## Peru
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
peru.ts_train <- as.ts(peru_train$new_cases)

peru_cv <- cvts(peru.ts_train, FUN = auto.arima,
              windowSize = 15, maxHorizon = 5)

peru_cv_models <- peru_cv$models
peru_resid <- peru_cv$residuals

as_tibble(peru_resid) %>% 
  rowid_to_column() %>% 
  mutate(average_resid = V1+V2+V3+V4+V5,
         average_resid = abs(average_resid)) %>% 
  arrange(average_resid) %>% 
  slice_head(n = 1)

peru_cv_best <- peru_cv_models[[6]]

# coefficients for aarima model
peru_cv_best$arma

peru_cv_preds <- predict(peru_cv_best, n.ahead = 35)$pred

peru_autoA_preds <- bind_cols(peru_test, peru_cv_preds) %>% 
  rename("preds" = "...3") %>% 
  mutate(preds = ifelse(preds < 0, 0, preds))

# Mean Absolute Scaled Error
mase_vec(truth = peru_autoA_preds$new_cases, estimate = peru_autoA_preds$preds)

# plot
colors <- c("new_cases" = "blue", "preds" = "red")
ggplot(peru_autoA_preds, aes(x = date)) +
  geom_line(aes(y = new_cases, color = "new_cases")) +
  geom_line(aes(y = preds, color = "preds")) +
  labs(y = "New Cases", x = NULL, title = "Peru Auto-ARIMA Predictions & Actuals") +
  scale_x_date(labels = date_format("%m-%Y")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(labels = c("New Cases", "Predictions"), values = colors) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.72,.91),
        panel.grid.major = element_blank())
