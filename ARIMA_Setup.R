library(tidyverse)
library(forecast)
library(TSstudio)
library(tsibble)
library(fpp3)

######################### DATA LOADING
our_countries <- c("Italy", "Mexico", "India", "Germany", "Australia",
                   "Japan", "Ireland", "Denmark", "Brazil", "Egypt")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data_firstdiffs/", i, "_uni.csv"))
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

# plotting ACF and PACF
italy_train %>% 
  feasts::gg_tsdisplay(y = new_cases, plot_type = "partial")


# removing dates - making data univariate
italy.ts_train <- as.ts(italy$new_cases)

italy.ts_train %>% 
  ggtsdisplay(main = "")

fit <- Arima(italy.ts_train, order=c(1,1,0))
checkresiduals(fit)





