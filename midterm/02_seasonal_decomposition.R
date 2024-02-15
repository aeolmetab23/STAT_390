library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)
library(lubridate)

# Australia ----
Australia_uni <- read_csv("country_data/univariate/Australia_uni.csv")

aus_ts <- ts(Australia_uni$new_cases, 
               freq=365.25/7, 
               start=decimal_date(ymd("2020-01-05")))

aus_decomp <- decompose(aus_ts)
plot(aus_decomp) + title(outer = F, sub = "Australia")

# Ireland ----
Ireland_uni <- read_csv("country_data/univariate/Ireland_uni.csv")

ire_ts <- ts(Ireland_uni$new_cases, 
               freq=52, 
               start=decimal_date(ymd("2020-01-05")))

ire_decomp <- decompose(ire_ts)
plot(ire_decomp) + title(sub = "Ireland")


# Mexico ----
Mexico_uni <- read_csv("country_data/univariate/Mexico_uni.csv")

mex_ts <- ts(Mexico_uni$new_cases, 
             freq=52, 
             start=decimal_date(ymd("2020-01-05")))

mex_decomp <- decompose(mex_ts)
plot(mex_decomp) + title(sub = "Mexico")


