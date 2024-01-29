library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)
library(urca)

load(file = "data/covid_clean.rda")

covid_sum <- covid_clean %>% 
  filter(
    date < "2024-01-01"
  ) %>% 
  group_by(date) %>% 
  select(date, new_cases) %>% 
  summarise(
    num_cases = sum(new_cases, na.rm = T)
  )

ts_data <- ts(covid_sum, frequency = 52)

variable <- ts_data[,2]

# Augmented Dickey-Fuller test
adf_test <- ur.df(variable, type = "trend", lags = 10)
summary(adf_test)
