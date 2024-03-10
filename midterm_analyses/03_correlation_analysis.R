library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)
library(zoo)

load(file = "data/covid_clean.rda")

covid_sum <- covid_clean %>% 
  group_by(date) %>% 
  select(new_cases) %>% 
  summarise(
    num_cases = sum(new_cases, na.rm = T)
  )

ts_data <- ts(covid_sum, frequency = 52)

# Autocorrelation plot
acf(coredata(ts_data))

# Partial autocorrelation plot
pacf(coredata(ts_data))
