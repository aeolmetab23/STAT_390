library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)
library(urca)
library(tseries)

load(file = "data/covid_clean.rda")

# don't have this rda on my local so added csv read - Alex
covid_clean <- read_csv("covid_clean_lags.csv")

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

# alternate method for Augmented Dickey-Fuller test. Was slightly confused by
# the summary results above
adf.test(covid_clean$new_cases)






