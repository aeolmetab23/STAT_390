library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)

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
decomposition_result <- decompose(ts_data)

trend_component <- decomposition_result$trend
seasonal_component <- decomposition_result$seasonal
residual_component <- decomposition_result$residual

# Plot the components
plot(trend_component, main = "Trend Component")
plot(seasonal_component, main = "Seasonal Component")
plot(residual_component, main = "Residual Component")


