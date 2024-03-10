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
    new_cases = sum(new_cases, na.rm = T)
  )

ts_data <- ts(covid_sum, frequency = 52)



# Histogram
ggplot(covid_sum, aes(new_cases)) +
  geom_histogram(bins = 100)

ggplot(covid_sum, aes(new_cases)) +
 geom_density()

# Box Plot
boxplot(ts_data, main = "Box Plot")
