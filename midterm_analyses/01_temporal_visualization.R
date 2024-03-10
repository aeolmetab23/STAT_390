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

ggplot(covid_sum, aes(x = date, y = num_cases)) +
  geom_line() +
  labs(title = "Time Plot", x = "Date", y = "Value") +
  theme_minimal()