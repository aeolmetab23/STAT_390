library(tidyverse)
library(tidymodels)
library(lubridate)

data <- read.csv("data/covid_clean.csv")

data %>% 
  group_by(iso_code) %>% 
  select(iso_code) %>% 
  summarize(n_distinct(iso_code)) %>% 
  print(n=255)

US_data <- data %>% 
  filter(iso_code == "USA")

# country's with the highest number of observations
data %>% 
  count(iso_code) %>% 
  arrange(-n)


##################### feature engineering
fdata <- data %>% 
  mutate(datee = ymd(date)) %>%
  select(-date) %>% 
  mutate(date = datee) %>% 
  select(-datee) %>% 
  relocate(date)

# class check
class(fdata$date)



