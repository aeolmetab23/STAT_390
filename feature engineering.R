library(tidyverse)
library(tidymodels)
library(lubridate)
library(tibbletime)

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

#na's per column
nas <- tibble(variable = names(colMeans(is.na(fdata))), pct_missing = colMeans(is.na(fdata)))

nas %>% 
  arrange(-pct_missing) %>% 
  print(n=70)


# creating lag variables, rolling windows, and date time features
# first need rolling functions
mean_roll_6 <- rollify(mean, window = 6)
mean_roll_12 <- rollify(mean, window = 12)

gdata <- fdata %>% 
  group_by(iso_code) %>% 
  #lags
  mutate(new_cases_2week_lag = lag(new_cases, n=2, default=NA),
         new_cases_6week_lag = lag(new_cases, n=6, default=NA),
         new_deaths_2week_lag = lag(new_deaths, n=2, default=NA),
         new_deaths_6week_lag = lag(new_deaths, n=6, default=NA)) %>% 
  relocate(new_cases_2week_lag,.after = new_cases) %>% 
  relocate(new_cases_6week_lag,.after = new_cases_2week_lag) %>% 
  relocate(new_deaths_2week_lag,.after = new_deaths) %>% 
  relocate(new_deaths_6week_lag,.after = new_deaths_2week_lag) %>% 
  # rolling statistics
  mutate(new_cases_6week_roll = mean_roll_6(new_cases),
         new_cases_12week_roll = mean_roll_12(new_cases),
         new_deaths_6week_roll = mean_roll_6(new_deaths),
         new_deaths_12week_roll = mean_roll_12(new_deaths)) %>% 
  relocate(new_cases_6week_roll,.after = new_cases_6week_lag) %>% 
  relocate(new_cases_12week_roll,.after = new_cases_6week_roll) %>% 
  relocate(new_deaths_6week_roll,.after = new_deaths_6week_lag) %>% 
  relocate(new_deaths_12week_roll,.after = new_deaths_6week_roll) %>% 
  # date time features
  mutate(month = lubridate::month(date),
         week = lubridate::week(date))

write_csv(gdata, file = "data/covid_clean_lags.csv")



