library(tidyverse)

data <- read.csv("data/owid-covid-data.csv") %>% 
  # get rid of iso_codes that start w "OWID"
  filter(substr(iso_code, start = 1, stop = 4) != "OWID")

US_data <- data %>% 
  filter(location == "United States")

data %>% 
  group_by(iso_code) %>% 
  select(iso_code) %>% 
  summarize(n_distinct(iso_code)) %>% 
  print(n=255)


# getting rid of cumulative mortality columns
data <- read.csv("data/covid_clean.csv")

data <- data %>% 
  select(-c(excess_mortality_cumulative_absolute, excess_mortality_cumulative))

# writing new csv
write_csv(data, "data/covid_cleaner.csv")

covid_cleaner <- read.csv("data/covid_cleaner.csv") 


# trial and error for good univariate countries:
covid_cleaner %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  select(date, iso_code, continent, location, new_cases) %>% 
  filter(location == "Peru", date >= "2023-10-01")
# our countries should be: c("Italy", "Mexico", "India", "Australia", "Argentina", 
# "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

