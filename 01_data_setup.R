
# Data Setup --------------------------------------------------------------

# Load Packages
library(tidyverse)
library(skimr)
library(mice)
library(caret)

# Read Original File
cv <- read_csv("data/owid-covid-data.csv") %>% 
  janitor::clean_names()

# Mutating Data
covid <- cv %>% 
  filter(
    !is.na(continent) # Some rows were demographics, like "Upper Middle Class", or all of one continent
    , str_length(iso_code) < 8
  )

# Remove Correlation Variables
covid_no_corr <- covid %>% 
  select(
    !matches(c("_per_million", "_per_thousand", "_per_hundred", "weekly_"))
  )

# Clean Dataset
covid_clean <- covid_no_corr %>% 
  filter(!is.na(new_cases)) %>% 
  mutate(
    total_cases = case_when(
      new_cases == 0 & is.na(total_cases) ~ 0,
      TRUE ~ total_cases
    ),
    total_deaths = case_when(
      new_deaths == 0 & is.na(total_deaths) ~ 0,
      TRUE ~ total_deaths
    ),
    new_cases_smoothed = case_when(
      new_cases == 0 & total_cases == 0 ~ 0.000,
      TRUE ~ new_cases_smoothed
    ),
    new_deaths_smoothed = case_when(
      new_deaths == 0 & total_deaths == 0 ~ 0.000,
      TRUE ~ new_deaths_smoothed
    ),
    new_deaths = case_when(
      is.na(new_deaths) ~ 0,
      TRUE ~ new_deaths
    )) %>% 
  select(-c(total_cases, total_deaths, total_tests, total_vaccinations, total_boosters)) %>% 
  select(!matches(c("_smoothed")))

# Save Dataset
save(covid_clean, file = "data/covid_clean.rda")

# Write CSV
write_csv(covid_clean, file = "covid_clean.csv")



