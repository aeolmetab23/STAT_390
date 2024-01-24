library(tidyverse)
library(skimr)

# Read Original File
cv <- read_csv("data/owid-covid-data.csv") %>% 
  janitor::clean_names()

# Mutating Data
covid <- cv %>% 
  filter(
    !is.na(continent) # Some rows were demographics, like "Upper Middle Class", or all of one continent
    , str_length(iso_code) < 8
  )

# Skim Data
skim_without_charts(covid)
## columns that begin with excess_mortality have a 0.0464 complete rate
## with weekly_icu/hosp also under 0.1
## Date Range: 01/01/20 - 01/17/24

# New Data Frame ----
covid_vac <- covid %>% 
  filter(
    !is.na(total_vaccinations) # Where there are vaccinations available
  )

# Skim Data
skim_without_charts(covid_vac)
## columns that begin with excess_mortality have a 0.0660 complete rate. WE SHOULD DELETE THESE COLUMNS
## weekly_icu ~ 0.0989
## weekly_hosp ~ 0.177
## Date Range: 12/02/20 - 01/17/24

# Third Data Frame Update ----
covid_complete <- covid_vac %>% 
  select(
    !matches('excess_mortality'))

skim_without_charts(covid_complete)

# USA Data ----
covid_USA <- covid_complete %>% 
  filter(
    location == "United States"
  )

skim_without_charts(covid_USA)

# Location Rows
covid_loc <- covid_complete %>% 
  group_by(location) %>% 
  summarise(
    total_rows = n()
  )

# Handwashing Data ----
covid_hands <- covid_complete %>% 
  filter(
    is.na(handwashing_facilities)
  ) %>% 
  group_by(location) %>% 
  summarise(
    num_NA_hands = n()
  )

covid_hands_2 <- covid_complete %>% 
  filter(
    !is.na(handwashing_facilities)
  ) %>% 
  group_by(location) %>% 
  summarise(
    num_hands = n()
  )

covid_washing <- left_join(covid_loc, covid_hands, by = c("location"))

covid_handwashing <- left_join(covid_washing, covid_hands_2, by = c("location"))

skim_without_charts(covid_handwashing)

# So they either have it or they don't, no in between. We should remove these columns

# Fourth Data Frame

corona <- covid_complete %>% 
  select(
    !c('handwashing_facilities'))

skim_without_charts(corona)

corona %>% 
  filter(
    !is.na(total_cases)
  )

## Starting Over, only data with total cases info

cv19 <- covid %>% 
  filter(
    !is.na(total_cases)
  )

cv19_complete <- cv19 %>% 
  select(
    !matches(c('excess_mortality', 'weekly_icu', 'weekly_hosp')))

skim_without_charts(cv19_complete)



