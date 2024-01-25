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

# Third Data Frame Update ----
covid_complete <- covid %>% 
  select(
    !matches(c('excess_mortality')))

skim_without_charts(covid_complete)

# Only Rows with new_cases reported

covid_clean <- covid_complete %>% 
  filter(
    !is.na(new_cases)
  )

skim_without_charts(covid_clean)

save(covid_clean, file = "data/covid_clean.rda")

covid_complete %>% 
  filter(
    location == "United States"
  ) %>% 
  count()



