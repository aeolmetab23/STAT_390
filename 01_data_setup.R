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

covid_names <- covid %>% 
  select(c(1:3, 49:63))

col_names <- names(covid_names)
covid <- covid %>%
  mutate(across(all_of(col_names), as.factor))

covid_clean <- covid %>% 
  filter(
    !is.na(new_cases)
  )

skim_without_charts(covid_clean)

save(covid_clean, file = "data/covid_clean.rda")

write_csv(covid_clean, file = "covid_clean.csv")



