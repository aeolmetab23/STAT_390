library(tidyverse)
library(skimr)

load("data/covid_clean.rda")

# don't have this rda on my local so added csv read - Alex
covid_clean <- read_csv(here::here("data/covid_cleaner.csv"))

na_counts <- covid_clean %>%
  group_by(continent, location) %>%
  summarize(na_count = mean(!is.na(across(where(is.numeric))))) %>% 
  arrange(-na_count) %>% 
  filter(na_count >= 0.5)

# Italy, Canada, India, Germany, Australia, Japan, Ireland, Denmark, Brazil, Sudan

our_countries <- c("Italy", "Canada", "Mexico", "India", "Germany", "Australia",
                   "Japan", "Ireland", "Denmark", "Brazil", "Egypt")

our_countries_v2 <- c("Italy", "Mexico", "India", "Australia", "Argentina",
                      "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

for (i in our_countries_v2) {
  df <- covid_clean %>% 
    filter(
      location == i
    )
  
  df <- df %>% 
    select(date, new_cases)
  
  write_csv(df, file = paste0("Models/model_data/uni_v2/", i, "_uni.csv"))
}

covid_clean_v2 <- covid_clean %>% 
  group_by(location) %>% 
  mutate(
    new_case_diff = new_cases - new_cases_1week_lag
  ) %>% 
  mutate(
    new_case_diff = ifelse(is.na(new_case_diff), 0, new_case_diff)
  ) %>% 
  ungroup()

for (i in our_countries_v2) {
  df <- covid_clean_v2 %>% 
    filter(
      location == i
    )
  
  df <- df %>% 
    select(date, new_case_diff)
  
  write_csv(df, file = paste0("Models/model_data/uni_diffs_v2/", i, "_uni.csv"))
}

