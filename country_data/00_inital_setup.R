library(tidyverse)
library(skimr)

load("data/covid_clean.rda")

na_counts <- covid_clean %>%
  group_by(continent, location) %>%
  summarize(na_count = mean(!is.na(across(where(is.numeric))))) %>% 
  arrange(-na_count) %>% 
  filter(na_count >= 0.5)

# Italy, Canada, India, Germany, Australia, Japan, Ireland, Denmark, Brazil, Sudan

our_countries <- c("Italy", "Canada", "Mexico", "India", "Germany", "Australia",
                   "Japan", "Ireland", "Denmark", "Brazil", "Egypt")

for (i in our_countries) {
  df <- covid_clean %>% 
    filter(
      location == i
    )
  
  df <- df %>% 
    select(date, new_cases)
  
  write_csv(df, file = paste0("country_data/univariate/", i, "_uni.csv"))
}

covid_clean %>% 
  filter(
    location == "Zimbabwe"
  ) %>% 
  view()
