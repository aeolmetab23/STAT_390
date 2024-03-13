library(tidyverse)
library(skimr)

load("data/covid_clean.rda")

# don't have this rda on my local so added csv read - Alex
covid_clean <- read_csv("covid_cleaner.csv")

na_counts <- covid_clean %>%
  group_by(continent, location) %>%
  summarize(na_count = mean(!is.na(across(where(is.numeric))))) %>% 
  arrange(-na_count) %>% 
  filter(na_count >= 0.5)

# Italy, Canada, India, Germany, Australia, Japan, Ireland, Denmark, Brazil, Sudan

our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

for (i in our_countries) {
  df <- covid_clean %>% 
    filter(
      location == i
    )
  
  df <- df %>% 
    select(date, new_cases)
  
  write_csv(df, file = paste0("country_data/univariate/", i, "_uni.csv"))
}

library(tseries)
aus <- read_csv("country_data/univariate/Australia_uni.csv")
View(aus)

aus_log <- aus %>% 
  mutate(
    log_cases = log(new_cases + 1),
    case_diff = new_cases - lag(new_cases,default = 1)
  )
library(zoo)
adf.test(aus_log$case_diff)
acf(coredata(aus_log$case_diff), plot = T)

covid_cleaner <- read_csv("covid_cleaner.csv")

covid_cln <- covid_cleaner %>% 
  mutate(month = lubridate::month(date),
         week = lubridate::week(date)) %>% 
  mutate(
    month = factor(month)
  ) %>% 
  ungroup() %>% 
  arrange(location, date)

covid_cln %>% 
  ggplot(aes(month, new_cases)) +
  geom_col(fill = "indianred") +
  theme_minimal() +
  labs(
    y = "New Cases",
    x = "Month",
    title = "New Cases by Month",
    caption = "Data: covid_cleaner"
  ) +
  theme(
        panel.grid.major.x = element_blank()
  )
