library(tidyverse)
library(scales)

data <- read.csv("data/covid_cleaner.csv")

mdata <- read.csv("data/covid_clean_lags.csv")

ggplot(mdata, aes(x = month, y = new_cases)) +
  geom_col()

ggplot(mdata, aes(x = week, y = new_cases)) + 
  geom_col(fill="purple") +
  ggtitle("New Cases by Week") +
  labs(x = "Week", y = "New Cases") +
  theme_minimal()

#china data
bigcountries <- mdata %>% 
  filter(iso_code %in% c("CHN", "RUS", "USA", "IND", "BRA")) %>% 
  mutate(datee = lubridate::ymd(date)) %>%
  select(-date) %>% 
  mutate(date = datee) %>% 
  select(-datee) %>% 
  relocate(date)

ggplot(bigcountries, aes(x = date, y = new_cases, group = iso_code, color = location)) +
  geom_line() +
  scale_x_date(date_breaks = "years", date_labels = "20%y") +
  theme_minimal()
  
