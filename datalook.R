library(tidyverse)

data <- read.csv("owid-covid-data.csv") %>% 
  # get rid of iso_codes that start w "OWID"
  filter(substr(iso_code, start = 1, stop = 4) != "OWID")

US_data <- data %>% 
  filter(location == "United States")

data %>% 
  group_by(iso_code) %>% 
  select(iso_code) %>% 
  summarize(n_distinct(iso_code)) %>% 
  print(n=255)