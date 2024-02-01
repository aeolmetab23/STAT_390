library(tidyverse)
library(skimr)

load("data/covid_clean.rda")

for (i in unique(covid_clean$location)) {
  df <- covid_clean %>% 
    filter(
      location == i
    )
  
  write.csv(df, file = paste0("country_data/", i, ".csv"))
}
