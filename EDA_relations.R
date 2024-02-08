library(tidyverse)

data <- read.csv("data/covid_cleaner.csv")

mdata <- read.csv("data/covid_clean_lags.csv")

ggplot(mdata, aes(x = month, y = new_cases)) +
  geom_col()
