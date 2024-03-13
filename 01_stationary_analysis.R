library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)
library(urca)
library(tseries)

covid_clean <- read_csv("data/covid_cleaner.csv")

###################################### running the ADF for each of our ten countries
# loading datasets
our_countries <- c("Italy", "Mexico", "India", "Australia", "Argentina", 
                   "United Kingdom", "Malaysia", "Morocco", "Sweden", "Peru")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("data/my_country_data/", i, "_uni.csv"))
}

italy <- as_tibble(country_data[["Italy"]])

covid <- covid_clean %>% 
  filter(iso_code == "ITA")

#setting up ADF summary tibble
ADF_summary <- tibble(country = "", test_stat = c(), p1 = c(), p5 = c(), p10 = c())
# running ADF for each
for (i in seq_along(country_data)) {
  #save country data as tibble
  dataa <- as_tibble(country_data[[i]])
  #perform the adf test
  results <- ur.df(dataa$new_cases, type = "none", lags = 14)
  ###### start building row to add
  #get critical value
  test_stat <- as.list(results@teststat)[[1]]
  #p values
  p1 <- as.list(results@cval)[[1]]
  p2 <- as.list(results@cval)[[2]]
  p3 <- as.list(results@cval)[[3]]
  # add row to adf summary
  ADF_summary <- ADF_summary %>% 
    bind_rows(tibble(country = names(country_data)[i], test_stat = round(test_stat,2), p1 = p1, p5 = p2, p10 = p3))
}

# get rid of nonsense first row
ADF_summary <- ADF_summary %>% 
  filter(p1<0)


####################################### ADF round 2
# taking differences example
as_tibble(country_data[["Italy"]]) %>% 
  mutate(new_cases = c(0, diff(new_cases)))


#setting up ADF summary tibble
ADF_summary2 <- tibble(country = "", test_stat = 0, p1 = 0, p5 = 0, p10 = 0)
# running ADF for each
for (i in seq_along(country_data)) {
  #save country data as tibble
  dataa <- as_tibble(country_data[[i]]) %>% 
    mutate(new_cases = c(0, diff(new_cases)))
  #perform the adf test
  results <- ur.df(dataa$new_cases, type = "none", lags = 14)
  ###### start building row to add
  #get critical value
  test_stat <- as.list(results@teststat)[[1]]
  #p values
  p1 <- as.list(results@cval)[[1]]
  p2 <- as.list(results@cval)[[2]]
  p3 <- as.list(results@cval)[[3]]
  # add row to adf summary
  ADF_summary2 <- ADF_summary2 %>% 
    bind_rows(tibble(country = names(country_data)[i], test_stat = round(test_stat,2), p1 = p1, p5 = p2, p10 = p3))
}

# get rid of nonsense first row
ADF_summary2 <- ADF_summary2 %>% 
  filter(p1<0)

# all countries pass the ADF test after the first round of differencing