library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)
library(urca)
library(tseries)

load(file = "data/covid_clean.rda")

# don't have this rda on my local so added csv read - Alex
covid_clean <- read_csv("covid_cleaner.csv")

covid_sum <- covid_clean %>% 
  filter(
    date < "2024-01-01"
  ) %>% 
  group_by(date) %>% 
  select(date, new_cases) %>% 
  summarise(
    num_cases = sum(new_cases, na.rm = T)
  )

ts_data <- ts(covid_sum, frequency = 52)

variable <- ts_data[,2]

# Augmented Dickey-Fuller test
adf_test <- ur.df(variable, type = "trend", lags = 10)
summary(adf_test)



# alternate method for Augmented Dickey-Fuller test. Was slightly confused by
# the summary results above
adf.test(covid_clean$new_cases)


results <- ur.df(covid_clean$new_cases, type = "none", lags = 14) #type none matches python default,lags calculated by python default of 12*(nobs/100)^{1/4}
summary(results)
# critical values
as.list(results@cval)[[1]]
# test statistic
test_stat <- as.list(results@teststat)[[1]]

ADF_summary %>% 
  bind_rows(tibble(country = "Italy", test_stat))

###################################### running the ADF for each of our ten countries
# loading datasets
our_countries <- c("Italy", "Mexico", "India", "Germany", "Australia",
                   "Japan", "Ireland", "Denmark", "Brazil", "Egypt")

country_data <- list()
for (i in our_countries) {
  country_data[[i]] <- read.csv(file = paste0("country_data/univariate/", i, "_uni.csv"))
}

as_tibble(country_data[["Italy"]])

#setting up ADF summary tibble
ADF_summary <- tibble(country = "", test_stat = 0, p1 = 0, p5 = 0, p10 = 0)
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
    bind_rows(tibble(country = names(country_data)[i], test_stat = test_stat, p1 = p1, p5 = p2, p10 = p3))
}

# get rid of nonsense first row
ADF_summary %>% 
  filter(p1<0)


