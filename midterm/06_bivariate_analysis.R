library(tidyverse)
library(tidymodels)
library(skimr)
library(forecast)
library(mice)
library(caret)
library(stats)

load(file = "data/covid_clean.rda")

## New Cases vs. New Deaths ----
ggplot(covid_clean, aes(new_cases, new_deaths)) +
  geom_point(alpha = 0.2) +
  coord_cartesian(xlim = c(0, 2000000), ylim = c(0, 30000)) +
  geom_smooth() +
  theme_minimal()

## New Cases vs. People Vaccinated ----
ggplot(covid_clean, aes(new_deaths, new_vaccinations)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 30000), ylim = c(0, 2000000)) +
  # geom_smooth() +
  theme_minimal()

## Aged 65 Older ----

covid_clean <- covid_clean %>% 
  mutate(
    age_65_percent_group = case_when(
      aged_65_older >= 0 & aged_65_older < 5 ~ "0-4.9",
      aged_65_older >= 5 & aged_65_older < 10 ~ "5-9.9",
      aged_65_older >= 10 & aged_65_older < 15 ~ "10-14.9",
      aged_65_older >= 15 & aged_65_older < 20 ~ "15-19.9",
      aged_65_older >= 20 & aged_65_older < 25 ~ "20-24.9",
      aged_65_older >= 25 ~ "≥ 25"
    ),
    age_65_percent_group = factor(age_65_percent_group, levels = c("0-4.9", "5-9.9", "10-14.9",
                                                                   "15-19.9", "20-24.9", "≥ 25"))
  )

### Total Cases
ggplot(covid_clean %>% filter(!is.na(age_65_percent_group)),
       aes(age_65_percent_group, total_cases)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Percent",
    y = "Total Cases",
    subtitle = "Total Number of Cases for Percentage of Country Population that is 65 and Older"
  )

### Total Deaths
ggplot(covid_clean %>% filter(!is.na(age_65_percent_group)),
       aes(age_65_percent_group, total_deaths)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Percent",
    y = "Total Deaths",
    subtitle = "Total Number of Deaths for Percentage of Country Population that is 65 and Older"
  )

### Total Vaccinations
ggplot(covid_clean %>% filter(!is.na(age_65_percent_group)),
       aes(age_65_percent_group, total_vaccinations)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Percent",
    y = "Total Vaccinations",
    title = "Total Number of Vaccinations for Percentage of Country Population that is 65 and Older"
  )


covid_65 <- covid_clean %>%
  filter(!is.na(age_65_percent_group),
         date < "2024-01-01") %>% 
  group_by(date, age_65_percent_group) %>% 
  select(
    date, age_65_percent_group, total_cases, total_deaths, new_cases, total_vaccinations
  ) %>% 
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE)
  ) %>% 
  filter(
    total_deaths > 0
  )

ggplot(covid_65,
       aes(date, total_cases, color = age_65_percent_group)) +
  geom_line() +
  theme_minimal()




 