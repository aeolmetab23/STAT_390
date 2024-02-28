
# Arima Results -----------------------------------------------------------

library(tidyverse)

metric_files <- list.files("Models/Arima/results/", pattern = "_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}

location_results <- bind_rows(Argentina_metrics,
                         Australia_metrics,
                         india_metrics,
                         italy_metrics,
                         malaysia_metrics,
                         mexico_metrics,
                         morocco_metrics,
                         peru_metrics,
                         sweden_metrics,
                         uk_metrics) %>% 
  mutate(location = c("Argentina","Argentina","Argentina",
                      "Australia","Australia","Australia",
                      "India","India","India",
                      "Italy","Italy","Italy",
                      "Malaysia","Malaysia","Malaysia",
                      "Mexico","Mexico","Mexico",
                      "Morocco","Morocco","Morocco",
                      "Peru","Peru","Peru",
                      "Sweden","Sweden","Sweden",
                      "United Kingdom","United Kingdom","United Kingdom"))

location_metrics <- pivot_wider(location_results, names_from = ".metric", values_from = ".estimate")

location_metrics <- location_metrics %>% 
  mutate(
    model = "Arima"
  )


