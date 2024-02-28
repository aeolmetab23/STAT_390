
# Auto Arima Results -----------------------------------------------------------

library(tidyverse)

metric_files <- list.files("Models/Auto Arima/results/", pattern = "_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}

location_results <- bind_rows(argentina_metrics,
                              australia_metrics,
                              india_metrics,
                              italy_metrics,
                              Malaysia_metrics,
                              mexico_metrics,
                              Morocco_metrics,
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

auto_arima_metrics <- location_metrics %>% 
  mutate(
    model = "Auto Arima"
  ) %>% 
  arrange(rmse)

auto_arima_metrics


