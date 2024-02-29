
# Arima Results -----------------------------------------------------------

library(tidyverse)

metric_files <- list.files("Models/Arima/results/", pattern = "_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}

arima_results <- bind_rows(Argentina_Arima,
                         Australia_Arima,
                         India_Arima,
                         Italy_Arima,
                         Malaysia_Arima,
                         Mexico_Arima,
                         Morocco_Arima,
                         Peru_Arima,
                         Sweden_Arima,
                         UK_Arima)

write_csv(arima_results, file = "Models/Arima/results/arima_results_donald.csv")


