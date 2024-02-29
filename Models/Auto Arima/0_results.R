
# Auto Arima Results -----------------------------------------------------------

library(tidyverse)

metric_files <- list.files("Models/Auto Arima/results/", pattern = "_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}

auto_arima_results <- bind_rows(argentina_Auto_Arima,
                              australia_Auto_Arima,
                              india_Auto_Arima,
                              italy_Auto_Arima,
                              Malaysia_Auto_Arima,
                              Mexico_Auto_Arima,
                              Morocco_Auto_Arima,
                              Peru_Auto_Arima,
                              Sweden_Auto_Arima,
                              UK_Auto_Arima) %>% 
  arrange(rmse)

auto_arima_results

write_csv(auto_arima_results, file = "Models/Auto Arima/results/auto_arima_results_donald.csv")


