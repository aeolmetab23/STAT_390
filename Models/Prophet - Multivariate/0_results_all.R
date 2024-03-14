library(tidyverse)

metric_files <- list.files("Models/Prophet - Multivariate/results_linear/",
                           pattern = "_linear_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}

prophet_multi_linear_results_donald <- bind_rows(Argentina_Prophet_multi_linear,
                                                 Australia_Prophet_multi_linear,
                                                 India_Prophet_multi_linear,
                                                 Italy_Prophet_multi_linear,
                                                 Malaysia_Prophet_multi_linear,
                                                 Mexico_Prophet_multi_linear,
                                                 Morocco_Prophet_multi_linear,
                                                 Peru_Prophet_multi_linear,
                                                 Sweden_Prophet_multi_linear,
                                                 UK_Prophet_multi_linear) %>% 
  mutate(
    type = "linear"
  ) %>% 
  arrange(rmse)

library(tidyverse)

metric_files <- list.files("Models/Prophet - Multivariate/results/", pattern = "_flat_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}

prophet_multi_flat_results_donald <- bind_rows(Argentina_Prophet_multi_flat,
                                               Australia_Prophet_multi_flat,
                                               India_Prophet_multi_flat,
                                               Italy_Prophet_multi_flat,
                                               Malaysia_Prophet_multi_flat,
                                               Mexico_Prophet_multi_flat,
                                               Morocco_Prophet_multi_flat,
                                               Peru_Prophet_multi_flat,
                                               Sweden_Prophet_multi_flat,
                                               UK_Prophet_multi_flat) %>% 
  mutate(
    type = "flat"
  ) %>% 
  arrange(rmse)

prophet_mutli <- rbind(prophet_multi_flat_results_donald, prophet_multi_linear_results_donald)

prophet_mutli %>% 
  arrange(location, rmse)
