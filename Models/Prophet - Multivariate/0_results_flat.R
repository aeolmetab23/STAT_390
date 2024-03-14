# Prophet - Multivariate Flat Results --------------------------------------------

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
  arrange(rmse)

prophet_multi_flat_results_donald

write_csv(prophet_multi_flat_results_donald,
          file = "Models/Prophet - Multivariate/results/prophet_multi_flat_results_donald.csv")
