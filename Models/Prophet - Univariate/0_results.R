
# Prophet - Univariate Results --------------------------------------------

library(tidyverse)

metric_files <- list.files("Models/Prophet - Univariate/results/", pattern = "_metrics.rda", full.names = TRUE)

for(i in metric_files){
  load(i)
}


prophet_uni_results <- bind_rows(Argentina_Prophet_uni,
                           Australia_Prophet_uni,
                           India_Prophet_uni,
                           Italy_Prophet_uni,
                           Malaysia_Prophet_uni,
                           Mexico_Prophet_uni,
                           Morocco_Prophet_uni,
                           Peru_Prophet_uni,
                           Sweden_Prophet_uni,
                           UK_Prophet_uni) %>% 
  arrange(rmse)

prophet_uni_results

write_csv(prophet_uni_results, file = "Models/Prophet - Univariate/results/prophet_uni_results_donald.csv")
