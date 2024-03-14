# Prophet - Multivariate Linear Results --------------------------------------------

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
  arrange(rmse)

prophet_multi_linear_results_donald

write_csv(prophet_multi_linear_results_donald,
          file = "Models/Prophet - Multivariate/results_linear/prophet_multi_linear_results_donald.csv")
