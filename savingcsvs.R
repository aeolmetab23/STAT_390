library(tidyverse)

write_csv(arima_results, "results/alexparima.csv")
write_csv(auto_arima_results, "results/alexpaarima.csv")
write_csv(prophet_results, "results/alexpprophet.csv")
write_csv(prophet_mp_results, "results/alexpmprophet.csv")
