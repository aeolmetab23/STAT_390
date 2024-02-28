
# Visuals -----------------------------------------------------------------

library(tidyverse)

load("Models/XGBoost/results/preds_5.rda")

colors <- c("Predicted" = "navyblue", "Actual" = "indianred")

# Italy ----
covid_pred_5 %>% 
  filter(location == "Italy") %>% 
  ggplot() +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 2) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Italy Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)
