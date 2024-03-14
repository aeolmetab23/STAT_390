
# Visuals -----------------------------------------------------------------

library(tidyverse)

load("Models/XGBoost/results/preds_5.rda")

colors <- c("Predicted" = "skyblue3", "Actual" = "indianred")

# Argentina ----
covid_preds %>% 
  filter(location == "Argentina") %>% 
  ggplot() +
  # geom_point(mapping = aes(date, .pred, color = "Predicted") ) +
  # geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Argentina",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "Argentina") %>% 
  ggplot() +
  # geom_point(mapping = aes(date, pred, color = "Predicted") ) +
  # geom_point(mapping = aes(date, new_cases, color = "Actual")) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Argentina",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

ggplot() +
  geom_line(covid_preds %>% filter(location == "Argentina"), mapping = aes(date, new_cases), color = "indianred") +
  geom_line(covid_pred_5 %>% filter(location == "Argentina"), mapping = aes(date, pred), color = "skyblue3") +
  theme_minimal()

# Australia ----
covid_preds %>% 
  filter(location == "Australia") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Australia",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "Australia") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Australia",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

# India ----
covid_preds %>% 
  filter(location == "India") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for India",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "India") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for India",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

# Italy ----
covid_pred_5 %>% 
  filter(location == "Italy") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted") ) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Italy Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Malaysia ----
covid_pred_5 %>% 
  filter(location == "Malaysia") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted") ) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Malaysia Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Mexico ----
covid_preds %>% 
  filter(location == "Mexico") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Mexico",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "Mexico") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Mexico",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

# Morocco ----
covid_preds %>% 
  filter(location == "Morocco") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Morocco",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "Morocco") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Morocco",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

# Peru ----
covid_preds %>% 
  filter(location == "Peru") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Peru",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "Peru") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Peru",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

# Sweden ----
covid_preds %>% 
  filter(location == "Sweden") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Sweden",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "Sweden") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for Sweden",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)

# United Kingdom ----
covid_preds %>% 
  filter(location == "United Kingdom") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for United Kingdom",
       subtitle = "From 3/8/20 to 12/31/23") +
  scale_color_manual(values = colors)

covid_pred_5 %>% 
  filter(location == "United Kingdom") %>% 
  ggplot() +
  geom_line(mapping = aes(date, new_cases, color = "Actual"), linewidth = .8) +
  geom_line(mapping = aes(date, pred, color = "Predicted"), linewidth = .8) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases",
       color = "",
       title = "New Covid Cases for United Kingdom",
       subtitle = "From 4/2/23  to 12/31/23") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors)
