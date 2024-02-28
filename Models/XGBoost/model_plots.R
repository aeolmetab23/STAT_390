
# Visuals -----------------------------------------------------------------

library(tidyverse)

load("Models/XGBoost/results/preds_5.rda")

colors <- c("Predicted" = "navyblue", "Actual" = "indianred")

# Argentina ----
covid_pred_5 %>% 
  filter(location == "Argentina") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Argentina Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Australia ----
covid_pred_5 %>% 
  filter(location == "Australia") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Australia Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# India ----
covid_pred_5 %>% 
  filter(location == "India") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "India Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Italy ----
covid_pred_5 %>% 
  filter(location == "Italy") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
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
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
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
covid_pred_5 %>% 
  filter(location == "Mexico") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Mexico Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Morocco ----
covid_pred_5 %>% 
  filter(location == "Morocco") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Morocco Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Peru ----
covid_pred_5 %>% 
  filter(location == "Peru") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Peru Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# Sweden ----
covid_pred_5 %>% 
  filter(location == "Sweden") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "Sweden Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)

# United Kingdom ----
covid_pred_5 %>% 
  filter(location == "United Kingdom") %>% 
  ggplot() +
  geom_point(mapping = aes(date, .pred, color = "Predicted"), shape = 10) +
  geom_point(mapping = aes(date, new_cases_log, color = "Actual")) +
  geom_line(mapping = aes(date, .pred, color = "Predicted"), linewidth = .8, linetype = 3) +
  geom_line(mapping = aes(date, new_cases_log, color = "Actual"), linewidth = 1) +
  theme_minimal() +
  labs(x = "Date",
       y = "New Cases (log)",
       color = "",
       title = "United Kingdom Predictions",
       subtitle = "From April 2nd to December 31") +
  scale_color_manual(values = colors)
