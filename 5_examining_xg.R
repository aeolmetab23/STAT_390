library(tidyverse)
library(xgboost)
library(Ckmeans.1d.dp)

load("results/xgb_tuned.rda")

df_simple <- df_withpreds %>% 
  select(date, location, new_cases, Preds)

xgb.ggplot.importance(xgb_importance, rel_to_first = TRUE)

ggplot(df_simple, aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red")

# for a given country
ggplot(df_simple %>% filter(location == "Mexico", date > "2022-11-06"), aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal()


df_stats <- df_simple %>% 
  filter(date > "2022-11-06") %>% 
  filter(location == "Sweden")

# test statistics
MAPE(y_pred = df_stats$new_cases, y_true = df_stats$Preds)
mase_vec(truth = df_stats$new_cases, estimate = df_stats$Preds)



