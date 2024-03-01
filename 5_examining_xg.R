library(tidyverse)
library(xgboost)
library(Ckmeans.1d.dp)
library(scales)

load("results/xgb_tuned.rda")

df_simple <- df_withpreds %>% 
  select(date, location, new_cases, Preds)

xgb.ggplot.importance(xgb_importance %>% top_n(Feature, n=10), rel_to_first = TRUE) +
  ggtitle("Feature Importance - Top 10 Variables") +
  theme_minimal()

xgb.ggplot.importance(head(xgb_importance, 10), rel_to_first = TRUE) +
  ggtitle("Feature Importance - Top 10 Variables") +
  theme_minimal()

ggplot(df_simple, aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "New Cases", title = "Actuals and Predictions for All Countries")

# for a given country
ggplot(df_simple %>% filter(location == "Italy"), aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "New Cases", title = "Actuals and Predictions for Italy")


df_stats <- df_simple %>% 
  filter(date > "2022-11-06") %>%
  filter(location == "Malaysia")

# test statistics
MAPE(y_pred = df_stats$new_cases, y_true = df_stats$Preds)
mase_vec(truth = df_stats$new_cases, estimate = df_stats$Preds)

rmse_vec(truth = df_stats$new_cases, estimate = df_stats$Preds)
mase_vec(truth = df_stats$new_cases, estimate = df_stats$Preds)
round(MAPE(y_pred = df_stats$new_cases, y_true = df_stats$Preds), 6)
mae_vec(truth = df_stats$new_cases, estimate = df_stats$Preds)


######################################### XGBoost2
load("results/xgb_tuned2.rda")

df_simple2 <- df_withpreds2 %>% 
  select(date, location, new_cases, Preds)

xgb.ggplot.importance(xgb_importance2 %>% top_n(Feature, n=10), rel_to_first = TRUE) +
  ggtitle("Feature Importance - Top 10 Variables") +
  theme_minimal()

xgb.ggplot.importance(head(xgb_importance2, 10), rel_to_first = TRUE) +
  ggtitle("Feature Importance - Top 10 Variables") +
  theme_minimal()

ggplot(df_simple2, aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "New Cases", title = "Actuals and Predictions for All Countries")

# for a given country
ggplot(df_simple2 %>% filter(location == "Italy"), aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "New Cases", title = "Actuals and Predictions for Italy")


df_stats2 <- df_simple2 %>% 
  filter(date > "2022-11-06") %>%
  mutate(Preds = ifelse(Preds < 0, 0, Preds))
  filter(location == "Malaysia")
  
df_stats2b <- data %>% 
  bind_cols(predict(xgb_model2, data)) %>% 
  rename(Preds = "...36") %>% 
  mutate(Preds = ifelse(Preds < 0, 0, Preds))

# for a given country
colors <- c("new_cases" = "blue", "Preds" = "red")
ggplot(df_stats2b %>% filter(location == "Italy", date >= "2023-03-26"), aes(x = date)) +
  geom_line(aes(y = new_cases, color = "new_cases")) +
  geom_line(aes(y = Preds, color = "Preds")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(labels = c("New Cases", "Predictions"), values = colors) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.12,.85),
        panel.grid.major = element_blank()) +
  labs(x = NULL, y = "New Cases", title = "Italy (Testing Set Only)")

# test statistics
rmse_vec(truth = df_stats2$new_cases, estimate = df_stats2$Preds)
mase_vec(truth = df_stats2$new_cases, estimate = df_stats2$Preds)
round(MAPE(y_pred = df_stats2$new_cases, y_true = df_stats2$Preds), 6)
mae_vec(truth = df_stats2$new_cases, estimate = df_stats2$Preds)



######################################### XGBoost3
load("results/xgb_tuned3.rda")

df_simple3 <- df_withpreds3 %>% 
  select(date, location, new_cases, Preds)

xgb.ggplot.importance(xgb_importance3 %>% top_n(Feature, n=10), rel_to_first = TRUE) +
  ggtitle("Feature Importance - Top 10 Variables") +
  theme_minimal()

xgb.ggplot.importance(head(xgb_importance3, 10), rel_to_first = TRUE) +
  ggtitle("Feature Importance - Top 10 Variables") +
  theme_minimal()

ggplot(df_simple3, aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "New Cases", title = "Actuals and Predictions for All Countries")

# for a given country
ggplot(df_simple3 %>% filter(location == "Italy"), aes(x = date)) +
  geom_line(aes(y = new_cases), color = "blue") +
  geom_line(aes(y = Preds), color = "red") +
  theme_minimal() +
  labs(x = NULL, y = "New Cases", title = "Actuals and Predictions for Italy")


df_stats3 <- df_simple3 %>% 
  filter(date > "2022-11-06") %>%
  filter(location == "United Kingdom")

# test statistics
rmse_vec(truth = df_stats3$new_cases, estimate = df_stats3$Preds)
mase_vec(truth = df_stats3$new_cases, estimate = df_stats3$Preds)
round(MAPE(y_pred = df_stats3$new_cases, y_true = df_stats3$Preds), 6)
mae_vec(truth = df_stats3$new_cases, estimate = df_stats3$Preds)

