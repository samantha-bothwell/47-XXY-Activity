#########################################
# LTE Wearables Latent Class Analysis
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : June 8th, 2026
#########################################

rm(list = ls())

## libraries 
library(refund)
library(tidyverse)
library(refund.shiny)
library(lavaan)
library(tidyLPA)

## Data 
sumsleep_1min_agg <- read.csv(here::here("data-clean", "Aggregated1minSleep_cleaned.csv")) %>% 
  mutate(ID = as.character(ID))
full_data <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv"))
sumdata_agg <- read_csv(here::here("data-clean", "Aggregated1min_cleaned.csv")) %>% 
  mutate(ID = as.character(ID))


## Reduce full data set
lca_data <- full_data %>% 
  # fill lean mass 
  group_by(pid) %>% 
  fill(dxa_lean_mass, .direction = "down") %>% 
  fill(group, .direction = "down") %>% 
  ungroup() %>% 
  # filter to only food logs 
  filter(redcap_repeat_instrument == "food_logs_summaries") %>% 
  dplyr::select(pid, group, fats, carbs, protein, calories, tee, cal_percent_tee, dxa_lean_mass) %>% 
  # average variables 
  group_by(pid, group) %>% 
  summarise(fats = mean(fats, na.rm = T), carbs = mean(carbs, na.rm = T), 
            protein = mean(protein, na.rm = T), calories = mean(calories, na.rm = T), 
            tee = mean(tee, na.rm = T), cal_percent_tee = mean(cal_percent_tee, na.rm = T), 
            dxa_lean_mass = mean(dxa_lean_mass, na.rm = T)) %>% 
  ungroup()


## Activity fpca 
sumdata_agg_wide <- sumdata_agg %>% 
  dplyr::select(ID, index, avg_met_minute) %>% 
  pivot_wider(names_from = "index", values_from = "avg_met_minute")

result <- fpca.face(Y = as.matrix(sumdata_agg_wide[,-1]))
#plot_shiny(result)
act_scores <- data.frame(result$scores[,1:6])
act_scores$ID <- sumdata_agg_wide$ID

lca_data$act_PCA1 <- act_scores$X1[match(lca_data$pid, act_scores$ID)]
lca_data$act_PCA2 <- act_scores$X2[match(lca_data$pid, act_scores$ID)]
lca_data$act_PCA3 <- act_scores$X3[match(lca_data$pid, act_scores$ID)]
lca_data$act_PCA4 <- act_scores$X4[match(lca_data$pid, act_scores$ID)]
lca_data$act_PCA5 <- act_scores$X5[match(lca_data$pid, act_scores$ID)]
lca_data$act_PCA6 <- act_scores$X6[match(lca_data$pid, act_scores$ID)]


## Sleep fpca 
sumsleep_1min_agg_wide <- sumsleep_1min_agg %>% 
  dplyr::select(ID, t_norm, mean_interp) %>% 
  pivot_wider(names_from = "t_norm", values_from = "mean_interp")

result <- fpca.face(Y = as.matrix(sumsleep_1min_agg_wide[,-1]))
#plot_shiny(result)
sleep_scores <- data.frame(result$scores[,1:8])
sleep_scores$ID <- sumsleep_1min_agg_wide$ID

lca_data$sleep_PCA1 <- sleep_scores$X1[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA2 <- sleep_scores$X2[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA3 <- sleep_scores$X3[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA4 <- sleep_scores$X4[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA5 <- sleep_scores$X5[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA6 <- sleep_scores$X6[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA7 <- sleep_scores$X7[match(lca_data$pid, sleep_scores$ID)]
lca_data$sleep_PCA8 <- sleep_scores$X8[match(lca_data$pid, sleep_scores$ID)]


## Run LPA
lca_data <- lca_data %>% filter(!(is.na(act_PCA1) & is.na(sleep_PCA1)))
diet <- lca_data %>%
  dplyr::select(-c(pid, group)) %>% 
  scale() %>%
  as.data.frame() %>% 
  select(fats, carbs, protein, calories, tee, act_PCA1, act_PCA2, act_PCA3, sleep_PCA1, sleep_PCA2, sleep_PCA3)

results_diet <- estimate_profiles(diet, 1:5, models = 1, package = "mclust") # BIC optimized for 3 classes
### NEED to justify looking at BIC over AIC or other parameters

diet_classes <- get_data(results_diet)
diet_classes$pid <- lca_data$pid
diet_classes$group <- lca_data$group

plot_profiles(results_diet)



act <- lca_data %>%
  dplyr::select(contains("act_")) %>% 
  as.data.frame()

results_act <- estimate_profiles(act, 1:4, models = 1, package = "mclust") # BIC optimized for 1 classes
### NEED to justify looking at BIC over AIC or other parameters

diet_classes <- get_data(results_diet)
diet_classes$pid <- lca_data$pid
diet_classes$group <- lca_data$group

plot_profiles(results_diet)