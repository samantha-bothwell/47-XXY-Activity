#########################################
# LTE Wearables PCA
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : June 25th, 2026
#########################################

rm(list = ls())

## Libraries 
library(readr)
library(tidyverse)
library(psych)
library(gridExtra)

## Load data 
sleep <- read_csv(here::here("data-clean", "Sleep_Stages.csv"))


## Code bouts 
sleep_pca_night <- sleep %>%
  group_by(ID, night) %>%
  mutate(change = ifelse(lag(state_mix) != state_mix, "Yes", "No"),
         change = replace_na(change, "Yes"), 
         bout_id = cumsum(change == "Yes"), 
         n_awakenings = sum(change == "Yes" & state_mix == "Wake/Active")) %>% 
  ungroup() %>% 
  group_by(ID, night, bout_id) %>%
  mutate(bout_length = n()) %>%
  ungroup() %>% 
  # average within night and ID
  group_by(ID, group.x, night) %>%
  summarise(hours_quiet = sum(state_mix == "Quiet Sleep")/60,
            hours_restless = sum(state_mix == "Restless Sleep")/60,
            hours_awake = sum(state_mix == "Wake/Active")/60,
            mean_bout_quiet = mean(bout_length[state_mix == "Quiet Sleep" & change == "Yes"]), 
            mean_bout_restless = mean(bout_length[state_mix == "Restless Sleep" & change == "Yes"]), 
            mean_bout_awake = mean(bout_length[state_mix == "Wake/Active" & change == "Yes"]), 
            n_awakenings = mean(n_awakenings)) %>% 
  ungroup()

sleep_pca_id <- sleep_pca_night %>% 
  # average across nights within ID
  group_by(ID, group.x) %>% 
  summarise(idmn_hours_quiet = mean(hours_quiet), idmn_hours_restless = mean(hours_restless),
            idmn_hours_awake = mean(hours_awake), idmn_mean_bout_quiet = mean(mean_bout_quiet), 
            idmn_mean_bout_restless = mean(mean_bout_restless), idmn_mean_bout_awake = mean(mean_bout_awake), 
            idmn_n_awakenings = mean(n_awakenings)) %>% 
  ungroup() %>% 
  # Scale all variables 
  mutate(hours_quiet_scaled = scale(idmn_hours_quiet)[,1],
         hours_restless_scaled = scale(idmn_hours_restless)[,1],
         hours_awake_scaled = scale(idmn_hours_awake)[,1],
         mean_bout_quiet_scaled = scale(idmn_mean_bout_quiet)[,1],
         mean_bout_restless_scaled = scale(idmn_mean_bout_restless)[,1],
         mean_bout_awake_scaled = scale(idmn_mean_bout_awake)[,1],
         n_awakenings_scaled = scale(idmn_n_awakenings)[,1])

# adjust and standardize individual sleep estimates
sleep_pca_night <- sleep_pca_night %>% 
  left_join(sleep_pca_id, by = c("ID")) %>% 
  mutate(hours_quiet_adj = hours_quiet - idmn_hours_quiet,
         hours_restless_adj = hours_restless - idmn_hours_restless,
         hours_awake_adj = hours_awake - idmn_hours_awake,
         mean_bout_quiet_adj = mean_bout_quiet - idmn_mean_bout_quiet,
         mean_bout_restless_adj = mean_bout_restless - idmn_mean_bout_restless,
         mean_bout_awake_adj = mean_bout_awake - idmn_mean_bout_awake,
         n_awakenings_adj = n_awakenings - idmn_n_awakenings) %>% 
  # Scale all variables 
  mutate(hours_quiet_scaled_night = scale(hours_quiet_adj)[,1],
         hours_restless_scaled_night = scale(hours_restless_adj)[,1],
         hours_awake_scaled_night = scale(hours_awake_adj)[,1],
         mean_bout_quiet_scaled_night = scale(mean_bout_quiet_adj)[,1],
         mean_bout_restless_scaled_night = scale(mean_bout_restless_adj)[,1],
         mean_bout_awake_scaled_night = scale(mean_bout_awake_adj)[,1],
         n_awakenings_scaled_night = scale(n_awakenings_adj)[,1])


write_csv(sleep_pca_night, here::here("data-clean", "Sleep_PCA_Night.csv"))
write_csv(sleep_pca_id, here::here("data-clean", "Sleep_PCA_ID.csv"))



######### PCA 
# Between ID dataset 
between_scaled <- sleep_pca_id %>% 
  dplyr::select(contains("_scaled"))

within_scaled <- sleep_pca_night %>% 
  dplyr::select(contains("_scaled_night"))

# Run PCAs separately
pca_between <- prcomp(between_scaled, center = FALSE, scale. = FALSE)
pca_within  <- prcomp(within_scaled,  center = FALSE, scale. = FALSE)

par(mfrow = c(1, 2))
screeplot(pca_between, main = "Between-person", type = "lines")
screeplot(pca_within,  main = "Within-person",  type = "lines")


# Extract scores and get labels 
between_scores <- as.data.frame(pca_between$x) %>%
  mutate(ID = sleep_pca_id$ID, group = sleep_pca_id$group.x)

within_scores <- as.data.frame(pca_within$x) %>%
  mutate(ID = sleep_pca_night$ID, group = sleep_pca_night$group.x.x)


bw_plot <- ggplot(between_scores, aes(x = PC1, y = PC2, color = group)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95) +
  labs(title = "Between-person PCA: Group Separation") +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom")

wi_plot <- ggplot(within_scores, aes(x = PC1, y = PC2, color = group)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95) +
  labs(title = "Within-person PCA: Night-level Patterns") +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom")

grid.arrange(bw_plot, wi_plot, ncol = 2)

summary(lm(PC2 ~ group, data = between_scores))

## Inspect loadings
print(pca_between$rotation[, 1:5])
print(pca_within$rotation[,  1:5])
