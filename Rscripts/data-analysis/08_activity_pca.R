#########################################
# LTE Wearables Activity PCA
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
sumdata_day <- read_csv(here::here("data-clean", "Nonaggregated1min_cleaned.csv"))

## Code bouts 
activity_pca <- sumdata_day %>%
  mutate(activity_level = case_when(met_minute >= 3 ~ "Moderate-to-Vigorous Activity", 
                                    met_minute >= 1.5 ~ "Light Activity", 
                                    met_minute < 1.5 ~ "Sedentary")) %>% 
  group_by(ID, date) %>%
  mutate(change = ifelse(lag(activity_level) != activity_level, "Yes", "No"),
         change = replace_na(change, "Yes"), 
         bout_id = cumsum(change == "Yes")) %>% 
  ungroup() %>% 
  group_by(ID, date, bout_id) %>%
  mutate(bout_length = n()) %>%
  ungroup() %>% 
  # average within day and ID
  group_by(ID, group, date) %>%
  summarise(hours_mvpa = sum(activity_level == "Moderate-to-Vigorous Activity")/60,
            hours_light = sum(activity_level == "Light Activity")/60,
            mean_bout_mvpa = mean(bout_length[activity_level == "Moderate-to-Vigorous Activity" & change == "Yes"]), 
            mean_bout_light = mean(bout_length[activity_level == "Light Activity" & change == "Yes"])) %>% 
  ungroup() %>% 
  mutate(mean_bout_mvpa = ifelse(is.na(mean_bout_mvpa), 0, mean_bout_mvpa)) %>% 
  mutate(mean_bout_light = ifelse(is.na(mean_bout_light), 0, mean_bout_light))


activity_pca_id <- activity_pca %>% 
  # average across days within ID
  group_by(ID, group) %>% 
  summarise(idmn_hours_mvpa = mean(hours_mvpa), idmn_hours_light = mean(hours_light),
            idmn_mean_bout_mvpa = mean(mean_bout_mvpa), idmn_mean_bout_light = mean(mean_bout_light)) %>% 
  ungroup() %>% 
  # Scale all variables 
  mutate(hours_mvpa_scaled = scale(idmn_hours_mvpa)[,1],
         hours_light_scaled = scale(idmn_hours_light)[,1],
         mean_bout_mvpa_scaled = scale(idmn_mean_bout_mvpa)[,1],
         mean_bout_light_scaled = scale(idmn_mean_bout_light)[,1])

# adjust and standardize individual activity estimates
activity_pca_day <- activity_pca %>% 
  left_join(activity_pca_id, by = c("ID")) %>% 
  mutate(hours_mvpa_adj = hours_mvpa - idmn_hours_mvpa,
         hours_light_adj = hours_light - idmn_hours_light,
         mean_bout_mvpa_adj = mean_bout_mvpa - idmn_mean_bout_mvpa,
         mean_bout_light_adj = mean_bout_light - idmn_mean_bout_light) %>% 
  # Scale all variables 
  mutate(hours_mvpa_scaled_day = scale(hours_mvpa_adj)[,1],
         hours_light_scaled_day = scale(hours_light_adj)[,1],
         mean_bout_mvpa_scaled_day = scale(mean_bout_mvpa_adj)[,1],
         mean_bout_light_scaled_day = scale(mean_bout_light_adj)[,1])


write_csv(activity_pca_day, here::here("data-clean", "Activity_PCA_Day.csv"))
write_csv(activity_pca_id, here::here("data-clean", "Activity_PCA_ID.csv"))



######### PCA 
# Between ID dataset 
between_scaled <- activity_pca_id %>% 
  dplyr::select(contains("_scaled"))

within_scaled <- activity_pca_day %>% 
  dplyr::select(contains("_scaled_day"))

# Run PCAs separately
pca_between <- prcomp(between_scaled, center = FALSE, scale. = FALSE)
pca_within  <- prcomp(within_scaled,  center = FALSE, scale. = FALSE)

par(mfrow = c(1, 2))
screeplot(pca_between, main = "Between-person", type = "lines")
screeplot(pca_within,  main = "Within-person",  type = "lines")


# Extract scores and get labels 
between_scores <- as.data.frame(pca_between$x) %>%
  mutate(ID = activity_pca_id$ID, group = activity_pca_id$group)

within_scores <- as.data.frame(pca_within$x) %>%
  mutate(ID = activity_pca_day$ID, group = activity_pca_day$group.x)


bw_plot <- ggplot(between_scores, aes(x = PC1, y = PC2, color = group)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95) +
  labs(title = "Between-person PCA: Group Separation") +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom")

wi_plot <- ggplot(within_scores, aes(x = PC1, y = PC2, color = group)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95) +
  labs(title = "Within-person PCA: Day-level Patterns") +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom")

grid.arrange(bw_plot, wi_plot, ncol = 2)

summary(lm(PC1 ~ group, data = between_scores))
summary(lm(PC1 ~ group, data = within_scores))

bartlett.test(PC2 ~ group, data = within_scores)
#Controls tend to have a globally more active activity profile characterized by 
#greater time in light activity and Moderate-to-Vigorous (non-sedantary) activity
#and somewhat longer activity bouts.

## Inspect loadings
print(pca_between$rotation[, 1:5])
print(pca_within$rotation[,  1:5])
