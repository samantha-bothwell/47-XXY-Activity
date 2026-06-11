#########################################
# LTE Wearables Sleep Mixture Model 
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : June 4th, 2026
#########################################

rm(list = ls())

## Libraries 
library(mclust)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

## Load data 
sleep <- read_csv(here::here("data-clean", "NonAggregated1minSleep_cleaned.csv")) %>% 
  filter(!is.na(active_smooth))


# Fit mixture model on log(activity) with varying components
mix_fit <- Mclust(sleep$log_act, G = 1:4); summary(mix_fit)

sleep$state_mix <- factor(mix_fit$classification, 
                          levels = c(1:4), 
                          labels = c("Very Still Sleep", "Quiet Sleep", 
                                     "Restless Sleep", "Wake/Active"))

# Get sleep stage summary per person and night
sleep_sum <- sleep %>% 
  mutate(time_period = ifelse(weekday == "Weekend" | school == "Summer", 
                              "Weekend or Summer Night", "School-Night")) %>% 
  group_by(ID, night, state_mix, group, time_period) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  group_by(ID, night, time_period) %>% 
  mutate(prop = N/sum(N)) %>% 
  ungroup()


sleep_stages <- ggplot(sleep_sum, aes(x = group, y = prop, fill = group)) + 
  geom_boxplot() + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", label.y.npc = "top", 
                     vjust = 0.7, label.x.npc = 0.5, hide.ns = T, size = 6) +
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  facet_grid(state_mix~time_period, scales = "free_y") + 
  labs(x = "", y = "Proportion of Sleeping Time", fill = "")

ggsave(filename = here::here("outputs", "sleep_stages.png"), plot = sleep_stages, width = 8, height = 9, units = "in")



  
