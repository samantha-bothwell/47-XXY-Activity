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

## Load data 
sleep <- read_csv(here::here("data-clean", "NonAggregated1minSleep_cleaned.csv")) %>% 
  filter(!is.na(active_smooth))


# Fit mixture model on log(activity) with varying components
mix_fit <- Mclust(sleep$log_act, G = 1:4); summary(mix_fit)

sleep$state_mix <- factor(mix_fit$classification, 
                          levels = c(1:4), 
                          labels = c("Very Still Sleep", "Quiet (Typical) Sleep", 
                                     "Restless Sleep", "Wake/Active"))

# Get sleep stage summary per person and night
sleep_sum <- sleep %>% 
  group_by(ID, night, state_mix, group) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  group_by(ID, night) %>% 
  mutate(prop = N/sum(N)) %>% 
  ungroup()


sleep_stages <- ggplot(sleep_sum, aes(x = state_mix, y = prop, fill = group)) + 
  geom_boxplot() + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", label.y.npc = "top", 
                     vjust = 0.7, label.x.npc = 0.4, hide.ns = T, size = 6) +
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  facet_wrap(~state_mix, scales = "free") + 
  labs(x = "", y = "Proportion of Sleeping Time", fill = "")

ggsave(filename = here::here("outputs", "sleep_stages.png"), plot = sleep_stages, width = 7, height = 7, units = "in")



  
