#########################################
# LTE Wearables Raw Activity
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : March 24th, 2026
#########################################

rm(list = ls())

## Libraries 
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(mgcv)
library(purrr)
library(ggpubr)


## Load data 
sumdata_day <- read_csv(here::here("data-clean", "Nonaggregated1min_cleaned.csv"))
sumdata_1min <- read_csv(here::here("data-clean", "Aggregated1min_cleaned.csv"))


## Smooth individual data 
smooth_data <- sumdata_day %>%
  group_by(ID, date) %>%
  nest() %>%
  mutate(fit = map(data, ~ {gam(met_minute ~ s(index, bs = "cs"), data = ., method = "REML")}),
    preds = map2(data, fit, ~ {tibble(index = .$index,
                                      yhat = pmax(predict(.y, newdata = .x), 1))})) %>%
  select(ID, date, preds) %>%
  unnest(preds)
sumdata_day$yhat <- smooth_data$yhat

### Multiple Days Within Individual 
mets_plt <- ggplot(sumdata_day, aes(x = index, group = paste0(ID, date), color = group)) + 
  geom_line(aes(y = yhat), alpha = 0.1, size = 0.8) + 
  theme_bw() + 
  geom_smooth(aes(y = met_minute, group = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + 
  ylab("METS per Minute") + 
  labs(color = "") + 
  theme(text = element_text(size = 20), 
        legend.position = "bottom") + 
  scale_color_manual(values = c("#369dd9", "#6D6D6D")) + 
  # Show levels of activity
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "gray40", size = 0.6) +
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "gray40", size = 0.6) + 
  annotate("text", x = 20, y = 1, label = "Sedentary Activity", hjust = 0, size = 5) + 
  annotate("text", x = 20, y = 2.25, label = "Light Activity", hjust = 0, size = 5) + 
  annotate("text", x = 20, y = 3.2, label = "Moderate-to-Vigorous Activity", hjust = 0, size = 5)


ggsave(filename = here::here("outputs", "raw_mets.png"), plot = mets_plt, width = 10, height = 7, units = "in")



### Summarize Avg Daily METs 
avg_mets <- sumdata_day %>% 
  group_by(date, ID, group) %>% 
  summarise(mean_mets = mean(met_minute, na.rm = T)) %>% 
  ungroup() %>% group_by(ID, group) %>% 
  summarise(mean_mets = mean(mean_mets, na.rm = T))

avg_mets_plt <- ggplot(avg_mets, aes(x = group, y = mean_mets, fill = group)) + 
  geom_boxplot(alpha = 0.8) + 
  theme_bw(base_size = 20) + 
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("Average Daily METS") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  stat_compare_means(method = "t.test", label = "p.format", label.x = 1.4, size = 6)


ggsave(filename = here::here("outputs", "raw_avg_mets.png"), plot = avg_mets_plt, width = 8, height = 6, units = "in")






### Self Reported Activity
sr_activity <- redcap %>% 
  # pull variables of interest
  dplyr::select(pid, rx_activitylevel, group) %>% 
  # fill activity level and group
  fill(rx_activitylevel, .direction = "downup") %>% 
  fill(group, .direction = "downup") %>% 
  # categorize activity level 
  mutate(rx_activitylevel = factor(rx_activitylevel, levels = c(1:5), 
                                   labels = c("Sedentary", "Lightly Active", "Moderately Active", 
                                              "Very Active", "Extremely Active")), 
         group = factor(group, levels = c(1, 0), labels = c("KS Case", "Non-KS Control"))) %>% 
  # Keep one record per person 
  group_by(pid) %>% slice(1)

sr_activity_sum <- sr_activity %>% 
  group_by(group, rx_activitylevel) %>% summarise(N = n()) %>% ungroup() %>% 
  group_by(group) %>% mutate(percent = (N/sum(N))*100) %>% ungroup()


sr_plot <- ggplot(sr_activity_sum, aes(x = percent, y = group, fill = rx_activitylevel)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("#fabec0", "#fa8e92", "#fa555b", "#d6020a")) + 
  theme_bw(base_size = 20) + 
  theme(legend.position = "bottom") + 
  #scale_x_continuous(labels = percent_format()) +
  labs(x = "Percent", y = "", 
       fill = "Self-Reported Activity Level") 

ggsave(filename = here::here("outputs", "sr_vs_actual_activity.png"), plot = sr_plot, width = 15, height = 7, units = "in")



### Self-Report vs Actual
avg_mets$sr_act <- sr_activity$rx_activitylevel[match(avg_mets$ID, sr_activity$pid)]

ggplot(avg_mets %>% filter(!is.na(sr_act)), aes(x = sr_act, y = mean_mets, fill = sr_act)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#fabec0", "#fa8e92", "#fa555b", "#d6020a")) + 
  theme_bw(base_size = 20) + 
  theme(legend.position = "none") + 
  labs(x = "Self-Reported Activity", y = "Average Daily METS") + 
  facet_wrap(~group, ncol = 1)


# within facet p-values
pvals <- avg_mets %>%
  filter(!is.na(sr_act)) %>%
  # keep only facets with both groups
  group_by(sr_act) %>%
  filter(n_distinct(group) == 2) %>%  
  # filter out very active as well
  filter(sr_act != "Very Active") %>% 
  summarise(p = t.test(mean_mets ~ group)$p.value) %>%
  mutate(label = paste0("p = ", signif(p, 3)))




sr_vs_actual <- ggplot(avg_mets %>% filter(!is.na(sr_act)), aes(x = group, y = mean_mets, fill = sr_act)) + 
  geom_boxplot() + 
  geom_jitter(fill = "white", pch = 21, width = 0.15, height = 0, size = 3) + 
  scale_fill_manual(values = c("#fabec0", "#fa8e92", "#fa555b", "#d6020a")) + 
  theme_bw(base_size = 20) + 
  theme(legend.position = "none") + 
  labs(x = "Self-Reported Activity", y = "Average Daily METS") + 
  facet_wrap(~sr_act, ncol = 4) + 
  geom_text(data = pvals,
            aes(x = 1.5, y = max(avg_mets$mean_mets, na.rm = TRUE) * 1.05, label = label),
            inherit.aes = FALSE, size = 5)

ggsave(filename = here::here("outputs", "sr_vs_actual_activity.png"), plot = sr_vs_actual, width = 15, height = 7, units = "in")

