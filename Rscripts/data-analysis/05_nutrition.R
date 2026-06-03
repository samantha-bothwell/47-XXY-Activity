#########################################
# LTE Wearables Food Intake
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : April 14th, 2026
#########################################

rm(list = ls())

## Libraries 
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(broom)
library(lme4)


## Load data
full_data <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv"))

## Subset nutrition
nutrition <- full_data %>% 
  # select variables of interest 
  dplyr::select(pid, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, fats, carbs, protein, 
                calories, tee, cal_percent_tee, dxa_lean_mass, group) %>% 
  # fill dxa_lean_mass 
  group_by(pid) %>% 
  fill(dxa_lean_mass, .direction = "down") %>% 
  fill(group, .direction = "down") %>% 
  ungroup() %>% 
  # filter to only food logs 
  filter(redcap_repeat_instrument == "food_logs_summaries") %>% 
  # Code group
  mutate(group = factor(group, levels = c(1, 0), labels = c("KS Case", "Non-KS Control"))) %>% 
  # Aggregate within individual 
  group_by(pid, group) %>% 
  summarise(across(c(fats, carbs, protein, calories, tee, cal_percent_tee, dxa_lean_mass), 
                ~ mean(.x, na.rm = T))) %>% 
  ungroup()

## Make a macronutrients plot
macros <- nutrition %>% 
  dplyr::select(pid, group, fats, carbs, protein) %>% 
  pivot_longer(!c(pid, group), names_to = "macronutrient", values_to = "percent") %>% 
  mutate(macronutrient = factor(macronutrient, levels = c("carbs", "fats", "protein"),
                                labels = c("Carbohydrates", "Fat", "Protein")))
  
macro_plot <- ggplot(macros, aes(x = macronutrient, y = percent, fill = group)) + 
  geom_boxplot() + 
  labs(x = "Macronutrient", y = "Average Daily Percent Intake", fill = "") + 
  theme_bw(base_size = 16) + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D"))

ggsave(filename = here::here("outputs", "macronutrients.png"), plot = macro_plot, width = 8, height = 6, units = "in")


cals_ener <- nutrition %>% 
  dplyr::select(pid, group, calories, tee, cal_percent_tee) %>% 
  pivot_longer(!c(pid, group), names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("calories", "tee", "cal_percent_tee"),
                                labels = c("Total Calories", "Total Energy Expenditure", 
                                           "Total Calories \nas a % of \nTotal Energy Expenditure")))


energy_calories <- ggplot(cals_ener, aes(y = value, fill = group, x = group)) + 
  geom_boxplot() + 
  stat_compare_means(method = "wilcox.test", label = "p.signif", label.y.npc = "top", 
                     vjust = 0.5, label.x.npc = 0.4, hide.ns = T, size = 6) +
  labs(x = "", y = "", fill = "") + 
  theme_bw(base_size = 16) + 
  theme(legend.position = "bottom", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  facet_wrap(~variable, scales = "free")

ggsave(filename = here::here("outputs", "calories_energy.png"), plot = energy_calories, width = 9, height = 5, units = "in")

  
