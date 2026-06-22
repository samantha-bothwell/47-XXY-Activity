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
library(Hmisc)

reload_helpers <- function() {source("~/.Rprofile")}; reload_helpers()


## Load data
full_data <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv"))
sumdata_day <- read_csv(here::here("data-clean", "Nonaggregated1min_cleaned.csv")) %>% 
  mutate(ID = as.character(ID))


## Subset nutrition
nutrition <- full_data %>% 
  # select variables of interest 
  dplyr::select(pid, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, fats, carbs, protein, 
                calories, tee, cal_percent_tee, dxa_lean_mass, group) %>% 
  # fill variables 
  group_by(pid) %>% 
  fill(dxa_lean_mass, .direction = "down") %>% 
  fill(group, .direction = "down") %>% 
  ungroup() %>% 
  # calculate calories and tee difference
  mutate(calories_minus_tee = calories - tee) %>% 
  # calculate calories per lean mass
  mutate(cals_per_leanmass = calories/dxa_lean_mass) %>% 
  # filter to only food logs 
  filter(redcap_repeat_instrument == "food_logs_summaries") %>% 
  # Code group
  mutate(group = factor(group, levels = c(1, 0), labels = c("KS Case", "Non-KS Control"))) %>% 
  # Aggregate within individual 
  group_by(pid, group) %>% 
  summarise(across(c(fats, carbs, protein, calories, tee, calories_minus_tee, cal_percent_tee, 
                     dxa_lean_mass, cals_per_leanmass), 
                ~ mean(.x, na.rm = T))) %>% 
  ungroup()


label(nutrition$fats) <- "% Fat"
label(nutrition$carbs) <- "% Carbohydrate"
label(nutrition$protein) <- "% Protein"
label(nutrition$calories) <- "Total Calories Consumed (cals)"
label(nutrition$tee) <- "Total Energy Expenditure (TEE) (cals)"
label(nutrition$calories_minus_tee) <- "Difference between Calories Consumed and TEE"
label(nutrition$cal_percent_tee) <- "Total Calories as a % of TEE (%)"
label(nutrition$cals_per_leanmass) <- "Calories Per Lean Mass (cals/kg)"


## Print table1
tbl2 <- table1(~ fats + carbs + protein + calories + tee + calories_minus_tee + cal_percent_tee + 
                 cals_per_leanmass| group, 
               data = nutrition, overall = F, extra.col=list(`P-value`= pvalue), 
               render.continuous = my.render.cont)

t1flex(tbl2) %>% 
  save_as_docx(path=here::here("outputs", "Table2.docx"))


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
  mutate(calories_per_lean_mass = calories / (dxa_lean_mass/1000)) %>% 
  dplyr::select(pid, group, calories, tee, cal_percent_tee, calories_per_lean_mass) %>% 
  pivot_longer(!c(pid, group), names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("calories", "tee", "cal_percent_tee", 
                                                "calories_per_lean_mass"),
                                labels = c("Total Calories (cals)", "Total Energy Expenditure (cals)", 
                                           "Total Calories as a % of \nTotal Energy Expenditure (%)", 
                                           "Calories Per Lean Mass \n(cals/kg)")))


energy_calories <- ggplot(cals_ener, aes(y = value, fill = group, x = group)) + 
  geom_boxplot() + 
  stat_compare_means(method = "wilcox.test", label = "p.signif", label.y.npc = "top", 
                     vjust = 0.7, label.x.npc = 0.4, hide.ns = T, size = 6) +
  labs(x = "", y = "", fill = "") + 
  theme_bw(base_size = 16) + 
  theme(legend.position = "bottom", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  facet_wrap(~variable, scales = "free")

ggsave(filename = here::here("outputs", "calories_energy.png"), plot = energy_calories, width = 7, height = 7, units = "in")


## Correlation with sleep
sleep_cor <- full_data %>% 
  dplyr::select(pid, group, sleep_ped_t, sleep_ad_t, calories, dxa_lean_mass) %>% 
  # fill variables 
  group_by(pid) %>% 
  fill(dxa_lean_mass, .direction = "down") %>% 
  fill(group, .direction = "down") %>% 
  fill(sleep_ped_t, .direction = "updown") %>% 
  fill(sleep_ad_t, .direction = "updown") %>% 
  ungroup() %>% 
  # filter out missing calories 
  filter(!is.na(calories)) %>% 
  # average calories and keep one row per person
  group_by(pid) %>% 
  mutate(calories = mean(calories, na.rm = T)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Code group
  mutate(group = factor(group, levels = c(1, 0), labels = c("KS Case", "Non-KS Control")))
  
# Peds
lm_stats <- sleep_cor %>%
  group_by(group) %>%
  do(tidy(lm(sleep_ped_t ~ calories, data = .))) %>%
  filter(term == "calories") %>%
  mutate(label = paste0(group,
                        ": β = ", round(estimate*100, 2), ", ",
                        ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", round(p.value, 3)))))
label = paste0(lm_stats$label[1], "\n", lm_stats$label[2])
  
ped_sleep_cals <- ggplot(sleep_cor, aes(x = calories, y = sleep_ped_t, color = group)) + 
  geom_point(size = 3) + geom_smooth(method = "lm") + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_color_manual(values = c("#369dd9", "#6D6D6D")) + 
  labs(x = "Average Daily Caloric Intake", y = "PROMIS Pediatric T-Score") +
  scale_y_continuous(limits = c(35, 75), breaks = seq(40, 70, by = 10)) +
  scale_x_continuous(limits = c(900, 3050), breaks = seq(1000, 3000, by = 500)) +
  annotate("label", x = 1000, y = 40, label = label, hjust = 0, size = 4.2, 
           fill = "white", color = "black", label.size = 0.4)

ggsave(filename = here::here("outputs", "calories_peds_sleep.png"), plot = ped_sleep_cals, width = 10, height = 5, units = "in")



# Adult
lm_stats <- sleep_cor %>%
  group_by(group) %>%
  do(tidy(lm(sleep_ad_t ~ calories, data = .))) %>%
  filter(term == "calories") %>%
  mutate(label = paste0(group,
                        ": β = ", round(estimate*100, 2), ", ",
                        ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", round(p.value, 3)))))
label = paste0(lm_stats$label[1], "\n", lm_stats$label[2])

ad_sleep_cals <- ggplot(sleep_cor, aes(x = calories, y = sleep_ad_t, color = group)) + 
  geom_point(size = 3) + geom_smooth(method = "lm") + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_color_manual(values = c("#369dd9", "#6D6D6D")) + 
  labs(x = "Average Daily Caloric Intake", y = "PROMIS Adult T-Score") +
  scale_y_continuous(limits = c(35, 75), breaks = seq(40, 70, by = 10)) +
  scale_x_continuous(limits = c(900, 3050), breaks = seq(1000, 3000, by = 500)) +
  annotate("label", x = 1000, y = 40, label = label, hjust = 0, size = 4.2, 
           fill = "white", color = "black", label.size = 0.4)

ggsave(filename = here::here("outputs", "calories_adult_sleep.png"), plot = ad_sleep_cals, width = 10, height = 5, units = "in")



## Correlation with activity
avg_mets <- sumdata_day %>% 
  group_by(date, ID, group) %>% 
  summarise(mean_mets = mean(met_minute, na.rm = T)) %>% 
  ungroup() %>% group_by(ID, group) %>% 
  summarise(mean_mets = mean(mean_mets, na.rm = T))

sleep_cor$avg_mets <- avg_mets$mean_mets[match(sleep_cor$pid, avg_mets$ID)]

lm_stats <- sleep_cor %>%
  group_by(group) %>%
  do(tidy(lm(avg_mets ~ calories, data = .))) %>%
  filter(term == "calories") %>%
  mutate(label = paste0(group,
                        ": β = ", round(estimate*1000, 2), ", ",
                        ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", round(p.value, 3)))))
label = paste0(lm_stats$label[1], "\n", lm_stats$label[2])

act_cals <- ggplot(sleep_cor, aes(x = calories, y = avg_mets, color = group)) + 
  geom_point(size = 3) + geom_smooth(method = "lm") + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_color_manual(values = c("#369dd9", "#6D6D6D")) + 
  labs(x = "Average Daily Caloric Intake", y = "Average Daily METS") +
  #scale_y_continuous(limits = c(35, 75), breaks = seq(40, 70, by = 10)) +
  scale_x_continuous(limits = c(900, 3050), breaks = seq(1000, 3000, by = 500)) +
  annotate("label", x = 1000, y = 1.55, label = label, hjust = 0, size = 4.2, 
           fill = "white", color = "black", label.size = 0.4)

ggsave(filename = here::here("outputs", "calories_mets.png"), plot = act_cals, width = 10, height = 5, units = "in")
