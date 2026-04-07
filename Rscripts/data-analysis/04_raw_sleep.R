#########################################
# LTE Wearables Raw Sleep
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : April 7th, 2026
#########################################

rm(list = ls())

## Libraries 
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)


## Load data 
dems_sleep <- read_csv(here::here("data-clean", "identifiable", "Sleep.csv"))

#####
## Plot sleep time variables
#####
## Bed Time
bt_pval <- t.test(bedtime_hours ~ group, data = dems_sleep)$p.value
bt_pval <- ifelse(bt_pval < 0.001, "p < 0.001", paste0("p = ", round(bt_pval, 3)))

bt_plot <- ggplot(dems_sleep, aes(x = group, y = bedtime_hours, fill = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  xlab("") + ylab("Bed Time") + 
  scale_y_continuous(breaks = seq(21, 26, by = 1), 
                     labels = c("9:00pm", "10:00pm", "11:00pm", "Midnight", "1:00am", "2:00am")) + 
  annotate("text", x = 1.5, y = max(dems_sleep$bedtime_hours, na.rm = TRUE), label = bt_pval,
            size = 5)

## Wake Time
wt_pval <- t.test(getup_hours ~ group, data = dems_sleep)$p.value
wt_pval <- ifelse(wt_pval < 0.001, "p < 0.001", paste0("p = ", round(wt_pval, 3)))

wt_plot <- ggplot(dems_sleep, aes(x = group, y = getup_hours, fill = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  xlab("") + ylab("Wake Time") + 
  scale_y_continuous(breaks = seq(6, 12, by = 1), 
                     labels = c("6:00am", "7:00am", "8:00am", "9:00am", "10:00am", "11:00am", "Noon")) + 
  annotate("text", x = 1.5, y = max(dems_sleep$getup_hours, na.rm = TRUE), label = wt_pval,
           size = 5)

## Sleep time
st_pval <- t.test(sleeptime_hours ~ group, data = dems_sleep)$p.value
st_pval <- ifelse(st_pval < 0.001, "p < 0.001", paste0("p = ", round(st_pval, 3)))

st_plot <- ggplot(dems_sleep, aes(x = group, y = sleeptime_hours, fill = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  xlab("") + ylab("Sleep Time (Hours)") + 
  scale_y_continuous(breaks = seq(5, 10, by = 1)) + 
  annotate("text", x = 1.5, y = max(dems_sleep$sleeptime_hours, na.rm = TRUE), label = st_pval,
           size = 5)


## Organize plots 
sleep_times <- grid.arrange(bt_plot, wt_plot, st_plot, ncol = 3)

ggsave(filename = here::here("outputs", "sleep_times.png"), plot = sleep_times, width = 12, height = 5, units = "in")



#####
## Plot sleep PROMIS variables
#####
## Pediatric
ped_pval <- t.test(sleep_ped_t ~ group, data = dems_sleep)$p.value
ped_pval <- ifelse(ped_pval < 0.001, "p < 0.001", paste0("p = ", round(ped_pval, 3)))

ped_plot <- ggplot(dems_sleep, aes(x = group, y = sleep_ped_t, fill = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  xlab("") + ylab("PROMIS Pediatric T-Score") + 
  scale_y_continuous(limits = c(35, 75), breaks = seq(40, 70, by = 10)) + 
  annotate("text", x = 1.5, y =75, label = ped_pval, size = 5)

## Adult
ad_pval <- t.test(sleep_ad_t ~ group, data = dems_sleep)$p.value
ad_pval <- ifelse(ad_pval < 0.001, "p < 0.001", paste0("p = ", round(ad_pval, 3)))

ad_plot <- ggplot(dems_sleep, aes(x = group, y = sleep_ad_t, fill = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  xlab("") + ylab("PROMIS Adult T-Score") + 
  scale_y_continuous(limits = c(35, 75), breaks = seq(40, 70, by = 10)) + 
  annotate("text", x = 1.5, y = 75, label = ad_pval, size = 5)

## Sleep time
pp_pval <- t.test(sleep_pp_t ~ group, data = dems_sleep)$p.value
pp_pval <- ifelse(pp_pval < 0.001, "p < 0.001", paste0("p = ", round(pp_pval, 3)))

pp_plot <- ggplot(dems_sleep, aes(x = group, y = sleep_pp_t, fill = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(text = element_text(size = 20), 
        legend.position = "none") + 
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  xlab("") + ylab("PROMIS Parent Proxy T-Score") + 
  scale_y_continuous(limits = c(35, 75), breaks = seq(40, 70, by = 10)) + 
  annotate("text", x = 1.5, y = 75, label = pp_pval, size = 5)


## Organize plots 
promis <- grid.arrange(ped_plot, ad_plot, pp_plot, ncol = 3)

ggsave(filename = here::here("outputs", "sleep_promis.png"), plot = promis, width = 12, height = 5, units = "in")


