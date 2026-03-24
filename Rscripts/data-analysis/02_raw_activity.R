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


## Load data 
sumdata_day <- read_csv(here::here("data-clean", "Nonaggregated1min_cleaned.csv"))
sumdata_1min <- read_csv(here::here("data-clean", "Aggregated1min_cleaned.csv"))


### Multiple Days Within Individual 
mets <- ggplot(sumdata_day, aes(x = index, y = met_minute, group = paste0(ID, date), color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.1, size = 0.8) + 
  theme_bw() + 
  geom_smooth(aes(x = index, y = met_minute, group = group, color = group), size = 2) + 
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


ggsave(filename = here::here("outputs", "raw_mets.png"), plot = mets, width = 10, height = 7, units = "in")

