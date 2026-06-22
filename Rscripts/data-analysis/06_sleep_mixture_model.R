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
library(lme4)
library(lmerTest)
library(nlme)
library(tidyr)
library(mgcv)

## Load data 
sleep <- read_csv(here::here("data-clean", "NonAggregated_Normed_Sleep_cleaned.csv")) %>% 
  filter(!is.na(active_interp)) %>% 
  mutate(log_act = log(active_interp + 0.01)) %>% 
  mutate(index = t_norm.x*199) %>% 
  mutate(time = ifelse(weekday == "Weekend" | school == "Summer", "Unstructured Night", 
                       "Structured Night"))


# Fit mixture model on log(activity) with varying components
mix_fit <- Mclust(sleep$log_act, G = 1:4); summary(mix_fit)

sleep$state_mix <- factor(mix_fit$classification, 
                          levels = c(1:4), 
                          labels = c("Very Still Sleep", "Quiet Sleep", 
                                     "Restless Sleep", "Wake/Active"))


smooth_data <- sleep %>%
  group_by(ID, night) %>%
  nest() %>%
  mutate(fit = purrr::map(data, ~ {gam(log_act ~ s(index, bs = "cr", k = 50), 
                                       data = ., method = "REML", gamma = 0.5)}),
         preds = purrr::map2(data, fit, ~ {tibble(index = .$index,
                                           yhat = pmax(predict(.y, newdata = .x), log(0.01)))})) %>%
  select(ID, night, preds) %>%
  unnest(preds)
sleep$yhat <- smooth_data$yhat



### Multiple Nights Within Individual 
sleep_plt <- ggplot(sleep, aes(x = index, y = yhat, group = paste0(ID, night), color = group.x)) + 
  geom_line(alpha = 0.1, size = 0.8) + 
  theme_bw() + 
  geom_smooth(aes(group = group.x), size = 2) + 
  scale_x_continuous(breaks = c(10, 190), labels = c("Waketime", "Bedtime")) +
  xlab("") +  
  ylab("Log(Sleep Activity Score)") + 
  labs(color = "") + 
  theme(text = element_text(size = 20), 
        legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  scale_color_manual(values = c("#369dd9", "#6D6D6D")) + 
  facet_wrap(~ time) + 
  # Show levels of activity
  geom_hline(yintercept = -2.1, linetype = "dashed", color = "gray40", size = 0.6) +
  geom_hline(yintercept = 1.4, linetype = "dashed", color = "gray40", size = 0.6) + 
  geom_hline(yintercept = 3.4, linetype = "dashed", color = "gray40", size = 0.6) + 
  annotate("text", x = 4, y = -4, label = "Very Still Sleep", hjust = 0, size = 5) + 
  annotate("text", x = 4, y = -0.1, label = "Quiet Sleep", hjust = 0, size = 5) + 
  annotate("text", x = 4, y = 2.5, label = "Restless Sleep", hjust = 0, size = 5) + 
  annotate("text", x = 4, y = 5.1, label = "Wake/Active", hjust = 0, size = 5)
sleep_plt

ggsave(filename = here::here("outputs", "raw_sleep.png"), plot = sleep_plt, width = 11, height = 6, units = "in")




# Load non-normed data
sleep <- read_csv(here::here("data-clean", "NonAggregated1minSleep_cleaned.csv")) %>% 
  filter(!is.na(activity))

# Fit mixture model on log(activity) with varying components
mix_fit <- Mclust(sleep$log_act, G = 1:4); summary(mix_fit)

sleep$state_mix <- factor(mix_fit$classification, 
                          levels = c(1:4), 
                          labels = c("Very Still Sleep", "Quiet Sleep", 
                                     "Restless Sleep", "Wake/Active"))

# Get sleep stage summary per person and night
sleep_sum <- sleep %>% 
  mutate(time_period = ifelse(weekday == "Weekend" | school == "Summer", 
                              "Unstructured Night", "Structured Night")) %>% 
  group_by(ID, night, state_mix, group, time_period) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  group_by(ID, night, group, time_period) %>% 
  mutate(prop = N/sum(N)) %>% 
  ungroup()


p_vals <- sleep_sum %>% 
  group_by(state_mix, time_period) %>% 
  group_modify(~ {
    if (n_distinct(.x$group) < 2 || n_distinct(.x$ID) < 2) {
      return(tibble(p_var = NA_real_))
    }
    
    m0 <- lme(N ~ group, random = ~1 | ID, data = .x)
    
    m1 <- tryCatch(
      lme(N ~ group,
          random = ~1 | ID,
          weights = varIdent(form = ~1 | group),
          data = .x),
      error = function(e) NULL
    )
    
    p <- if (!is.null(m1)) {
      anova(m0, m1)$`p-value`[2]
    } else {
      NA_real_
    }
    
    tibble(p_var = p)
  }) %>%
  ungroup()



sleep_stages <- ggplot(sleep_sum, aes(x = group, y = prop, fill = group)) + 
  geom_boxplot() + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c("#369dd9", "#6D6D6D")) + 
  scale_y_continuous(
    breaks = function(limits) {
      rng <- limits[2] - limits[1]
      
      if (limits[2] >= 9) {
        seq(0, ceiling(limits[2] / 3) * 3, by = 3)
      } else {
        seq(floor(limits[1]), ceiling(limits[2]), by = 1)
      }
    }
  ) + 
  facet_grid(state_mix~time_period, scales = "free_y") + 
  labs(x = "", y = "Proportion of Sleep Time", fill = "")

ggsave(filename = here::here("outputs", "sleep_stages_proportion.png"), plot = sleep_stages, width = 8, height = 9, units = "in")



  
