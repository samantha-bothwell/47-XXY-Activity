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
  mutate(index = t_norm*199)


# Fit mixture model on log(activity) with varying components
mix_fit <- Mclust(sleep$log_act, G = 1:4); summary(mix_fit)

sleep$state_mix <- factor(mix_fit$classification, 
                          levels = c(1:4), 
                          labels = c("Very Still Sleep", "Quiet Sleep", 
                                     "Restless Sleep", "Wake/Active"))


smooth_data <- sleep %>%
  group_by(ID, night) %>%
  nest() %>%
  mutate(fit = purrr::map(data, ~ {gam(log_act ~ s(index, bs = "cs", k = 20), data = ., method = "REML")}),
         preds = purrr::map2(data, fit, ~ {tibble(index = .$index,
                                           yhat = predict(.y, newdata = .x))})) %>%
  select(ID, night, preds) %>%
  unnest(preds)
sleep$yhat <- smooth_data$yhat

group_avg <- sleep %>%
  group_by(group, index) %>%
  summarise(
    log_act_mean = mean(log_act, na.rm = TRUE),
    .groups = "drop"
  )

overall_smooth <- group_avg %>%
  group_by(group) %>%
  group_modify(~ {
    fit <- gam(log_act_mean ~ s(index, bs = "cs", k = 30),
               data = ., method = "REML")
    tibble(index = .x$index,
           yhat = exp(predict(fit, newdata = .x)))
  }) %>%
  ungroup()


### Multiple Nights Within Individual 
sleep_plt <- ggplot(sleep, aes(x = index, group = paste0(ID, night), color = group)) + 
  geom_line(aes(y = yhat), alpha = 0.1, size = 0.8) + 
  theme_bw() + 
  # geom_smooth(data = overall_smooth, aes(x = index, y = yhat, group = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 200),
                     labels = c("Waketime", "Bedtime")) +
  xlab("") + 
  #ylab("METS per Minute") + 
  labs(color = "") + 
  theme(text = element_text(size = 20), 
        legend.position = "bottom") + 
  scale_color_manual(values = c("#369dd9", "#6D6D6D")) + 
  # Show levels of activity
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "gray40", size = 0.6) +
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "gray40", size = 0.6) + 
  annotate("text", x = 20, y = 1.1, label = "Sedentary Activity", hjust = 0, size = 5) + 
  annotate("text", x = 20, y = 2.25, label = "Light Activity", hjust = 0, size = 5) + 
  annotate("text", x = 20, y = 4, label = "Moderate-to-Vigorous Activity", hjust = 0, size = 5)
mets_plt


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



  
