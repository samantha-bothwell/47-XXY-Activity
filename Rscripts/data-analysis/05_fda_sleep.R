#########################################
# LTE Wearables Sleep FDA
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
library(mgcv)
library(refund)
library(readr)
library(dplyr)
library(ggplot2)

## Load data 
sleep <- read_csv(here::here("data-clean", "NonAggregated1minSleep_cleaned.csv"))


## Clean up for modeling
sleep <- sleep %>% 
  # create a unique day_subject variable 
  mutate(subject_sleep = paste0(ID, "_", night)) %>% 
  # make sure ID and subject_day are factors
  mutate(ID = as.factor(ID)) %>% 
  mutate(night = as.factor(night)) %>% 
  # Make Group numeric (per pg 125 of textbook)
  mutate(group = as.numeric(group == "Case (KS)")) %>% 
  # filter out missing smooth variables 
  filter(!is.na(active_smooth))


## FoSR
fit <- gam(active_smooth ~ 
             group + weekday + school + 
             s(t_norm, bs = "cr") +                   # functional intercept
             s(t_norm, by = group, bs = "cr") +       # functional effect for group
             s(night, ID, bs = "re") +                # nesting of nights within individual
             s(t_norm, ID, bs = "fs", m = 1, k = 5),  # functional random intercept
           family = quasibinomial(link = "logit"),
           data = sleep, method = "REML")

saveRDS(fit, file = here::here("outputs", "sleep_mvmt_fit.rds"))


fit_we_summer <- gam(active_smooth ~ 
             s(t_norm, bs = "cr") +                   # functional intercept
             s(t_norm, by = group, bs = "cr") +       # functional effect for group
             #s(night, ID, bs = "re") +                # nesting of nights within individual
             s(t_norm, ID, bs = "fs", m = 1, k = 5),  # functional random intercept
           family = quasibinomial(link = "logit"),
           data = sleep %>% filter(weekday == "Weekend" | school == "Summer"), method = "REML")

saveRDS(fit_we_summer, file = here::here("outputs", "sleep_mvmt_we_summer_fit.rds"))


## Prediction data 
df_pred <- data.frame(t_norm = seq(0, 1, length = 200),
                      ID = sleep$ID[1], 
                      night = 1,
                      group = 1, 
                      weekday = "Weekday", 
                      school = "School-Year")
fhat <- predict(fit, newdata=df_pred, se.fit=TRUE,type='terms')

## Prediction data 
df_pred_we_summer <- data.frame(t_norm = seq(0, 1, length = 200),
                      ID = sleep$ID[1], 
                      night = 1,
                      group = 1)
fhat_we_summer <- predict(fit_we_summer, newdata=df_pred_we_summer, se.fit=TRUE,type='terms')

## Pull linear functional fit estimates
group_hat <- fhat$fit[,"s(t_norm):group"]; group_se <- fhat$se.fit[,"s(t_norm):group"]
crude_ests <- data.frame(group_hat, 
                         group_low = group_hat - 1.96*group_se, 
                         group_high = group_hat + 1.96*group_se, 
                         sind = seq(0, 1, length = 200))

## Pull linear functional fit estimates
group_hat <- fhat_we_summer$fit[,"s(t_norm):group"]; group_se <- fhat_we_summer$se.fit[,"s(t_norm):group"]
crude_ests <- data.frame(group_hat, 
                         group_low = group_hat - 1.96*group_se, 
                         group_high = group_hat + 1.96*group_se, 
                         sind = seq(0, 1, length = 200))


## Make plots 
fda_sleep_mvmt <- ggplot(crude_ests, aes(x = sind, y = group_hat)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), 
              fill = "blue", alpha = 0.2) +
  geom_line(size = 1)  + 
  ylab("Estimated Difference in Probability of Movement \n(KS Cases vs Non-KS Controls)") + 
  ggtitle("Functional Coefficient for \nProbability of Movement During Sleep") + theme_bw() +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Bedtime", "Waketime")) + 
  xlab("") + theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 0.2, y = 0.75, label = "Higher KS Case Movement", size = 5)  + 
  annotate("text", x = 0.2, y = -0.7, label = "Lower KS Case Movement", size = 5)

ggsave(filename = here::here("outputs", "fda_sleep_mvmt.png"), plot = fda_sleep_mvmt, width = 10, height = 7, units = "in")



