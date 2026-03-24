#########################################
# LTE Wearables Activity FDA
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
library(mgcv)
library(refund)
library(readr)
library(dplyr)
library(ggplot2)

## Load data 
sumdata_day <- read_csv(here::here("data-clean", "Nonaggregated1min_cleaned.csv"))

## Assign day index 
sumdata_day <- sumdata_day %>% 
  group_by(ID) %>% 
  arrange(date, .by_group = TRUE) %>%
  mutate(day_index = as.integer(date - min(date)) + 1) %>%
  ungroup() %>% 
  # create a unique day_subject variable 
  mutate(subject_day = paste0(ID, "_", day_index)) %>% 
  # make sure ID and subject_day are factors
  mutate(ID = as.factor(ID), subject_day = as.factor(subject_day)) %>% 
  # Make Group numeric (per pg 125 of textbook)
  mutate(group = as.numeric(group == "KS Case"))


## Naive FoSR
fit <- gam(met_minute ~ 
                   s(index, bs = "cc") +                   # functional intercept
                   s(index, by = group, bs = "cc") +       # functional effect for group
                   s(index, ID, bs = "fs", m = 1, k = 5),  # functional random intercept
                   family = Gamma(link = "log"),
                 data = sumdata_day, method = "REML")

saveRDS(fit, file = here::here("outputs", "met_fit.rds"))

## Prediction data 
df_pred <- data.frame(index = seq(1, 1440, by = 1),
                      ID = sumdata_day$ID[1], 
                      group = 1)
fhat <- predict(fit, newdata=df_pred, se.fit=TRUE,type='terms')

## Pull linear functional fit estimates
group_hat <- fhat$fit[,2]; group_se <- fhat$se.fit[,2]
crude_ests <- data.frame(group_hat, 
                         group_low = group_hat - 1.96*group_se, 
                         group_high = group_hat + 1.96*group_se, 
                         sind = seq(1, 1440, by = 1))

## Make plots 
ggplot(crude_ests, aes(x = sind, y = group_hat)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), 
              fill = "blue", alpha = 0.2) +
  geom_line(size = 1)  + 
  ylab("Estimated Difference in METs per Minute\n(KS Cases vs Non-KS Controls)") + 
  ggtitle("Crude Model Functional Coefficient \nof METs per Minute") + theme_bw() +
  scale_x_continuous(breaks = c(0, 12, 24, 36, 48, 60, 72, 84, 96), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5))


