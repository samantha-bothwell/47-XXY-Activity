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
library(hms)

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

sumdata_day <- sumdata_day %>% 
  mutate(dayofweek = weekdays(date), 
         month = months(date),
         weekday = case_when(dayofweek %in% c("Saturday", "Sunday") ~ "Weekend", 
                             .default = "Weekday"),
         school = ifelse(month %in% c("June", "July", "August"), "Summer", "School-Year"))


## FoSR
fit_we <- gam(met_minute ~ 
            # weekday + school + 
                   s(index, bs = "cc") +                   # functional intercept
                   s(index, by = group, bs = "cc") +       # functional effect for group
                   s(index, ID, bs = "fs", m = 1, k = 5),  # functional random intercept
                 family = Gamma(link = "log"),
                 data = sumdata_day %>% filter(!(weekday == "Weekday" & school == "School-Year")), 
           method = "REML")

saveRDS(fit, file = here::here("outputs", "met_fit.rds"))


fit <- fit_we
## Prediction data 
df_pred <- data.frame(index = seq(1, 1440, by = 1),
                      ID = sumdata_day$ID[1], 
                      group = 1, 
                      weekday = "Weekday", 
                      school = "School-Year")
fhat <- predict(fit, newdata=df_pred, se.fit=TRUE,type='terms')

## Pull linear functional fit estimates
group_hat <- fhat$fit[,"s(index):group"]; group_se <- fhat$se.fit[,"s(index):group"]
crude_ests <- data.frame(group_hat, 
                         group_low = group_hat - 1.96*group_se, 
                         group_high = group_hat + 1.96*group_se, 
                         sind = seq(1, 1440, by = 1))

## Make plots 
fda_mets_pct <- ggplot(crude_ests, aes(x = sind, y = group_hat)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), 
              fill = "blue", alpha = 0.2) +
  geom_line(size = 1)  + 
  ylab("Percent Difference in Expected METs per Minute\n(KS Cases vs Non-KS Controls)") + 
  ggtitle("Functional Percent Difference of METs per Minute \nWeekends or Summer") + theme_bw() +
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05), labels = c("-15%", "-10%", "-5%", "0%", "5%")) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 200, y = 0.07, label = "Higher KS Case Activity", size = 5)  + 
  annotate("text", x = 200, y = -0.1, label = "Lower KS Case Activity", size = 5)

ggsave(filename = here::here("outputs", "fda_mets_pct_weekends_summer.png"), plot = fda_mets_pct, width = 10, height = 7, units = "in")
