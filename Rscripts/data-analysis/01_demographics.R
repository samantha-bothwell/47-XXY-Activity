##############################################
# LTE Wearables Cohort Demographics Summary
#
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : March 23rd, 2026
##############################################

rm(list = ls())

## Libraries 
library(readr)
library(table1)
library(Hmisc)
library(flextable)
library(magrittr)
library(dplyr)


## Source functions
source(here::here("Rfunctions", "table1_helpers.R"))


## Load demographic data 
dems <- read_csv(here::here("data-clean", "identifiable", "Demographics.csv")) %>% 
  ## Keep only complete data 
  filter(!is.na(calc_age))

dems_sleep <- read_csv(here::here("data-clean", "identifiable", "Sleep.csv"))

## Assign labels to dems variables 
label(dems$calc_age) <- "Age (Years)"
label(dems$nih_ethnicity_race) <- "Race/Ethnicity"
label(dems$pe_wt) <- "Weight (kg)"
label(dems$pe_ht) <- "Height (cm)"
label(dems$pe_bmi) <- "BMI (kg/m^2)"
label(dems$season) <- "Activity School-Year vs Season Collection"
label(dems$n_days) <- "Number of Usable Activity Monitor Days (>70% Wear Time)"
label(dems$percent_wear) <- "Percent Wear Time Across Usable Days"
label(dems$avg_sleep_efficiency) <- "Average Sleep Efficiency (%)"
label(dems$avg_waso) <- "Average Wake After Sleep Onset (mins)"
label(dems$mean_mets) <- "Average Daily METs"
label(dems$mean_mvpa) <- "Average Percent of Time in MVPA"


## Print table1
tbl1 <- table1(~ calc_age + nih_ethnicity_race + pe_wt + pe_ht + pe_bmi + season + 
                 n_days + percent_wear + avg_sleep_efficiency + avg_waso + 
                 mean_mets + mean_mvpa| group, 
       data = dems, overall = F, extra.col=list(`P-value`= pvalue), 
       render.continuous = my.render.cont)

t1flex(tbl1) %>% 
  save_as_docx(path=here::here("outputs", "Table1.docx"))


## Manual means for times
as_hms(mean(as_hms(dems_sleep$getup_seconds[dems_sleep$group == "KS Case"]), na.rm = T))
as_hms(sd(as_hms(dems_sleep$getup_seconds[dems_sleep$group == "KS Case"]), na.rm = T))
as_hms(mean(as_hms(dems_sleep$getup_seconds[dems_sleep$group == "Non-KS Control"]), na.rm = T))
as_hms(sd(as_hms(dems_sleep$getup_seconds[dems_sleep$group == "Non-KS Control"]), na.rm = T))
t.test(as_hms(dems_sleep$getup_seconds[dems_sleep$group == "Non-KS Control"]), 
       as_hms(dems_sleep$getup_seconds[dems_sleep$group == "KS Case"]))

mean(dems_sleep$bedtime_hours[dems_sleep$group == "KS Case"], na.rm = T)
sd(dems_sleep$bedtime_hours[dems_sleep$group == "KS Case"], na.rm = T)
mean(dems_sleep$bedtime_hours[dems_sleep$group == "Non-KS Control"], na.rm = T)
sd(dems_sleep$bedtime_hours[dems_sleep$group == "Non-KS Control"], na.rm = T)
t.test(dems_sleep$bedtime_hours[dems_sleep$group == "Non-KS Control"], 
       dems_sleep$bedtime_hours[dems_sleep$group == "KS Case"])

mean(dems_sleep$sleeptime_hours[dems_sleep$group == "KS Case"], na.rm = T)
sd(dems_sleep$sleeptime_hours[dems_sleep$group == "KS Case"], na.rm = T)
mean(dems_sleep$sleeptime_hours[dems_sleep$group == "Non-KS Control"], na.rm = T)
sd(dems_sleep$sleeptime_hours[dems_sleep$group == "Non-KS Control"], na.rm = T)
t.test(dems_sleep$sleeptime_hours[dems_sleep$group == "Non-KS Control"], 
       dems_sleep$sleeptime_hours[dems_sleep$group == "KS Case"])

