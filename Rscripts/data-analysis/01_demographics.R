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

## Source functions
source(here::here("Rfunctions", "table1_helpers.R"))

## Load demographic data 
dems <- read_csv(here::here("data-clean", "identifiable", "Demographics.csv")) %>% 
  ## Keep only complete data 
  filter(!is.na(calc_age))

## Assign labels to dems variables 
label(dems$calc_age) <- "Age (Years)"
label(dems$nih_ethnicity_race) <- "Race/Ethnicity"
label(dems$pe_wt) <- "Weight (kg)"
label(dems$pe_ht) <- "Height (cm)"
label(dems$pe_bmi) <- "BMI (kg/m^2)"


## Print table1
table1(~ calc_age + nih_ethnicity_race + pe_wt + pe_ht + pe_bmi | group, 
       data = dems, overall = F, extra.col=list(`P-value`= pvalue), 
       render.continuous = my.render.cont)


