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


## Load data
full_data <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv"))

## Subset nutrition
nutrition <- full_data %>% 
  # select variables of interest 
  dplyr::select(pid, redcap_event_name, fats, carbs, protein, calories, tee, cal_percent_tee)
  
  
  
  
