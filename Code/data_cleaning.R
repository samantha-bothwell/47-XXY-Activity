###################################################
# LTE Wearables Data Cleaning 
# 
# PI : Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : August 12th, 2024, 
#                 more data added on Jan 8th, 2024
####################################################

rm(list = ls())

## Libraries 
library(readxl)
library(janitor)
library(tidyverse)
library(refund)
library(mgcv)
library(refund.shiny)


## Get names of all datasets 
path <- "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/15sEpochs/"
files <- list.files(path)

## Demographic data 
dems <- read.csv("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/LTE212860Interrogati_DATA_2025-11-20_1034.csv") %>% 
  filter(!is.na(group))


## Loop over data and save summarized data 
sumdata_1min <- data.frame(matrix(NA, ncol = 4))
sumdata_day <- data.frame(matrix(NA, ncol = 5))
sumdata_15min <- data.frame(matrix(NA, ncol = 4))
names(sumdata_1min) = c("minute", "Avg_StepCount", "Avg_ActivityScore", "ID")
names(sumdata_day) = c("minute", "date", "StepCount", "ActivityScore", "ID")
names(sumdata_15min) = c("min15", "Avg_StepCount", "Avg_ActivityScore", "ID")


for(i in 1:length(files)){
  ## load data 
  pt_dat <- read.csv(paste0(path, files[i]))
  
  ## Save id 
  id <- substr(files[i], 1, 3)
  
  ## Convert all columns to numeric 
  pt_dat <- sapply( pt_dat, as.numeric )
  
  ## clean up date
  pt_dat <- data.frame(pt_dat) %>% 
    mutate(time = excel_numeric_to_date(time, include_time = TRUE)) 

  ## Summarize minute level data across days 
  pt_clean <- pt_dat %>% 
    # Save minute and date
    mutate(minute = substr(time, 12, 16), 
           date = substr(time, 1, 10)) %>%
    # Fix empty minutes 
    mutate(minute = ifelse(minute == "", "00:00", minute)) %>% 
    # Filter out when watch was not worn 
    filter(Nonwear.Time..s. == 0) %>% 
    # filter to days where watch was worn 70% of the time 
    group_by(date) %>% 
    filter(n() >= 1440*4*0.7) %>% 
    ungroup()
  
  if(nrow(pt_clean > 0)){
    pt_day <- pt_clean %>% 
      # Sum over minute
      group_by(minute, date) %>% 
      summarise(StepCount = sum(StepCount, na.rm = T),
                ActivityScore = sum(Activity.Score..MET.s., na.rm = T)) %>% 
      ungroup()
    
    pt_minute <- pt_day %>% 
      # Mean across minutes 
      group_by(minute) %>% 
      summarise(Avg_StepCount = mean(StepCount, na.rm = T), 
                Avg_ActivityScore = mean(ActivityScore, na.rm = T)) %>%
      ungroup()
    
    
    pt_15min <- pt_day %>% 
      # Get a 15 minute variable
      mutate(min15 = as.numeric(substr(minute, 1, 2))*60 + as.numeric(substr(minute, 4, 4))*10, 
             min15 = floor(min15/15)) %>% 
      # sum over each 15 minute window
      group_by(min15, date) %>% 
      summarise(StepCount = sum(StepCount, na.rm = T), 
                ActivityScore = sum(ActivityScore, na.rm = T)) %>% 
      ungroup() %>% 
      # Mean across 15 minutes 
      group_by(min15) %>% 
      summarise(Avg_StepCount = mean(StepCount, na.rm = T), 
                Avg_ActivityScore = mean(ActivityScore, na.rm = T)) %>%
      ungroup()
    
    # List ID 
    pt_day$ID <- id
    pt_minute$ID <- id
    pt_15min$ID <- id
  }

  ## Save data
  sumdata_1min <- data.frame(rbind(sumdata_1min, pt_minute))
  sumdata_day <- data.frame(rbind(sumdata_day, pt_day))
  sumdata_15min <- data.frame(rbind(sumdata_15min, pt_15min))
}


## Clean up datasets
sumdata_1min <- sumdata_1min %>% 
  # remove extra row from initializing dataframe
  filter(!is.na(minute)) %>% 
  # assign index 
  mutate(index = as.numeric(factor(minute, levels = unique(minute), 
                        labels = c(1:1440))))

sumdata_15min <- sumdata_15min %>% 
  # remove extra row from initializing dataframe
  filter(!is.na(min15)) %>% 
  # assign index 
  mutate(index = as.numeric(factor(min15, levels = unique(min15), 
                                   labels = c(1:96))))

sumdata_day <- sumdata_day %>% 
  # remove extra row from initializing dataframe
  filter(!is.na(minute)) %>% 
  # assign index 
  mutate(index = as.numeric(factor(minute, levels = unique(minute), 
                                   labels = c(1:1440))))

## Add group
sumdata_1min$group <- dems$group[match(sumdata_1min$ID, dems$pid)]
sumdata_1min$group <- ifelse(sumdata_1min$group == 1, "KS Case", "Non-KS Control")
sumdata_day$group <- dems$group[match(sumdata_day$ID, dems$pid)]
sumdata_day$group <- ifelse(sumdata_day$group == 1, "KS Case", "Non-KS Control")
sumdata_15min$group <- dems$group[match(sumdata_15min$ID, dems$pid)]
sumdata_15min$group <- ifelse(sumdata_15min$group == 1, "KS Case", "Non-KS Control")

## Remove the ids that were removed from the study (should be 501, 512, 514, 517, 518, and 565)
sumdata_1min <- sumdata_1min %>% filter(!is.na(group))
sumdata_day <- sumdata_day %>% filter(!is.na(group))
sumdata_15min <- sumdata_15min %>% filter(!is.na(group))


## Save files
write.csv(sumdata_1min, "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/Aggregated1min_cleaned.csv")
write.csv(sumdata_day, "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/Nonaggregated1min_cleaned.csv")
write.csv(sumdata_15min, "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/Aggregated15min_cleaned.csv")


