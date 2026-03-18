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

############ Environment setup
rm(list = ls())

## Libraries 
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(refund.shiny)

## Get names of all datasets 
epochs_dir <- here::here("data-raw", "15sEpochs")
files <- list.files(epochs_dir)

## Demographic data 
dems <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv"))

## Source functions
source(here::here("R", "parse_time.R"))



############ Data Processing 
## Initialize results dataframes
res_1min   <- vector("list", length(files))
res_15min  <- vector("list", length(files))
res_day    <- vector("list", length(files))


## Loop over files and summarize data
for (i in seq_along(files)) {
  ## Load patient epoch data
  fpath <- if (!is.null(epochs_dir)) file.path(epochs_dir, files[i]) else files[i]
  pt_dat <- suppressMessages(readr::read_csv(fpath, show_col_types = FALSE)) %>% clean_names()
  
  ## Extract participant ID from path
  id <- substr(basename(fpath), 1, 3)
  
  ## Clean date and time
  pt_dat <- pt_dat %>%
    mutate(time = parse_time(.data$time),
           date = as.Date(time),
           minute = format(time, "%H:%M"),
           # summarize 15 minute intervals
           min15 = floor((hour(time) * 60 + minute(time)) / 15))

  ## Filter out nonwear
  pt_clean <- pt_dat %>%
    filter(nonwear_time_s == 0) %>%
    # Only keep days with >= 70% wear at 15s epochs (5760/day)
    group_by(date) %>%
    filter(n() >= 1440 * 4 * 0.7) %>%
    ungroup()
  
  ## If there are no valid rows, skip
  if (nrow(pt_clean) == 0) {
    warning("No valid data for ID ", id)
    next
  }
  
  ## Summarize Activity within days 
  pt_day <- pt_clean %>%
    group_by(date, minute) %>%
    summarise(step_count = sum(.data$step_count, na.rm = TRUE),
              activity_score = sum(.data$activity_score_met_s, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ID = id) %>%
    select(minute, date, step_count, activity_score, ID)
  
  ## Summarize 1 minute level data accross all days
  pt_minute <- pt_day %>%
    group_by(minute) %>%
    summarise(avg_step_count = mean(step_count, na.rm = TRUE),
              avg_activity_score = mean(activity_score, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ID = id) %>%
    select(minute, avg_step_count, avg_activity_score, ID)
  
  ## Summarize 15 minute level data across all days
  pt_15min <- pt_clean %>%
    group_by(date, min15) %>%
    summarise(step_count = sum(.data$step_count, na.rm = TRUE),
              activity_score = sum(.data$activity_score_met_s, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(min15) %>%
    summarise(avg_step_count = mean(step_count, na.rm = TRUE),
              avg_activity_score = mean(activity_score, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ID = id) %>%
    select(min15, avg_step_count, avg_activity_score, ID)
  
  ## Save participant results
  res_day[[i]]   <- pt_day
  res_1min[[i]]  <- pt_minute
  res_15min[[i]] <- pt_15min
  
}

## Bind all the results
sumdata_day   <- dplyr::bind_rows(res_day)
sumdata_1min  <- dplyr::bind_rows(res_1min)
sumdata_15min <- dplyr::bind_rows(res_15min)


## Clean up datasets
sumdata_1min <- sumdata_1min %>% 
  # remove extra row from initializing dataframe
  filter(!is.na(minute)) %>% 
  # assign index 
  mutate(index = as.numeric(factor(minute, levels = unique(minute), labels = c(1:1440))))

sumdata_15min <- sumdata_15min %>% 
  # remove extra row from initializing dataframe
  filter(!is.na(min15)) %>% 
  # assign index 
  mutate(index = as.numeric(factor(min15, levels = unique(min15), labels = c(1:96))))

sumdata_day <- sumdata_day %>% 
  # remove extra row from initializing dataframe
  filter(!is.na(minute)) %>% 
  # assign index 
  mutate(index = as.numeric(factor(minute, levels = unique(minute), labels = c(1:1440))))


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
write.csv(sumdata_1min, here::here("inst", "extdata", "Aggregated1min_cleaned.csv"))
write.csv(sumdata_day, here::here("inst", "extdata", "Nonaggregated1min_cleaned.csv"))
write.csv(sumdata_15min, here::here("inst", "extdata", "Aggregated15min_cleaned.csv"))


