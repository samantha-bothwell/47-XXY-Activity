#########################################
# LTE Wearables Sleep Data Data Cleaning 
# 
# PI : Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : September 16th, 2024
#########################################

rm(list = ls())

## Libraries 
library(here)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(zoo)

## Get names of all datasets 
epochs_dir <- here::here("data-raw", "Sleep")
files <- list.files(epochs_dir)

## Demographic data 
dems <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv")) %>% 
  # filter out IDs with a missing group
  filter(!is.na(group)) %>%
  # replace "z" in IDs
  mutate(pid = gsub("z", "", pid))


## Sleep Diaries
sleep_diary <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv")) %>% 
  # replace "z" in IDs
  mutate(pid = gsub("z", "", pid)) %>% 
  # keep only sleep logs
  filter(redcap_repeat_instrument == "daily_sleep_log") %>% 
  # keep only variables for validation 
  dplyr::select(pid, sd_date, sd_q1, sd_q5) %>% 
  # create a sleep date variable 
  mutate(sleep_date = ifelse(as.character(sd_q1) < "05:00:00", sd_date, sd_date - 1), 
         sleep_date = as.Date(sleep_date, origin = "1970-01-01")) %>% 
  # rename variables 
  rename(wake_date = sd_date, sleep_time = sd_q1, wake_time = sd_q5)



############ Data Processing 
## Initialize results dataframes
res_day <- vector("list", length(files))
res_1min <- vector("list", length(files))
res_1min_agg <- vector("list", length(files))


## Loop over files and summarize data
for(i in seq_along(files)){
  
  ## Load patient epoch data
  fpath <- if (!is.null(epochs_dir)) file.path(epochs_dir, files[i]) else files[i]
  pt_dat <- suppressMessages(readr::read_csv(fpath, show_col_types = FALSE)) %>% 
    clean_names() %>% filter(!is.na(line)) %>% 
    # Assume awake if off wrist
    mutate(sleep_wake = ifelse(off_wrist_status == 1, 1, sleep_wake), 
           interval_status = ifelse(off_wrist_status == 1, "ACTIVE", interval_status))
  
  ## Extract participant ID from path
  id <- substr(basename(fpath), 1, 3)
  
  print(paste0("index = ", i, "; ID = ", id))
  
  ## Get sleep validation 
  sleep_val <- sleep_diary %>% 
    filter(pid == id)
  
  ## Require date to be in diary for validation 
  pt_dat <- pt_dat %>% 
    mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
    filter(date %in% c(sleep_val$wake_date, sleep_val$sleep_date))
  
  ## Manual exclusion for lack of usable data
  ## If there are no valid rows, skip
  if (id %in% c("510", "518", "557", "562", "566", "570")) {
    warning("No valid data for ID ", id)
    next
  }
  
  ## Day summaries 
  pt_day <- pt_dat %>% 
    group_by(date) %>% 
    summarise(asleep_time = length(interval_status[interval_status == "REST-S"] == TRUE),
              awake_time = length(interval_status[interval_status %in% c("ACTIVE", "REST")] == TRUE), 
              prop_asleep = asleep_time/1440, 
              prop_awake = awake_time/1440) %>% 
    # Filter to only days where the watch was worn at least 90% of the time
    filter(prop_awake + prop_asleep > 0.9) %>% 
    # filter to only days with less than 75% of time asleep
    filter(prop_asleep < 0.75 & prop_asleep > 0.083) %>% 
    ungroup() %>% 
    # Sort by date
    mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
    arrange(date) %>%
    filter(date != min(date), date != max(date)) %>% 
    # Assign day index 
    mutate(index = 1:length(date)) %>%
    mutate(dayofweek = weekdays(date), 
           month = months(date),
           weekday = ifelse(dayofweek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>% 
    mutate(ID = id)
  
  
  ## Summarize minute level data across days 
  pt_minute <- vector("list", length(unique(sleep_val$sleep_date)))
  pt_agg <- vector("list", length(unique(sleep_val$sleep_date)))
  
  for(j in 1: length(unique(sleep_val$sleep_date))){
    sleep_val_day <- sleep_val[j,]
    
    sleep_minute <- pt_dat %>% 
      # Filter to only night time sleep 
      filter(interval_status == "REST-S") %>% 
      # filter to wake and sleep dates 
      filter((date == sleep_val_day$sleep_date & time > sleep_val_day$sleep_time) | 
               (date == sleep_val_day$wake_date & time < sleep_val_day$wake_time)) %>% 
      # Determine activity 
      mutate(active = activity >= 14, # based off of mixture model
             log_act = log(activity + 1)) %>% 
      mutate(night = j)
    
    if (dim(sleep_minute)[1] == 0) {
      warning("No valid data on ", sleep_val_day, " for ID ", id)
      next
    }
    
    
    # Make a smoothed probability of activity
    window = 20  # ~5 min
    sleep_minute$active_smooth = rollapply(sleep_minute$active,
                                           width = window, FUN = mean, fill = NA, align = "center")
    
    # Normalize time 
    n = nrow(sleep_minute)
    sleep_minute$t_norm = seq(0, 1, length.out = n)
    
    pt_minute[[j]] <- sleep_minute
    
    ## Approximate to get an aggregate within individual 
    grid <- seq(0, 1, length.out = 200)
    interp_fun <- approxfun(sleep_minute$t_norm, sleep_minute$active_smooth, rule = 2)
    night_interp <- interp_fun(grid)
    night_agg <- data.frame(night = j, t_norm = grid, active_interp = night_interp)
    
    pt_agg[[j]] <- night_agg
    
  }
  
  ## Bind all the results
  pt_minute <- dplyr::bind_rows(pt_minute) %>% 
    mutate(ID = id)
  
  pt_agg <- dplyr::bind_rows(pt_agg) %>% 
    group_by(t_norm) %>% 
    summarise(mean_interp = mean(active_interp)) %>% 
    ungroup() %>% 
    mutate(ID = id)
  
  
  ## Save participant results
  res_day[[i]] <- pt_day
  res_1min[[i]] <- pt_minute
  res_1min_agg[[i]] <- pt_agg
  
}


## Bind all the results
sleep_daysum <- dplyr::bind_rows(res_day)
sumsleep_1min <- dplyr::bind_rows(res_1min)
sumsleep_1min_agg <- dplyr::bind_rows(res_1min_agg)


## Add group
sumsleep_1min$group <- dems$group[match(sumsleep_1min$ID, dems$pid)]
sumsleep_1min$group <- ifelse(sumsleep_1min$group == 1, "Case (KS)", "Control")
sleep_daysum$group <- dems$group[match(sleep_daysum$ID, dems$pid)]
sleep_daysum$group <- ifelse(sleep_daysum$group == 1, "Case (KS)", "Control")
sumsleep_1min_agg$group <- dems$group[match(sumsleep_1min_agg$ID, dems$pid)]
sumsleep_1min_agg$group <- ifelse(sumsleep_1min_agg$group == 1, "Case (KS)", "Control")

## Remove the ids that were removed from the study (should be 501, 512, 514, 517, 518, and 565)
sumsleep_1min <- sumsleep_1min %>% filter(!is.na(group))
sleep_daysum <- sleep_daysum %>% filter(!is.na(group))
sumsleep_1min_agg <- sumsleep_1min_agg %>% filter(!is.na(group))


## Save files
write.csv(sumsleep_1min, here::here("data-clean", "NonAggregated1minSleep_cleaned.csv"))
write.csv(sumsleep_1min_agg, here::here("data-clean", "Aggregated1minSleep_cleaned.csv"))
write.csv(sleep_daysum, here::here("data-clean", "DaySleepSummary_cleaned.csv"))



## Summarize the average probability of movement per group
ggplot(sumsleep_1min_agg, aes(x = t_norm, y = mean_interp, group = ID, color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.3, size = 1, n = 200) + theme_bw() + 
  geom_smooth(aes(x = t_norm, y = mean_interp, group = group, color = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Bedtime", "Waketime")) + 
  xlab("") + ylab("Probability of Movement at Time t") + 
  theme(text = element_text(size = 16))



## Order weekday variable 
sleep_daysum$dayofweek <- ordered(factor(sleep_daysum$dayofweek), 
                                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday", "Sunday"))

sleep_daysum$school_night <- ifelse(sleep_daysum$dayofweek %in% c("Friday", "Saturday"), "No", 
                                    ifelse(sleep_daysum$month %in% c("June", "July", "August"), "No", "Yes"))

ggplot(sleep_daysum, aes(x = dayofweek, y = prop_asleep, fill = as.character(group))) + 
  geom_boxplot(alpha = 0.7) + xlab("Day of Week") + ylab("Proportion of Time Asleep") + 
  theme_bw() + labs(fill = "") +
  theme(text = element_text(size = 16), legend.position = "bottom") + 
  stat_compare_means(aes(group = group), vjust = 0.5, hide.ns = T, size = 8,
                     label = "p.signif", method = "wilcox.test", paired = FALSE) + 
  geom_hline(yintercept = 0.33, color = "red", linetype = "dashed", size = 1.5)

ggplot(sleep_daysum, aes(x = school_night, y = prop_asleep, fill = as.character(group))) + 
  geom_boxplot(alpha = 0.7) + xlab("School Night") + ylab("Proportion of Time Asleep") + 
  theme_bw() + labs(fill = "") +
  theme(text = element_text(size = 16), legend.position = "bottom") + 
  stat_compare_means(aes(group = group), vjust = 0.5, hide.ns = T, size = 8,
                     label = "p.signif", method = "wilcox.test", paired = FALSE) + 
  geom_hline(yintercept = 0.33, color = "red", linetype = "dashed", size = 1.5)

wilcox.test(prop_asleep~as.character(group), data = sleep_daysum)
summary(sleep_daysum$prop_asleep[sleep_daysum$group == 1])*24

summary(lm(asleep_time ~ group, data = sleep_daysum[sleep_daysum$school_night == "No",]))




