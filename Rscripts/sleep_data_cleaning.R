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

## Get names of all datasets 
epochs_dir <- here::here("data-raw", "Sleep")
files <- list.files(epochs_dir)

## Demographic data 
dems <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv")) %>% 
  # filter out IDs with a missing group
  filter(!is.na(group)) %>%
  # filter out IDs beginning with 'z'
  filter(!(grepl("z", pid)))



############ Data Processing 
## Initialize results dataframes
res_day <- vector("list", length(files))
res_1min <- vector("list", length(files))


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
  
  
  ## Manual exclusion of IDs 557 and 570 for lack of usable data
  ## If there are no valid rows, skip
  if (id %in% c("557", "570")) {
    warning("No valid data for ID ", id)
    next
  }
  
  ## Day summaries 
  pt_day <- pt_dat %>% 
    group_by(date) %>% 
    summarise(#asleep_time = length(Sleep.Wake[Sleep.Wake == 0] == TRUE),
              #awake_time = length(Sleep.Wake[Sleep.Wake == 1] == TRUE), 
              asleep_time = length(interval_status[interval_status == "REST-S"] == TRUE),
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
  pt_minute <- pt_dat %>% 
    # Save minute and date
    mutate(#time = format(strptime(time, "%H:%M:%S %p"), format = "%H:%M:%S"), 
           minute = as.numeric(substr(time, 1, 2))*60 + as.numeric(substr(time, 4, 5))) %>%
    # Filter to days in the summary dataset 
    mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
    filter(date %in% pt_day$date) %>% 
    # Fill in missing minutes 
    group_by(date) %>% 
    complete(minute = 0:1439) %>%
    # Mean across days 
    group_by(minute) %>% 
    summarise(avg_white_light = mean(white_light, na.rm = T), 
              avg_red_light = mean(red_light, na.rm = T),
              avg_green_light = mean(green_light, na.rm = T),
              avg_blue_light = mean(blue_light, na.rm = T)) %>%
    ungroup() %>% 
    # Save index
    mutate(index = 1:length(minute)) %>% 
    # List ID 
    mutate(ID = id)
  
  ## Save participant results
  res_day[[i]] <- pt_day
  res_1min[[i]] <- pt_minute

}


## Bind all the results
sleep_daysum <- dplyr::bind_rows(res_day)
sumsleep_1min <- dplyr::bind_rows(res_1min)


## Add group
sumsleep_1min$group <- dems$group[match(sumsleep_1min$ID, dems$pid)]
sumsleep_1min$group <- ifelse(sumsleep_1min$group == 1, "Case (KS)", "Control")
sleep_daysum$group <- dems$group[match(sleep_daysum$ID, dems$pid)]
sleep_daysum$group <- ifelse(sleep_daysum$group == 1, "Case (KS)", "Control")

## Remove the ids that were removed from the study (should be 501, 512, 514, 517, 518, and 565)
sumsleep_1min <- sumsleep_1min %>% filter(!is.na(group))
sleep_daysum <- sleep_daysum %>% filter(!is.na(group))


## Save files
write.csv(sumsleep_1min, here::here("data-clean", "Aggregated1minSleep_cleaned.csv"))
write.csv(sleep_daysum, here::here("data-clean", "DaySleepSummary_cleaned.csv"))



## Summarize the average time asleep per group (Split by Weekday and Weekend)
ggplot(sumsleep_1min, aes(x = index, y = Avg_WhiteLight, group = ID, color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.3, size = 1) + theme_bw() + 
  geom_smooth(aes(x = index, y = Avg_WhiteLight, group = group, color = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average White Light (Per Minute)") + 
  theme(text = element_text(size = 16))

ggplot(sumsleep_1min, aes(x = index, y = Avg_RedLight, group = ID, color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.3, size = 1) + theme_bw() + 
  geom_smooth(aes(x = index, y = Avg_RedLight, group = group, color = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average Red Light (Per Minute)") + 
  theme(text = element_text(size = 16))

ggplot(sumsleep_1min, aes(x = index, y = Avg_GreenLight, group = ID, color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.3, size = 1) + theme_bw() + 
  geom_smooth(aes(x = index, y = Avg_GreenLight, group = group, color = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average Green Light (Per Minute)") + 
  theme(text = element_text(size = 16))

ggplot(sumsleep_1min, aes(x = index, y = Avg_BlueLight, group = ID, color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.3, size = 1) + theme_bw() + 
  geom_smooth(aes(x = index, y = Avg_BlueLight, group = group, color = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average Blue Light (Per Minute)") + 
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

 
## Index from 0 to 1
sumsleep_1min$sind <- rep(seq(0,1,len=1440), length(unique(sumsleep_1min$ID)))

#####fit the models output -- with the smoothed data 
#sumsleep_1min$group <- as.factor(sumsleep_1min$group)
fit_white <- bam(Avg_WhiteLight ~ s(sind, bs="cc") + s(sind, by = group, bs="cc"), 
               data=sumsleep_1min, method="fREML")
fit_red <- bam(Avg_RedLight ~ s(sind, bs="cc") + s(sind, by = group, bs="cc"), 
               data=sumsleep_1min, method="fREML")
fit_blue <- bam(Avg_BlueLight ~ s(sind, bs="cc") + s(sind, by = group, bs="cc"), 
               data=sumsleep_1min, method="fREML")
fit_green <- bam(Avg_GreenLight ~ s(sind, bs="cc") + s(sind, by = group, bs="cc"), 
                data=sumsleep_1min, method="fREML")


# Save residuals
resid_white <- data.frame(cbind(".id" = sumsleep_1min$ID, ".index" = sumsleep_1min$index, 
                                ".value" = fit_red$residuals))
resid_red <- data.frame(cbind(".id" = sumsleep_1min$ID, ".index" = sumsleep_1min$index, 
                              ".value" = fit_red$residuals))
resid_blue <- data.frame(cbind(".id" = sumsleep_1min$ID, ".index" = sumsleep_1min$index, 
                              ".value" = fit_blue$residuals))
resid_green <- data.frame(cbind(".id" = sumsleep_1min$ID, ".index" = sumsleep_1min$index, 
                               ".value" = fit_green$residuals))


## Convert all columns to numeric 
resid_white <- sapply( resid_white, as.numeric ); resid_white <- data.frame(resid_white)
resid_red <- sapply( resid_red, as.numeric ); resid_red <- data.frame(resid_red)
resid_blue <- sapply( resid_blue, as.numeric ); resid_blue <- data.frame(resid_blue)
resid_green <- sapply( resid_green, as.numeric ); resid_green <- data.frame(resid_green)


# Extract eigenfunctions 
resid_fpca_white <- fpca.sc(ydata = resid_white); Phi_hat_white <- resid_fpca_white$efunctions
resid_fpca_red <- fpca.sc(ydata = resid_red); Phi_hat_red <- resid_fpca_red$efunctions
resid_fpca_blue <- fpca.sc(ydata = resid_blue); Phi_hat_blue <- resid_fpca_blue$efunctions
resid_fpca_green <- fpca.sc(ydata = resid_green); Phi_hat_green <- resid_fpca_green$efunctions

# refund.shiny::plot_shiny(resid_fpca_white) # Plot to see eigenfunctions

# Include first 4 eigenfunctions in data
N = length(unique(sumsleep_1min$ID))
sumsleep_1min$Phi1_white <- rep(Phi_hat_white[,1], N); sumsleep_1min$Phi2_white <- rep(Phi_hat_white[,2], N)
sumsleep_1min$Phi3_white <- rep(Phi_hat_white[,3], N)
sumsleep_1min$Phi1_red <- rep(Phi_hat_red[,1], N); sumsleep_1min$Phi2_red <- rep(Phi_hat_red[,2], N)
sumsleep_1min$Phi3_red <- rep(Phi_hat_red[,3], N)
sumsleep_1min$Phi1_blue <- rep(Phi_hat_blue[,1], N); sumsleep_1min$Phi2_blue <- rep(Phi_hat_blue[,2], N)
sumsleep_1min$Phi3_blue <- rep(Phi_hat_blue[,3], N)
sumsleep_1min$Phi1_green <- rep(Phi_hat_green[,1], N); sumsleep_1min$Phi2_green <- rep(Phi_hat_green[,2], N)
sumsleep_1min$Phi3_green <- rep(Phi_hat_green[,3], N)

sumsleep_1min$ID <- as.numeric(sumsleep_1min$ID)

# Random functional intercept models
fit_white_rfi <- bam(Avg_WhiteLight ~ s(sind, bs="cc", k = 30) + s(sind, by = group, bs="cc", k = 30) + 
                     s(ID, by = Phi1_white, bs="cc", k=20) + s(ID, by = Phi2_white, bs="cc", k=20) + 
                     s(ID, by = Phi3_white, bs="cc", k=20), 
                   data=sumsleep_1min, method="fREML", discrete = TRUE)
fit_red_rfi <- bam(Avg_RedLight ~ s(sind, bs="cc", k = 30) + s(sind, by = group, bs="cc", k = 30) + 
                      s(ID, by = Phi1_red, bs="cc", k=20) + s(ID, by = Phi2_red, bs="cc", k=20) + 
                      s(ID, by = Phi3_red, bs="cc", k=20), 
                    data=sumsleep_1min, method="fREML", discrete = TRUE)
fit_blue_rfi <- bam(Avg_BlueLight ~ s(sind, bs="cc", k = 30) + s(sind, by = group, bs="cc", k = 30) + 
                     s(ID, by = Phi1_blue, bs="cc", k=20) + s(ID, by = Phi2_blue, bs="cc", k=20) + 
                     s(ID, by = Phi3_blue, bs="cc", k=20), 
                   data=sumsleep_1min, method="fREML", discrete = TRUE)
fit_green_rfi <- bam(Avg_GreenLight ~ s(sind, bs="cc", k = 30) + s(sind, by = group, bs="cc", k = 30) + 
                      s(ID, by = Phi1_green, bs="cc", k=20) + s(ID, by = Phi2_green, bs="cc", k=20) + 
                      s(ID, by = Phi3_green, bs="cc", k=20), 
                    data=sumsleep_1min, method="fREML", discrete = TRUE)

white_rfi_summary <- data.frame(summary(fit_white_rfi)$s.table)
red_rfi_summary <- data.frame(summary(fit_red_rfi)$s.table)
blue_rfi_summary <- data.frame(summary(fit_blue_rfi)$s.table)
green_rfi_summary <- data.frame(summary(fit_green_rfi)$s.table)

## Prediction data
df_pred <- data.frame(sind = sumsleep_1min$sind[1:1440], ID = sumsleep_1min$ID[1], group = 1,
                      Phi1_white = 0, Phi2_white = 0, Phi3_white = 0, Phi1_red = 0, Phi2_red = 0, 
                      Phi3_red = 0, Phi1_blue = 0, Phi2_blue = 0, Phi3_blue = 0, Phi1_green = 0, 
                      Phi2_green = 0, Phi3_green = 0)

fhat_white_rfi <- predict(fit_white_rfi, newdata=df_pred, se.fit=TRUE,type='terms')
fhat_red_rfi <- predict(fit_red_rfi, newdata=df_pred, se.fit=TRUE,type='terms')
fhat_blue_rfi <- predict(fit_blue_rfi, newdata=df_pred, se.fit=TRUE,type='terms')
fhat_green_rfi <- predict(fit_green_rfi, newdata=df_pred, se.fit=TRUE,type='terms')

## Save estimates
group_hat_white <- fhat_white_rfi$fit[,2]; group_se_white <- fhat_white_rfi$se.fit[,2]
group_hat_red <- fhat_red_rfi$fit[,2]; group_se_red <- fhat_red_rfi$se.fit[,2]
group_hat_blue <- fhat_blue_rfi$fit[,2]; group_se_blue <- fhat_blue_rfi$se.fit[,2]
group_hat_green <- fhat_green_rfi$fit[,2]; group_se_green <- fhat_green_rfi$se.fit[,2]
white_rfi_ests <- data.frame(group_hat_white, 
                             group_low = group_hat_white - 3.5*group_se_white, 
                             group_high = group_hat_white + 3.5*group_se_white, 
                             sind = seq(0, 1439, by = 1)/1440)
red_rfi_ests <- data.frame(group_hat_red, 
                           group_low = group_hat_red - 1.96*group_se_red, 
                           group_high = group_hat_red + 1.96*group_se_red, 
                           sind = seq(0, 1439, by = 1)/1440)
blue_rfi_ests <- data.frame(group_hat_blue, 
                            group_low = group_hat_blue - 3.5*group_se_blue, 
                            group_high = group_hat_blue + 3.5*group_se_blue, 
                            sind = seq(0, 1439, by = 1)/1440)
green_rfi_ests <- data.frame(group_hat_green, 
                             group_low = group_hat_green - 1.96*group_se_green, 
                             group_high = group_hat_green + 1.96*group_se_green, 
                             sind = seq(0, 1439, by = 1)/1440)

# Make plots 
ggplot(white_rfi_ests, aes(x = sind, y = group_hat_white)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), fill = "grey40", alpha = 0.2) +
  geom_line(size = 1, color = "black")  + 
  ylab("Difference in White Light Exposure \nbetween KS Cases and non-KS Controls") + 
  ggtitle("") + theme_bw() +
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1441)/1441, 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + theme(text = element_text(size = 16))

ggplot(blue_rfi_ests, aes(x = sind, y = group_hat_blue)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), fill = "blue", alpha = 0.2) +
  geom_line(size = 1, color = "blue")  + 
  ylab("Difference in Blue Light Exposure \nbetween KS Cases and non-KS Controls") + 
  ggtitle("") + theme_bw() +
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1441)/1441, 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + theme(text = element_text(size = 16))


ggplot(red_rfi_ests, aes(x = sind, y = group_hat_red)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) +
  ## Red Light
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), fill = "red4", alpha = 0.2) +
  geom_line(size = 1, color = "red")  +
  # Blue LIght
  geom_ribbon(data = blue_rfi_ests, aes(x = sind, ymin = group_low, ymax = group_high), fill = "blue4", alpha = 0.2) +
  geom_line(data = blue_rfi_ests, aes(x = sind, y = group_hat_blue), size = 1, color = "blue") +
  # Green Light
  geom_ribbon(data = green_rfi_ests, aes(x = sind, ymin = group_low, ymax = group_high), fill = "green4", alpha = 0.2) +
  geom_line(data = green_rfi_ests, aes(x = sind, y = group_hat_green), size = 1, color = "green")  +
  ## White Light
  geom_ribbon(data = white_rfi_ests, aes(x = sind, ymin = group_low, ymax = group_high), fill = "grey20", alpha = 0.2) +
  geom_line(data = white_rfi_ests, aes(x = sind, y = group_hat_white), size = 1, color = "black")  +
  ylab("Difference in White Light Exposure \nbetween KS Cases and non-KS Controls") + 
  ggtitle("") + theme_bw() +
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1441)/1441, 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + theme(text = element_text(size = 16))





