#########################################
# Demographics Data Cleaning
# 
# PI : Samantha Bothwell
#      Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : September 16th, 2024
#########################################

rm(list = ls())

## Libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)


## Load full LTE dataset
full <- read_csv(here::here("data-raw", "LTE_FullDATA_03172026.csv")) %>% 
  group_by(pid) %>% 
  fill(group, .direction = "downup") %>% 
  ungroup()
## Load cleaned activity data
sumdata_day <- read_csv(here::here("data-clean", "Nonaggregated1min_cleaned.csv"))
mvpa <- read_csv(here::here("data-clean", "LTE_METSandMVPA.csv"))



#####
## Clean demographic table variables 
#####
dems_clean <- full %>% 
  # filter to only screening arm for demographics
  filter(redcap_event_name == "screening_arm_1") %>% 
  # Remove non-participant records 
  filter(!is.na(group)) %>% filter(!grepl("SURVEYS", pid, ignore.case = T)) %>% 
  # Remove exclusion IDs who dropped before screening
  filter(!(pid %in% c("z517", "z565"))) %>% 
  # remove "z" from IDs that do have activity data
  mutate(pid = gsub("z", "", pid)) %>% 
  # Filter to only IDs within the activity summary data
  mutate(pid = as.character(pid)) %>% 
  filter(pid %in% sumdata_day$ID) %>% 
  # Recode demographic variables 
  mutate(group = factor(group, levels = c(1, 0, 2), labels = c("KS Case", "Non-KS Control", "P-Value")), 
         nih_ethnicity_race = case_when(race_eth_new___4 == 1 ~ "Hispanic/Latinx", 
                                        race_eth_new___1 + race_eth_new___2 + race_eth_new___3 + race_eth_new___5 + 
                                          race_eth_new___6 + race_eth_new___7 > 1 ~ "More than one race", 
                                        race_eth_new___7 == 1 ~ "White Non-Hispanic/Latinx", 
                                        race_eth_new___1 == 1 ~ "American Indian or Alaska Native", 
                                        race_eth_new___2 == 1 ~ "Asian", 
                                        race_eth_new___3 == 1 ~ "Black or African American", 
                                        race_eth_new___5 == 1 ~ "Middle Eastern or North African", 
                                        race_eth_new___6 == 1 ~ "Native Hawaiian or Pacific Islander", 
                                        .default = "Unknown")) %>% 
  mutate(vo2_max_ml_kg_min = as.numeric(vo2_max_ml_kg_min)) %>% 
  # Select variables we're interested in 
  dplyr::select(pid, calc_age, group, nih_ethnicity_race, pe_wt, pe_ht, pe_bmi, vo2_max_ml_kg_min, totalsedentarytimem,
                ad_date, ad_time, ad_q7, fat_ad_t, fat_ped_t, fat_pp_t, sleep_ped_t, sleep_ad_t, sleep_pp_t)


## Categorize time period of data collection
act_summary <- sumdata_day %>% 
  # Determine if collection was school-year or summer
  mutate(month = month(date), 
         season = ifelse(month %in% c(1:5, 9:12), "School Year", "Summer")) %>% 
  # summarize # of days of wear and % wear time
  group_by(ID) %>% 
  summarise(season = season[1], 
            n_days = length(unique(date)), 
            percent_wear = (n()/(n_days*1440))*100)
  
## Add variables to dems_clean
dems_clean <- merge(dems_clean, act_summary, by.x = "pid", by.y = "ID")

write_csv(dems_clean, here::here("data-clean", "identifiable", "Demographics.csv"))



## Clean self reported activity
sr_act <- full %>% 
  # Select variables we're interested in 
  dplyr::select(pid, ad_date, ad_q7) %>% 
  filter(!is.na(ad_q7)) %>% 
  filter(pid %in% dems_clean$pid) %>% 
  mutate(ad_q7 = ifelse(ad_q7 == 1, "Yes", "No")) %>% 
  rename(selfreport_exercise = ad_q7)
sr_act$group <- dems_clean$group[match(sr_act$pid, dems_clean$pid)]


mvpa <- merge(mvpa, sr_act, by.x = c("ID", "date"), by.y = c("pid", "ad_date"))

ggplot(mvpa, aes(y = Avg_MVPA_Percent, x = group, fill = selfreport_exercise)) + 
  geom_boxplot() + 
  theme_bw(base_size = 16) + 
  labs(x = "", y = "Daily % MVPA", fill = "Self Reported \nDaily Exercise") + 
  theme(legend.position = c(0.15, 0.8))

library(nlme)

model_cs <- lme(
  Avg_MVPA_Percent ~ group * selfreport_exercise,
  random = ~1 | ID,
  correlation = corCompSymm(form = ~ 1 | ID),
  data = mvpa
)

model_cs <- lme(
  Avg_MVPA_Percent ~ selfreport_exercise,
  random = ~1 | ID,
  correlation = corCompSymm(form = ~ 1 | ID),
  data = mvpa[mvpa$group == "KS Case",]
)


summary(model_cs)



#####
## Clean sleep data
#####
dems_sleep <- full %>% 
  # Select variables of interest 
  dplyr::select(pid, redcap_event_name, vc_date, vc_age, avg_get_up_time, avg_bedtime, avg_total_sleep_time, 
                contains("sleep_ped_"), contains("sleep_ad_"), contains("sleep_pp_"), group) %>% 
  # Fill all variables
  group_by(pid) %>% 
  fill(c(avg_get_up_time, avg_bedtime, avg_total_sleep_time), .direction = "downup") %>% 
  ungroup() %>% 
  # filter to visits 1 and 2
  filter(redcap_event_name %in% c("visit_1_arm_1", "visit_2_arm_1")) %>% 
  filter(!is.na(vc_date)) %>% 
  # Clean bed time 
  mutate(avg_get_up_time = strptime(avg_get_up_time, format="%H:%M:%S"),
         getup_seconds = as.numeric(avg_get_up_time$hour) * 3600 + as.numeric(avg_get_up_time$min) * 60 + 
           as.numeric(avg_get_up_time$sec),
         getup_hours = as.numeric(avg_get_up_time$hour) + as.numeric(avg_get_up_time$min)/60 + 
           as.numeric(avg_get_up_time$sec)/3600,
         avg_bedtime = strptime(avg_bedtime, format="%H:%M:%S"),
         bedtime_seconds = as.numeric(avg_bedtime$hour) * 3600 + as.numeric(avg_bedtime$min) * 60 + 
           as.numeric(avg_bedtime$sec),
         bedtime_hours = as.numeric(avg_bedtime$hour) + as.numeric(avg_bedtime$min)/60 + 
           as.numeric(avg_bedtime$sec)/3600,
         bedtime_hours = ifelse(bedtime_hours < 4, 24 + bedtime_hours, bedtime_hours),
         avg_total_sleep_time = strptime(avg_total_sleep_time, format="%H:%M:%S"),
         sleeptime_seconds = as.numeric(avg_total_sleep_time$hour) * 3600 + 
           as.numeric(avg_total_sleep_time$min) * 60 + 
           as.numeric(avg_total_sleep_time$sec),
         sleeptime_hours = as.numeric(avg_total_sleep_time$hour) + 
           as.numeric(avg_total_sleep_time$min)/60 + 
           as.numeric(avg_total_sleep_time$sec)/3600) %>% 
  # Categorize group
  mutate(group = factor(group, levels = c(1, 0), labels = c("KS Case", "Non-KS Control")))

write_csv(dems_sleep, here::here("data-clean", "identifiable", "Sleep.csv"))



###### Format Continuous Values in tables
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=1), 
       c("", "Median [IQR]"=sprintf("%s [%s, %s]", round(quantile(x, 0.5, na.rm = T), 1), 
                                    round(quantile(x, 0.25, na.rm = T), 1), 
                                    round(quantile(x, 0.75, na.rm = T), 1))))
}

# Format table
rndr_tb1 <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- dems_clean[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    p <- NULL
    if (is.numeric(y) | is.integer(y)) {
      try(p <- kruskal.test(y ~ dems_clean$group)$p.value)
    } else {
      try(p <- fisher.test(table(y, droplevels(dems_clean$group)), simulate.p.value = TRUE)$p.value)
    }
    if (is.null(p)){
      s <- "-"
    }else{
      s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    }
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}


table1(~ calc_age + nih_ethnicity_race + pe_wt + pe_ht + pe_bmi| group, 
       data = dems_clean, render.continuous = my.render.cont, render = rndr_tb1, 
       droplevels = FALSE, overall = c(left = "Total"))

