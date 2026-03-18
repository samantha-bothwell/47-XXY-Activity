
rm(list = ls())

## Libraries 
library(readxl)
library(janitor)
library(tidyverse)
library(refund)
library(mgcv)
library(refund.shiny)
library(svMisc)
library(hrbrthemes)

## Get names of all datasets 
path <- "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/15sEpochs/"
#path <- "~/Desktop/Wearable Device Data/Data/15sEpochs/"
files <- list.files(path)

## Demographic data 
#dems <- read.csv("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/LTE212860Interrogati_DATA_2025-01-08_0859.csv") %>% 
#  filter(!is.na(group))# %>% 
#mutate(pid = ifelse(grepl("z", pid), substr(pid, 2, 4), pid))
dems <- read.csv("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/LTE212860Interrogati_DATA_2025-11-20_1034.csv") %>% 
  filter(!is.na(group)) %>% 
  mutate(pid = ifelse(grepl("z", pid), substr(pid, 2, 4), pid))

## Loop over data and save summarized data 
sumdata_mvpa <- data.frame(matrix(NA, ncol = 8))
names(sumdata_mvpa) = c("date", "Total_METS_per_mins", "Total_METS_per_30mins", "Total_METS_per_hour", 
                        "MVPA_Time", "n", "index", "ID")


for(i in 1:length(files)){
  ## load data 
  pt_dat <- read.csv(paste0(path, files[i]))
  
  ## Save id 
  id <- substr(files[i], 1, 3)
  
  ## Convert all columns to numeric 
  pt_dat <- sapply( pt_dat, as.numeric )
  
  ## clean up date
  pt_dat <- data.frame(pt_dat) %>% 
    mutate(time = excel_numeric_to_date(time, include_time = TRUE)) %>% 
    mutate(month = as.numeric(substr(time, 6, 7)), 
           month = min(month, na.rm = T),
           season = factor(month, levels = c(1:12), 
                           labels = c(rep("School-Year", 5), rep("Summer", 3), rep("School-Year", 4))))
  
  ## Summarize minute level data across days 
  pt_mvpa <- pt_dat %>% 
    # Filter to wear time 
    filter(Nonwear.Time..s. == 0) %>% 
    # Save minute and date
    mutate(minute = substr(time, 12, 16), 
           date = substr(time, 1, 10)) %>%
    # Sum over minute
    group_by(minute, date)  %>%  # , season) %>% 
    summarise(StepCount = sum(StepCount, na.rm = T),
              ActivityScore = sum(Activity.Score..MET.s., na.rm = T), 
              METS = ActivityScore/(n()*15)) %>% 
    ungroup() %>%
    # get time in MVPA per day 
    group_by(date) %>%  # , season) %>% 
    #mutate(METS = METS*(1441/n())) %>% 
    summarise(Total_METS_per_mins = sum(METS, na.rm = T), 
              Total_METS_per_30mins = sum(METS, na.rm = T)/30,
              Total_METS_per_hour = sum(METS, na.rm = T)/60,
              MVPA_Time = sum(METS>=3, na.rm = T), 
              n = n()) %>%
    ungroup() %>% 
    # Require wear at least 50% of the time 
    filter(n > 1440*0.5) %>% 
    # Get one value for average MVPA
    #filter(minute != "") %>% 
    # Save index
    mutate(index = 1:length(minute)) %>% 
    # List ID 
    mutate(ID = id)
  
  
  ## Save data
  sumdata_mvpa <- data.frame(rbind(sumdata_mvpa, pt_mvpa))
  
}

sumdata_mvpa <- sumdata_mvpa %>% filter(!is.na(ID))

#sumdata_mvpa$avg_daily_mets <- sumdata_mvpa$Avg_mins_METS/1440

sumdata_mvpa$Avg_MVPA_Percent <- (sumdata_mvpa$MVPA_Time/sumdata_mvpa$n)*100

write.csv(sumdata_mvpa, "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/LTE Cardiorespiratory/Data/LTE_METSandMVPA_11202025.csv")



summary(sumdata_mvpa$Avg_30mins_METS)


## Add group
sumdata_mvpa$group <- dems$group[match(sumdata_mvpa$ID, dems$pid)]
sumdata_mvpa$group <- factor(ifelse(sumdata_mvpa$group == 1, "XXY", "Control"))

summary(sumdata_mvpa$Avg_30mins_METS[sumdata_mvpa$group == "Control"])


ggplot(sumdata_mvpa, aes(y = Avg_MVPA, x = group, fill = group)) + 
  geom_boxplot() + theme_minimal() + xlab("") + 
  ylab("Average daily MVPA (mins/day)") + theme(text = element_text(size = 16)) + 
  scale_fill_manual(values = c("grey50", "goldenrod"))


t.test(Avg_MVPA ~ group, data = sumdata_mvpa)






