####################################################
# LTE Wearables Data Cleaning 
# 
# PI : Karli Swenson and Shanlee Davis
#
# Analyst : Samantha Bothwell
#
# Date Modified : August 12th, 2024
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
library(svMisc)

## Get names of all datasets 
# path <- "/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/15sEpochs/"
path <- "~/Desktop/Wearable Device Data/Data/15sEpochs/"
files <- list.files(path)

## Demographic data 
#dems <- read.csv("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/LTE212860Interrogati_DATA_2025-01-08_0859.csv") %>% 
#  filter(!is.na(group))# %>% 
  #mutate(pid = ifelse(grepl("z", pid), substr(pid, 2, 4), pid))
dems <- read.csv("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/Bothwell - PA and Sleep in LTE/Wearable Device Data/Data/LTE212860Interrogati_DATA_2025-11-20_1034.csv") %>% 
  filter(!is.na(group)) %>% 
  mutate(pid = ifelse(grepl("z", pid), substr(pid, 2, 4), pid))

## Loop over data and save summarized data 
sumdata_1min <- data.frame(matrix(NA, ncol = 4))
names(sumdata_1min) = c("minute", "Avg_StepCount", "index", "ID")
#names(sumdata_1min) = c("minute", "season", "Avg_StepCount", "index", "ID")
sumdata_10min <- data.frame(matrix(NA, ncol = 4))
names(sumdata_10min) = c("minute", "Avg_StepCount", "index", "ID")
#names(sumdata_10min) = c("minute", "season", "Avg_StepCount", "index", "ID")
sumdata_30min <- data.frame(matrix(NA, ncol = 4))
names(sumdata_30min) = c("minute", "Avg_StepCount", "index", "ID")


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
  pt_minute <- pt_dat %>% 
    # Save minute and date
    mutate(minute = substr(time, 12, 16), 
           date = substr(time, 1, 10)) %>%
    # Sum over minute
    group_by(minute, date)  %>%  # , season) %>% 
    summarise(StepCount = sum(StepCount, na.rm = T),
              ActivityScore = sum(Activity.Score..MET.s., na.rm = T)) %>% 
    ungroup() %>%
    # Mean across days 
    group_by(minute) %>%  # , season) %>% 
    summarise(Avg_StepCount = mean(StepCount, na.rm = T),
              Avg_ActivityScore = mean(ActivityScore, na.rm = T)) %>%
    ungroup() %>% 
    # Filter out missing minute
    #filter(minute != "") %>% 
    # Save index
    mutate(index = 1:length(minute)) %>% 
    # List ID 
    mutate(ID = id)
  
  ## Summarize 10 minute level data
  pt_10minutes <- pt_dat %>% 
    # Save 10 minutes and date
    mutate(minute = substr(time, 12, 15), 
           date = substr(time, 1, 10)) %>%
    # Sum over 10 minute period
    group_by(minute, date) %>%  # , season) %>% 
    summarise(StepCount = sum(StepCount, na.rm = T)) %>% 
    ungroup() %>% 
    # Mean across days 
    group_by(minute) %>% # , season) %>% 
    summarise(Avg_StepCount = mean(StepCount, na.rm = T)) %>%
    ungroup() %>% 
    # Filter out missing minute
    #filter(minute != "") %>% 
    # Save index
    mutate(index = 1:length(minute)) %>% 
    # List ID 
    mutate(ID = id)
  
  
  ## Summarize 30 minute level data
  pt_30minutes <- pt_dat %>% 
    # Save 30 minutes and date
    mutate(minute = substr(time, 12, 15), 
           date = substr(time, 1, 10), 
           min30 = as.numeric(substr(minute, 1, 2))*60 + as.numeric(substr(minute, 4, 4))*10, 
           min30 = floor(min30/30)) %>%
    # Sum over 10 minute period
    group_by(min30, date) %>%  # , season) %>% 
    summarise(StepCount = sum(StepCount, na.rm = T)) %>% 
    ungroup() %>% 
    # Mean across days 
    group_by(min30) %>% # , season) %>% 
    summarise(Avg_StepCount = mean(StepCount, na.rm = T)) %>%
    ungroup() %>% 
    # Filter out missing minute
    #filter(minute != "") %>% 
    # Save index
    mutate(index = 1:length(min30)) %>% 
    # List ID 
    mutate(ID = id)
  
  ## Save data
  sumdata_1min <- data.frame(rbind(sumdata_1min, pt_minute))
  sumdata_10min <- data.frame(rbind(sumdata_10min, pt_10minutes))
  sumdata_30min <- data.frame(rbind(sumdata_30min, pt_30minutes))
  
  #progress(i, max.value = length(files))
  
}

## Clean up datasets
sumdata_1min <- sumdata_1min %>% filter(!is.na(minute))
sumdata_10min <- sumdata_10min %>% filter(!is.na(minute))


ggplot(sumdata_1min[sumdata_1min$ID == "501",], aes(x = index, y = Avg_StepCount)) + 
  geom_point(alpha = 0.3) +
  geom_path(color = "blue") + 
  # geom_smooth(geom="line", method = "loess", size = 2, fullrange = F, color = "blue") +
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  theme_bw() + 
  xlab("") + ylab("Average Step Count (Per Minute)") + labs(color = "") + 
  theme(text = element_text(size = 16), 
        legend.position = "bottom") + ylim(0, 30)

## Add group
sumdata_1min$group <- dems$group[match(sumdata_1min$ID, dems$pid)]
sumdata_10min$group <- dems$group[match(sumdata_10min$ID, dems$pid)]
sumdata_1min$group <- factor(ifelse(sumdata_1min$group == 1, "XXY", "Control"))
#sumdata_10min$group <- factor(ifelse(sumdata_10min$group == 1, "KS Case", "Non-KS Control"))

sumdata_1min <- sumdata_1min[!is.na(sumdata_1min$group),]


## Plots
ggplot(sumdata_1min, aes(x = index, y = Avg_StepCount, group = ID, color = group)) + 
  #geom_line(, alpha = 0.3) + 
  stat_smooth(geom="line", method = "loess", alpha = 0.3, size = 1, fullrange = F) + 
  theme_bw() + 
  geom_smooth(aes(x = index, y = Avg_StepCount, group = group, color = group), size = 2) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average Step Count (Per Minute)") + labs(color = "") + 
  theme(text = element_text(size = 16), 
        legend.position = "bottom") # + ylim(0, 30)

daily_sum <- sumdata_1min %>% 
  group_by(ID) %>% 
  summarise(group = group, Avg_StepCount = sum(Avg_StepCount, na.rm = T)) %>% 
  slice(1)

p <- t.test(Avg_StepCount ~ group, data = daily_sum)$p.value
p <- ifelse(p < 0.001, "p < 0.001", paste("p =", round(p, 3)))


daily_sum$group <- ordered(factor(daily_sum$group), levels = c("XXY", "Control"))
ggplot(daily_sum, aes(x = group, y = Avg_StepCount, color = group)) + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("") + ylab("Average Daily Step Count") + 
  labs(color = "") + 
  scale_color_manual(values = c("green4", "grey40")) +
  theme(text = element_text(size = 22), 
        legend.position = "none") + 
  annotate("richtext", x = 1.5, y = 16000, label = paste("<b>", p, "</b>"), size = 7, 
           fill = NA, label.color = NA)


ggplot(sumdata_10min, aes(x = index, y = Avg_StepCount, group = ID, color = group)) + 
  stat_smooth(geom="line", se = F, alpha = 0.3, size = 1) + 
  #geom_smooth(method="loess", se = F, alpha = 0.3, size = 1) + 
  theme_bw() + 
  geom_smooth(aes(x = index, y = Avg_StepCount, group = group, color = group), size = 2) +
  scale_x_continuous(breaks = c(0, 20, 38, 56, 74, 92, 110, 128, 145), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average Step Count (Per 10 Minutes)") +
  theme(text = element_text(size = 16)) + ylim(0, 150)

sumdata_1min <- sumdata_1min[!(sumdata_1min$ID %in% c(521, 522, 525, 560, 566, 567, 569, 570, 571, 573)),]




############################
#### FDA 
############################
## Make Wide datasets
long_1min <- sumdata_1min[,c("ID", "Avg_StepCount", "index")] %>% 
  pivot_wider(names_from = index, values_from = Avg_StepCount)
long_10min <- sumdata_10min[,c("ID", "Avg_StepCount", "index")] %>% 
  pivot_wider(names_from = index, values_from = Avg_StepCount)

sumdata_1min$sind <- rep(seq(0,1,len=1441), length(unique(sumdata_1min$ID)))
sumdata_10min$sind <- rep(seq(0,1,len=145), length(unique(sumdata_1min$ID)))

#####fit the model output -- with the smoothed data 
#sumdata_1min <- sumdata_1min %>% filter(season == "Summer")

fit_1min <- bam(Avg_StepCount ~ s(sind, bs="cc", k = 30) + 
                  s(sind, by = group, bs="cc", k = 30), data=sumdata_1min,
             method="fREML")
summary(fit_1min)

# Save residuals
resid_dat <- data.frame(cbind(".id" = sumdata_1min$ID, 
                              ".index" = sumdata_1min$index, 
                              ".value" = fit_1min$residuals))

## Convert all columns to numeric 
resid_dat <- sapply( resid_dat, as.numeric )
resid_dat <- data.frame(resid_dat)


# Extract eigenfunctions 
# step_mat <- as.matrix(step_wide[,-c(1:4)])
resid_fpca <- fpca.sc(ydata = resid_dat)

resid_fpca_Y <- data.frame(resid_fpca$Yhat) %>% 
  pivot_longer(everything(), names_to = "index", values_to = "Avg_StepCount") %>% 
  mutate(Avg_StepCount = ifelse(Avg_StepCount < 0, 0, Avg_StepCount), 
         index = sapply(strsplit(index,"X"), `[`, 2), 
         index = as.numeric(index), id = sumdata_1min$ID, 
         group = sumdata_1min$group)

## Plots
ggplot(resid_fpca_Y, aes(x = index, y = Avg_StepCount, group = id, color = group)) + 
  geom_line(alpha = 0.3, size = 1) + 
  #stat_smooth(geom="line", method = "loess", alpha = 0.3, size = 1, fullrange = F) + 
  theme_bw() + 
  geom_smooth(data = sumdata_1min, aes(x = index, y = Avg_StepCount, group = group, color = group), size = 2, n = 100) + 
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1442), 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + ylab("Average Step Count (Per Minute)") + labs(color = "") + 
  theme(text = element_text(size = 16), 
        legend.position = "bottom")


Phi_hat <- resid_fpca$efunctions

refund.shiny::plot_shiny(resid_fpca) # Plot to see eigenfunctions

# Include first 4 eigenfunctions in data
N = length(unique(sumdata_1min$ID))
sumdata_1min$Phi1 <- rep(Phi_hat[,1], N)
sumdata_1min$Phi2 <- rep(Phi_hat[,2], N)
sumdata_1min$Phi3 <- rep(Phi_hat[,3], N)

sumdata_1min$ID <- as.numeric(sumdata_1min$ID )

# Random functional intercept model
fit_1min_rfi <- bam(Avg_StepCount ~ s(sind, bs="cc", k = 10) + 
                  s(sind, by = group, bs="cc", k = 10) + 
                  #s(sind, by = factor(season), bs="cc", k = 30) + 
                  s(ID, by = Phi1, bs="cc", k=10) + 
                  s(ID, by = Phi2, bs="cc", k=10) + 
                  s(ID, by = Phi3, bs="cc", k=10), 
                  data=sumdata_1min, method="fREML", discrete = TRUE)

rfi_summary <- data.frame(summary(fit_1min_rfi)$s.table)

## Prediction data
df_pred <- data.frame(sind = sumdata_1min$sind[1:1441], ID = sumdata_1min$ID[1],
                      group = 1, 
                      #season = factor("School-Year", levels = levels(sumdata_1min$season)),
                      Phi1 = 0, Phi2 = 0, Phi3 = 0)

fhat_rfi   <- predict(fit_1min_rfi, newdata=df_pred, se.fit=TRUE,type='terms')

## Save estimates
group_hat <- fhat_rfi$fit[,2]; group_se <- fhat_rfi$se.fit[,2]
rfi_ests <- data.frame(group_hat, 
                       group_low = group_hat - 1.96*group_se, #*(sqrt(length(unique(sumdata_1min$ID)))), 
                       group_high = group_hat + 1.96*group_se, #*(sqrt(length(unique(sumdata_1min$ID)))), 
                       sind = seq(1, 1441, by = 1)/1441)

# Make plots 
ggplot(rfi_ests, aes(x = sind, y = group_hat)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_ribbon(aes(x = sind, ymin = group_low, ymax = group_high), fill = "blue", alpha = 0.2) +
  geom_line(size = 1)  + 
  ylab("Difference in \nAcitivity Count") + 
  ggtitle("") + theme_bw() +
  scale_x_continuous(breaks = c(0, 182, 362, 542, 722, 902, 1082, 1262, 1441)/1441, 
                     labels = c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", 
                                "9 pm", "Midnight")) + 
  xlab("") + theme(text = element_text(size = 25))


##there are 360 5 second intervals in 30 minutes
xinx_1min <- c(0, 182, 362, 542, 722, 902, 1082, 1262, 1441)/1441
xinx_lab_1min <- c("Midnight", "3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", "9 pm", "Midnight")
plot(fit_1min, xlab="Time Before Birth", xaxt='n',
     ylab=expression(hat(gamma)(s)), main="")
axis(1,at=xinx_1min, xinx_lab_1min)




odds_ratio <- function(pA, pB, nA, nB){
  events_A = round(pA*nA, 0)
  events_B = round(pB*nB, 0)
  or = (events_B/(nB - events_B))/(events_A/(nA - events_A))
  return(or)
}





