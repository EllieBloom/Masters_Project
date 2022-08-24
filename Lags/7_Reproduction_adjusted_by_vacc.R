
# R adjusted for vaccination rates ----------------------------------------


# Date started: 23 August 2022



# Setup -------------------------------------------------------------------

library(tidyverse)



# Data --------------------------------------------------------------------

combined_england <- readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_england.rds")
combined_regional <-readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_regional.rds")



# Dates -------------------------------------------------------------------

# Useful dates
lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

react_reprod_start <- as.Date("2020-05-15","%Y-%m-%d")


# R regression ------------------------------------------------------------

combined_england <- combined_england %>% filter(date>=react_reprod_start, date<=lockdown_3_end)

model_england <- lm(r~cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england)
summary(model_england)


combined_england$r_adj_vacc <- summary(model_england)$resid

setwd("~/Desktop/Masters/Project/Analysis/Lags")
write.csv(combined_england, "england_r_adjusted.csv")



# CCF ---------------------------------------------------------------------

### Lockdown 3 with adjusted R --------------------------------------------------

combined_england_lockdown3 <- combined_england %>% filter(date>=lockdown_3_start, date<=lockdown_3_end)

lag_max=100
ccf <- ccf(combined_england_lockdown3$workplaces_av,combined_england_lockdown3$r_adj_vacc,  na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(combined_england_lockdown3)

ccf_data


ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


ccf_plot <- ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  xlim(-50,50) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  theme_light()

ccf_plot

# Peaks at -20 and 39


# DTW with adjusted R -----------------------------------------------------

combined_england_lockdown3 <- combined_england %>% filter(date>=lockdown_3_start, date<=lockdown_3_end)


dtw_lag <- dtw(normalize(combined_england_lockdown3$workplaces_av), normalize(combined_england_lockdown3$r_adj_vacc))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
colnames(dtw_data)<- c("mob_index","r_index")
write.csv(dtw_data,"dtw_indices.csv" )
dtw_plot <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
  geom_line(col="blue") + 
  theme_bw() +
  labs(x="Mobility series index",
       y="R(t) series index")+
  #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
  geom_abline(slope=1, intercept=0, col="red", linetype="dashed")
dtw_plot
