# Lag sliding window

# Date started: 10th June 2022

# Note this normalisation is now across the ENTIRE period... consider whether this is correct?

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(astsa) # Time series package used in datacamp course
library(lubridate) # Using to add months to date
library(lmtest) # For granger causality test

# Functions

min_max_normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Useful dates ------------------------------------------------------------

bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                        "%Y-%m-%d")

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

REACT_start <-as.Date("2020-05-01","%Y-%m-%d")

start_date <- REACT_start
end_date <- lockdown_2_start%m+%months(-1)

# Looking at London workplace mobility from the start of REACT

region_of_interest <- "LONDON"
mobility_of_interest <- "workplaces"
mobility_of_interest_av <- "workplaces_av"


# Reading tibbles ---------------------------------------------------------

# Loading tibbles ---------------------------------------------------------

mobility_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/mobility_tibble_raw.rds")
summary(mobility_ts)

cases_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/cases_tibble.rds")
summary(cases_ts)

prev_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/prev_tibble.rds")
summary(prev_ts)

prev_smooth_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/prev_smooth_tibble.rds")



## Mobility - workplace ----------------------------------------------------


workplace_ts <- mobility_ts %>% filter(region=="LONDON",type_mobility=="workplaces",date>=REACT_start)

workplace_ts$mobility_normalised <- min_max_normalise(workplace_ts$mobility)
summary(workplace_ts$mobility_normalised)

table(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0])
min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0]<-min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

## REACT prevalence --------------------------------------------------------

seq(1,nrow(prev_smooth_ts),10)

prev_smooth_ts_10 <- prev_smooth_ts[seq(1,nrow(prev_smooth_ts),10),]

london_prev_smooth_ts <- prev_smooth_ts_10 %>% filter(region=="LONDON", d_comb>=REACT_start)

# Normalisation
london_prev_smooth_ts$prev_normalised <- min_max_normalise(london_prev_smooth_ts$p)
summary(london_prev_smooth_ts$prev_normalised)

london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised==0]<-min(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised!=0])
min(london_cases_ts$cases_normalised) 


# Official cases ----------------------------------------------------------


london_cases_ts <- cases_ts %>% filter(region=="LONDON", date>=REACT_start)


# Normalisation
london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
summary(london_cases_ts$cases_normalised)

min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) # Looks like this worked







# Dynamic Time warping  - prev vs mobility -------------------------------------

dtw_lag <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=REACT_start & workplace_ts$date<end_date],
                 london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=REACT_start & london_prev_smooth_ts$d_comb<end_date])

dtw_lag$distance
# 64.767 days


## 4-months moving window --------------------------------------------------

difftime(max(london_prev_smooth_ts$d_comb),REACT_start%m+%months(4),units="weeks")

dtw_results_4months <- NA

for (i in seq(1,75,1)){
start_date <- REACT_start%m+%weeks(i-1)
end_date <- REACT_start%m+%months(4)%m+%weeks(i-1)
alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],
                 london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date])
distance <- alignment$distance 
results <- c(start_date, end_date, distance)
dtw_results_4months <- rbind(dtw_results_4months,results)
}

dtw_results_4months <- dtw_results_4months[-1,]
colnames(dtw_results_4months) <- c("start_date", "end_date", "dtw_lag")
dtw_results_4months <- as.data.frame(dtw_results_4months)
dtw_results_4months$start_date <- as.Date(dtw_results_4months$start_date)
dtw_results_4months$end_date <- as.Date(dtw_results_4months$end_date)


dtw_results_4months %>%
  ggplot(aes(x=start_date,y=dtw_lag/7))+
  geom_smooth(se=FALSE)+
  geom_line()+
  labs(y="Dynamic Time Warping lag (Weeks)",
       x="Start date of 4 month window",
       subtitle="Prevalence vs rolling av(mobility) - 1 week movements") +
  ggtitle("Dynamic Time Warping Lag over varying 4 month windows")+
  theme_light()

## 6-months moving window --------------------------------------------------

difftime(max(london_prev_smooth_ts$d_comb),REACT_start%m+%months(6),units="weeks")

dtw_results_6months <- NA

for (i in seq(1,67,1)){
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(6)%m+%weeks(i-1)
  alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],
                   london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date])
  distance <- alignment$distance 
  results <- c(start_date, end_date, distance)
  dtw_results_6months <- rbind(dtw_results_6months,results)
}

dtw_results_6months <- dtw_results_6months[-1,]
colnames(dtw_results_6months) <- c("start_date", "end_date", "dtw_lag")
dtw_results_6months <- as.data.frame(dtw_results_6months)
dtw_results_6months$start_date <- as.Date(dtw_results_6months$start_date)
dtw_results_6months$end_date <- as.Date(dtw_results_6months$end_date)

dtw_results_6months %>%
  ggplot(aes(x=start_date,y=dtw_lag/7))+
  geom_smooth(se=FALSE)+
  geom_line()+
  labs(y="Dynamic Time Warping lag (Weeks)",
       x="Start date of 6 month window",
       subtitle="Prevalence vs rolling av(mobility) - 1 week movements") +
  ggtitle("Dynamic Time Warping Lag over varying 6 month windows")+
  theme_light()


## Logging - 4 month window ----------------------------------------------


dtw_results_4months_log <- NA

for (i in seq(1,75,1)){
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(4)%m+%weeks(i-1)
  alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],
                   log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date]))
  distance <- alignment$distance 
  results <- c(start_date, end_date, distance)
  dtw_results_4months_log <- rbind(dtw_results_4months_log,results)
}

dtw_results_4months_log <- dtw_results_4months_log[-1,]
colnames(dtw_results_4months_log) <- c("start_date", "end_date", "dtw_lag")
dtw_results_4months_log <- as.data.frame(dtw_results_4months_log)
dtw_results_4months_log$start_date <- as.Date(dtw_results_4months_log$start_date)
dtw_results_4months_log$end_date <- as.Date(dtw_results_4months_log$end_date)


dtw_results_4months_log %>%
  ggplot(aes(x=start_date,y=dtw_lag/7))+
  geom_smooth(se=FALSE)+
  geom_line()+
  labs(y="Dynamic Time Warping lag (Weeks)",
       x="Start date of 4 month window",
       subtitle="Log(prevalence) vs rolling av(mobility)") +
  ggtitle("Dynamic Time Warping Lag over varying 4 month windows - 1 week movements")+
  theme_light()


## Logging - 6 month window ----------------------------------------------


dtw_results_6months_log <- NA

for (i in seq(1,67,1)){
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(6)%m+%weeks(i-1)
  alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],
                   log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date]))
  distance <- alignment$distance 
  results <- c(start_date, end_date, distance)
  dtw_results_6months_log <- rbind(dtw_results_6months_log,results)
}

dtw_results_6months_log <- dtw_results_6months_log[-1,]
colnames(dtw_results_6months_log) <- c("start_date", "end_date", "dtw_lag")
dtw_results_6months_log <- as.data.frame(dtw_results_6months_log)
dtw_results_6months_log$start_date <- as.Date(dtw_results_6months_log$start_date)
dtw_results_6months_log$end_date <- as.Date(dtw_results_6months_log$end_date)


dtw_results_6months_log %>%
  ggplot(aes(x=start_date,y=dtw_lag/7))+
  geom_smooth(se=FALSE)+
  geom_line()+
  labs(y="Dynamic Time Warping lag (Weeks)",
       x="Start date of 6 month window",
       subtitle="Log(prevalence) vs rolling av(mobility) - 1 week movements") +
  ggtitle("Dynamic Time Warping Lag over varying 6 month windows")+
  theme_light()






# CCF  - prev vs mobility  ------------------------------------------------

## One time period ---------------------------------------------------------



lag_max=200

ccf_prev_mobility <- ccf(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date],
                    rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7),
                    lag.max=lag_max,na.action=na.pass)



ccf_prev_mobility <- as.data.frame(cbind(ccf_prev_mobility$acf,ccf_prev_mobility$lag))
colnames(ccf_prev_mobility)[1:2]<-c("acf","lag")

ccf_prev_mobility$lag[which.max(ccf_prev_mobility$acf)]
# Max CCF is 48! -> pretty long ~ 7 weeks

# Calculating confidence intervals for lag 1 to 200
n <- nrow(london_cases_ts)
k <- seq(1,200,1)

ccf_norm$n <- nrow(london_cases_ts)

ccf_prev_mobility <- ccf_prev_mobility %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
  mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))

ccf_prev_mobility %>% filter(lag>=0) %>%
  ggplot(aes(x=lag/7))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=acf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_prev_mobility$lag[which.max(ccf_prev_mobility$acf)]/7,
           y=max(ccf_prev_mobility$acf),ymin=0, ymax=max(ccf_prev_mobility$acf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_prev_mobility$lag[which.max(ccf_prev_mobility$acf)]/7,
           y=max(ccf_prev_mobility$acf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (weeks)", y="CCF",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and official cases in London")+
  theme_light()




## 6-month moving window ---------------------------------------------------
ccf_combined_results_6months <-NA

for (i in seq(1,67,1)){
  print(i)
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(6)%m+%weeks(i-1)
  ccf <- ccf(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date],
                           rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7),
                           lag.max=lag_max,na.action=na.pass)
  ccf_results <- as.data.frame(cbind(ccf$acf,ccf$lag))
  colnames(ccf_results)<-c("acf","lag")
  ccf_results <- ccf_results[ccf_results$lag>=0,]
  ccf_max <- max(ccf_results$acf)
  print(ccf_max)
  ccf_max_lag <- ccf_results$lag[which.max(ccf_results$acf)]
  print(ccf_max_lag)
  ccf_summary <- c(start_date, end_date,ccf_max, ccf_max_lag)
  ccf_combined_results_6months <- rbind(ccf_combined_results_6months,ccf_summary)
}

colnames(ccf_combined_results_6months)<-c("start_date","end_date","ccf_max","ccf_max_lag")
ccf_combined_results_6months<-ccf_combined_results_6months[-1,]
ccf_combined_results_6months <- as.data.frame(ccf_combined_results_6months)
ccf_combined_results_6months$start_date <- as.Date(ccf_combined_results_6months$start_date)
ccf_combined_results_6months$end_date <- as.Date(ccf_combined_results_6months$end_date)

ccf_combined_results_6months

ccf_combined_results_6months %>%
  ggplot(aes(x=start_date,y=ccf_max_lag/7))+
  geom_smooth(se=FALSE)+
  geom_line()+
  labs(y="CCF lag (Weeks)",
       x="Start date of 6 month window",
       subtitle="Prevalence vs rolling av(mobility)") +
  ggtitle("CCF Lag over varying 6 month windows")+
  theme_light()

## 6-month moving window - logged------------------------------------------------

ccf_combined_results_6months_log <-NA

for (i in seq(1,67,1)){
  print(i)
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(6)%m+%weeks(i-1)
  ccf <- ccf(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date]),
             rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7),
             lag.max=lag_max,na.action=na.pass)
  ccf_results <- as.data.frame(cbind(ccf$acf,ccf$lag))
  colnames(ccf_results)<-c("acf","lag")
  ccf_results <- ccf_results[ccf_results$lag>=0,]
  ccf_max <- max(ccf_results$acf)
  print(ccf_max)
  ccf_max_lag <- ccf_results$lag[which.max(ccf_results$acf)]
  print(ccf_max_lag)
  ccf_summary <- c(start_date, end_date,ccf_max, ccf_max_lag)
  ccf_combined_results_6months_log <- rbind(ccf_combined_results_6months_log,ccf_summary)
}

colnames(ccf_combined_results_6months_log)<-c("start_date","end_date","ccf_max","ccf_max_lag")
ccf_combined_results_6months_log<-ccf_combined_results_6months_log[-1,]
ccf_combined_results_6months_log <- as.data.frame(ccf_combined_results_6months_log)
ccf_combined_results_6months_log$start_date <- as.Date(ccf_combined_results_6months_log$start_date)
ccf_combined_results_6months_log$end_date <- as.Date(ccf_combined_results_6months_log$end_date)

ccf_combined_results_6months_log

ccf_combined_results_6months_log %>%
  ggplot(aes(x=start_date,y=ccf_max_lag/7))+
  geom_smooth(se=FALSE)+
  geom_line()+
  labs(y="CCF lag (Weeks)",
       x="Start date of 6 month window",
       subtitle="Log(prevalence) vs rolling av(mobility)") +
  ggtitle("CCF Lag over varying 6 month windows")+
  theme_light()







# Granger causality test --------------------------------------------------


# https://www.r-bloggers.com/2021/11/granger-causality-test-in-r-with-example/
#grangertest(x, y, order = 1, na.action = na.omit, ...)

prev_test <- london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=REACT_start & london_prev_smooth_ts$d_comb<lockdown_2_start]
length(prev_test)
prev_test <- prev_test[4:(length(prev_test)-3)]
length(prev_test)
mobility_test <- rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=REACT_start & workplace_ts$date<lockdown_2_start],7)
length(mobility_test)
# Check same lengths
length(prev_test)==length(mobility_test)

# Need to decide upon the lag to use in the test first, although could this be used to see where causation is?
granger_test_mob_prev <- grangertest(prev_test,mobility_test, order=50) 
granger_test_mob_prev 
# p>0.05 -> mobility not useful in forecasting cases
# p<0.05 -> mobility useful in forecasting cases


# order is the max lag not the only lag...
# http://www.scholarpedia.org/article/Granger_causality#:~:text=Granger%20causality%20is%20a%20statistical,values%20of%20X2%20alone.
# Maybe need to offset the time series by a chosen lag BEFORE doing the test
# But then what is the order?? Is this the p in ARIMA? i.e. autoregressiveness?

# This test requires stationarity... so tranformation and differencing must be done? but then this loses a lot of meaning??