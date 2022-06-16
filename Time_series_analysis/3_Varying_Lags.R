# Lag sliding window

# Date started: 10th June 2022

# Note this normalisation is now across the ENTIRE period... consider whether this is correct?

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(astsa) # Time series package used in datacamp course
library(lubridate) # Using to add months to date
library(lmtest) # For granger causality test
library(dtw)

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


workplace_ts <- mobility_ts %>% filter(region=="LONDON",type_mobility=="workplaces",date>=REACT_start, date<end_date)

workplace_ts$mobility_normalised <- min_max_normalise(workplace_ts$mobility)
summary(workplace_ts$mobility_normalised)

table(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0])
min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0]<-min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

## REACT prevalence --------------------------------------------------------

# seq(1,nrow(prev_smooth_ts),10)
# 
# prev_smooth_ts_10 <- prev_smooth_ts[seq(1,nrow(prev_smooth_ts),10),]

london_prev_smooth_ts <- prev_smooth_ts %>% filter(region=="LONDON", d_comb>=REACT_start, d_comb<end_date)

# Normalisation
london_prev_smooth_ts$prev_normalised <- min_max_normalise(london_prev_smooth_ts$p)
summary(london_prev_smooth_ts$prev_normalised)

london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised==0]<-min(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised!=0])
min(london_cases_ts$cases_normalised) 


# Official cases ----------------------------------------------------------


london_cases_ts <- cases_ts %>% filter(region=="LONDON", date>=REACT_start, date<end_date)


# Normalisation
london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
summary(london_cases_ts$cases_normalised)

min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) # Looks like this worked







# Dynamic Time warping  - prev vs mobility -------------------------------------


## Static example ----------------------------------------------------------


start_date <- REACT_start
end_date <- lockdown_2_start%m+%months(-1)

mob_series <- rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=REACT_start & workplace_ts$date<end_date],7)
length(mob_series)
prev_series <- london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=REACT_start & london_prev_smooth_ts$d_comb<end_date]
length(prev_series) # need to remove values from either side to make the same length
prev_series <- prev_series[4:(length(prev_series)-3)]
length(prev_series) ==length(mob_series)

date_series <-workplace_ts$date[workplace_ts$date>=REACT_start & workplace_ts$date<end_date]
date_series <-date_series[4:(length(date_series)-3)]
length(mob_series)==length(date_series)

dtw_lag <- dtw(mob_series, prev_series)

dtw_lag$distance
# 40.86139 days


dtw_lag_log <- dtw(mob_series,
               min_max_normalise(log(prev_series)))

dtw_lag_log$distance
# 57 days -> ~ 8 weeks

# Exploring the other ouputs
names(dtw_lag_log)
dtw_lag_log$stepPattern
dtw_lag_log$N
dtw_lag_log$M
dtw_lag_log$call
dtw_lag_log$openEnd
dtw_lag_log$openBegin
dtw_lag_log$windowFunction()
dtw_lag_log$jmin
dtw_lag_log$index1
dtw_lag_log$index2
dtw_lag_log$index1==dtw_lag_log$index1s
dtw_lag_log$index2==dtw_lag_log$index2s
dtw_lag_log$index2s
dtw_lag_log$stepsTaken
dtw_lag_log$normalizedDistance

mean(dtw_lag_log$index1-dtw_lag_log$index2)

plot(dtw_lag_log$index1,dtw_lag_log$index2,main="Warping function")
plot(dtw_lag_log, type="alignment",
     main="DTW: simple alignment plot")


# Trying a multi-plot

library(tidyverse)
library(easyGgplot2)
library(devtools)

str(dtw_lag_log)

plot2<- ggplot()+
          geom_line(aes(x=date_series,
                y=mob_series), col="royal blue")+
          coord_flip()+
          labs(x="Date",
               y="Mobility")+
          theme_minimal()+
          theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank())
plot2

plot1 <- ggplot()+
        geom_line(aes(date_series,y=prev_series), col="dark green")+
        labs(x="Date",
        y="Log prevalence")+
        theme_minimal()+
          theme(axis.text.x=element_blank(),
                axis.text.y=element_blank())

plot1


plot3 <- ggplot()+
            geom_line(aes(x=dtw_lag_log$index2,
                       y=dtw_lag_log$index1), col="red")+
            theme_minimal()+
            theme(axis.text.x=element_blank(),
                axis.text.y=element_blank())+
            labs(x="Prevalence index",y="Mobility index")
            
plot3

ggplot2.multiplot("",plot1,plot2,plot3, cols=2)


##Attempting another type of plot

mob_series <- as.data.frame(mob_series)
colnames(mob_series)<-"series"
mob_series$type <- "mobility"
mob_series <- cbind(mob_series,date_series)

prev_series <- as.data.frame(prev_series)
colnames(prev_series)<-"series"
prev_series$type <- "prev"
prev_series <- cbind(prev_series,date_series)


data_plot<- rbind(mob_series,prev_series)
data_plot <- as.data.frame(data_plot)
data_plot$type <- as.factor(data_plot$type)

ggplot(data=data_plot,aes(x=date_series,y=series, color=type))+
  geom_line()+
  geom_line(aes(group=paired))

# Can't get this to work - exporting the data to use in python instead

data_plot 
dtw_indices <- cbind(dtw_lag_log$index1,dtw_lag_log$index2)
colnames(dtw_indices) <- c("mob_index","prev_index")
dtw_indices <- as.data.frame(dtw_indices)



setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/DTW_data_plots")
write_csv(dtw_indices, "dtw_indices.csv")
write_csv(prev_series, "prev_series.csv")
write_csv(mob_series, "mob_series.csv")








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


plot_dtw_4months_notlogged <- dtw_results_4months %>%
                                ggplot(aes(x=start_date,y=dtw_lag/7))+
                                geom_smooth(se=FALSE)+
                                geom_line()+
                                labs(y="Dynamic Time Warping lag (Weeks)",
                                     x="Start date of 4 month window",
                                     subtitle="Prevalence vs rolling av(mobility) - 1 week movements") +
                                ggtitle("Dynamic Time Warping Lag over varying 4 month windows")+
                                theme_light()
plot_dtw_4months_notlogged

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Lags")
write.csv(dtw_results_4months,"dt_4months_notlogged.csv")
ggsave("plot_dtw_4months_notlogged.pdf",plot_dtw_4months_notlogged)

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

for (i in seq(1,60,1)){
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(4)%m+%weeks(i-1)
  alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],
                   min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date])))
  distance <- alignment$distance 
  results <- c(start_date, end_date, distance)
  dtw_results_4months_log <- rbind(dtw_results_4months_log,results)
}

# It looks like this might be giving hours? But how??

dtw_results_4months_log <- dtw_results_4months_log[-1,]
colnames(dtw_results_4months_log) <- c("start_date", "end_date", "dtw_lag")
dtw_results_4months_log <- as.data.frame(dtw_results_4months_log)
dtw_results_4months_log$start_date <- as.Date(dtw_results_4months_log$start_date)
dtw_results_4months_log$end_date <- as.Date(dtw_results_4months_log$end_date)


dtw_results_4months_log %>%
  ggplot(aes(x=start_date,y=dtw_lag/7))+
  geom_line()+
  geom_smooth(se=FALSE)+
  labs(y="Dynamic Time Warping lag (Weeks)",
       x="Start date of 4 month window",
       subtitle="Log(prevalence) vs rolling av(mobility) - 1 week movements") +
  ggtitle("Dynamic Time Warping Lag over varying 4 month windows")+
  theme_light()

# Jumpy but around a mean
mean(dtw_results_4months_log$dtw_lag)




## Logging - 6 month window ----------------------------------------------

# This is logged and works really nicely?! Need to check I understand the units of DTW


dtw_results_6months_log <- NA

i=1
for (i in seq(1,67,1)){
  start_date <- REACT_start%m+%weeks(i-1)
  end_date <- REACT_start%m+%months(6)%m+%weeks(i-1)
  alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],
                   min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date])))
  distance <- alignment$distance 
  results <- c(start_date, end_date, distance)
  dtw_results_6months_log <- rbind(dtw_results_6months_log,results)
}

# It looks like this might be giving hours? But how??

dtw_results_6months_log <- dtw_results_6months_log[-1,]
colnames(dtw_results_6months_log) <- c("start_date", "end_date", "dtw_lag")
dtw_results_6months_log <- as.data.frame(dtw_results_6months_log)
dtw_results_6months_log$start_date <- as.Date(dtw_results_6months_log$start_date)
dtw_results_6months_log$end_date <- as.Date(dtw_results_6months_log$end_date)


dtw_results_6months_log %>%
  ggplot(aes(x=start_date,y=dtw_lag/(7)))+
  geom_line()+
  geom_smooth(se=FALSE)+
  labs(y="Dynamic Time Warping lag (Weeks)",
       x="Start date of 6 month window",
       subtitle="Log(prevalence) vs rolling av(mobility) - 1 week movements") +
  ggtitle("Dynamic Time Warping Lag over varying 6 month windows")+
  theme_light()

# Jumpy (really really jumpy...) but around a mean
mean(dtw_results_6months_log$dtw_lag)




# Could do the same thing but for increasing lengths like with CCF














# CCF  - prev vs mobility  ------------------------------------------------



## One time period - not logged  -----------------------------------------------

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
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and prevalence London")+
  theme_light()


## One time period -  logged  -----------------------------------------------

lag_max=200

ccf_prev_mobility_log <- ccf(min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date])),
                         rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7),
                         lag.max=lag_max,na.action=na.pass)



ccf_prev_mobility_log <- as.data.frame(cbind(ccf_prev_mobility_log$acf,ccf_prev_mobility_log$lag))
colnames(ccf_prev_mobility_log)[1:2]<-c("acf","lag")

ccf_prev_mobility_log$lag[which.max(ccf_prev_mobility_log$acf)]
# Max CCF is now 83 -> very long  long ~ 12 weeks

# Calculating confidence intervals for lag 1 to 200
n <- nrow(london_cases_ts)
k <- seq(1,200,1)

ccf_prev_mobility_log$n <- nrow(london_cases_ts)

ccf_prev_mobility_log <- ccf_prev_mobility_log %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
  mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))

ccf_prev_mobility_log %>% filter(lag>=0) %>%
  ggplot(aes(x=lag/7))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=acf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_prev_mobility_log$lag[which.max(ccf_prev_mobility_log$acf)]/7,
           y=max(ccf_prev_mobility_log$acf),ymin=0, ymax=max(ccf_prev_mobility_log$acf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_prev_mobility_log$lag[which.max(ccf_prev_mobility_log$acf)]/7,
           y=max(ccf_prev_mobility_log$acf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (weeks)", y="CCF",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and log(prevalence)")+
  theme_light()

# Find all of the lags which are significant!

ccf_prev_mobility_log_sig_lags <-
      ccf_prev_mobility_log %>% filter(lag>=0) %>% filter(acf>upper_ci)
# Significant from lags 49 to 101 -> could therefore shift the time series forward 49 to make it line up

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Lags")
write.csv(ccf_prev_mobility_log_sig_lags,"ccf_prev_mobility_log_sig_lags.csv")




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









