# Fitting ARIMA models to each time series tibble

# Date started: 8th June 2022


# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(astsa) # Time series package used in datacamp course
library(lubridate)
library(zoo)
library(fable) # For auto ARIMA I think


# Defining min max function

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


# Loading tibbles ---------------------------------------------------------

mobility_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/mobility_tibble_raw.rds")
summary(mobility_ts)

cases_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/cases_tibble.rds")
summary(cases_ts)

prev_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/prev_tibble.rds")
summary(prev_ts)

prev_smooth_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/prev_smooth_tibble.rds")

# Constraints -------------------------------------------------------------

# Looking at London workplace mobility from the start of lockdown 1 to the start of lockdown 2

region_of_interest <- "LONDON"
mobility_of_interest <- "workplaces"
mobility_of_interest_av <- "workplaces_av"
start_date <- REACT_start
end_date <- lockdown_2_start%m+%months(-1) # Make it the month before the start of lockdown to allow for test/train split/forecasting



# Exploring ARIMA models --------------------------------------------------




## Workplace mobility ------------------------------------------------------


### Getting data ready ------------------------------------------------------

# Data normalised including forecasting period
workplace_ts_all <- mobility_ts %>% filter(region=="LONDON",type_mobility=="workplaces", date<lockdown_2_start)


min_workplace <- min(workplace_ts_all$mobility)
min_workplace #-87
max_workplace <- max(workplace_ts_all$mobility)
max_workplace # 1



# Need to use min max scaling to bring all values between 0 and 1 so that they can be logged

workplace_ts_all$mobility_normalised <- min_max_normalise(workplace_ts_all$mobility)
summary(workplace_ts_all$mobility_normalised)




# Change any 0s to the non-zero miniumum
workplace_ts_all$mobility_normalised[workplace_ts_all$mobility_normalised==0]<-min(workplace_ts_all$mobility_normalised[workplace_ts_all$mobility_normalised!=0])
min(workplace_ts_all$mobility_normalised) 


# Now filter to the desired date
workplace_ts <- workplace_ts_all %>% filter(date>=start_date, date<end_date)

ggplot(data=workplace_ts, aes(x=date,y=mobility)) +
  geom_line(color="royal blue") + 
  ggtitle("Workplace mobility in London")+
  labs(y="Mobility change from baseline (%)",
       x="Date (2020)",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  theme_light()




### Exploring ARIMA ---------------------------------------------------------

# For Ljung Box test
library(stats)
# H0: data are independently distributed
# H1: data are not independtly distributed; they exhibit serial correlation

Box.test(london_cases_ts$cases_normalised, lag = 20, type = "Ljung")


# Normalised data
tsplot(workplace_ts$mobility_normalised)
Box.test(workplace_ts$mobility_normalised, lag = 20, type = "Ljung")
# p< 0.05 -> not stationary
sarima(workplace_ts$mobility_normalised,0,0,0,0,0,0,7)

# Log normalised (to reduce increasing variance)
tsplot(log(workplace_ts$mobility_normalised))
Box.test(log(workplace_ts$mobility_normalised), lag = 20, type = "Ljung")
# p< 0.05 -> not stationary
sarima(log(workplace_ts$mobility_normalised),0,0,0,0,0,0,7)

# Non-seasonal differenced log data
tsplot(diff(log(workplace_ts$mobility_normalised)))
Box.test(diff(log(workplace_ts$mobility_normalised)), lag = 20, type = "Ljung")
# p< 0.05 -> not stationary
sarima(log(workplace_ts$mobility_normalised),0,2,0,0,0,0,7)

# Additional weekly seasonal differencing
tsplot(diff(diff(log(workplace_ts$mobility_normalised),lag=7)))
Box.test(diff(diff(log(workplace_ts$mobility_normalised),lag=7)), lag = 20, type = "Ljung")
# p< 0.05 -> not stationary
sarima(log(workplace_ts$mobility_normalised),0,1,0,0,1,0,7)

# Only seasonal differencing
tsplot(diff(workplace_ts$mobility_normalised,lag=7))
Box.test(diff(workplace_ts$mobility_normalised,lag=7), lag = 20, type = "Ljung")
# p< 0.05 -> not stationary
sarima(workplace_ts$mobility_normalised,0,0,0,0,1,0,7)



# Creating log dd data to use later
mobility_normalised_log_dd <- diff(diff(log(workplace_ts$mobility_normalised),lag=7))

acf2(mobility_normalised_log_dd)

# Fitting auto model
fit_log <- workplace_ts %>%
  model( auto = ARIMA(log(mobility_normalised), stepwise = FALSE, approx = FALSE)
  )

fit_log
 
# Auto gives
# p = 4
# d = 0
# q = 0
# P = 0
# D = 1
# Q = 1
# S = 7



sarima(log(workplace_ts$mobility_normalised),4,0,0,0,1,1,7)
# AIC: -0.3807216
# BIC: -0.2402253

# Looks pretty good - there are however heavy tails for redisuals -> not quite normal, also fails on Ljung Box with greater lags




# Could also try without logging and see if it is an improvement

fit <- workplace_ts %>%
  model( auto = ARIMA(mobility_normalised, stepwise = FALSE, approx = FALSE)
  )

fit

fit_final <- sarima(workplace_ts$mobility_normalised,2,0,1,0,1,2,7)
fit_final
# Better than logged data, especially on Ljung-Box test -> use unlogged for workplace mobility
# AIC: -3.594837
# BIC: -3.45434



### Plotting model/forecasting ------------------------------------------------------

#Using astsa package to get predicted data
pred_mob=sarima.for(workplace_ts$mobility_normalised,n.ahead=31,2,0,1,0,1,2,7)
pred_mob
length(pred_mob$pred)

pred_date<-seq(max(workplace_ts$date)%m+%days(1),max(workplace_ts$date)%m+%days(31),1)
length(pred_date)

pred_final <-cbind(pred_date,pred_mob$pred)
colnames(pred_final)<-c("date","mob_pred")
pred_final <- as.data.frame(pred_final)
pred_final$date <- as.Date(pred_final$date)


# Getting actual data to compare forecast with
min_date <- min(pred_final$date)
max_date <- max(pred_final$date)


workplace_ts_pred_actual <- workplace_ts_all %>% filter(date>=end_date)
workplace_ts_pred_actual$date <-as.Date(workplace_ts_pred_actual$date)

# Plot, converted back to original scale...

ggplot() +
  geom_line(data=workplace_ts, aes(x=date,
                                   y=(mobility_normalised*(max_workplace-min_workplace)+min_workplace))) + 
  geom_line(data=workplace_ts_pred_actual, aes(x=date,
                                               y=(mobility_normalised*(max_workplace-min_workplace)+min_workplace)), col="blue")+
  geom_line(data=pred_final, aes(x=date,
                                 y=(mob_pred*(max_workplace-min_workplace)+min_workplace)), linetype="dashed", col="red")+
  ggtitle("Workplace mobility in London")+
  labs(y="Mobility change from baseline (%)",
       x="Date (2020)",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  theme_light()

# Blue = actual
# Read = forecast





## Official cases ----------------------------------------------------------



### Getting data ready ------------------------------------------------------


london_cases_ts_all <- cases_ts %>% filter(region=="LONDON", date>=start_date, date<lockdown_2_start)

summary(london_cases_ts_all)


min_cases <- min(london_cases_ts_all$cases)
min_cases #20
max_cases <- max(london_cases_ts_all$cases)
max_cases # 2965


# Normalisation prior to cutting off final month
london_cases_ts_all$cases_normalised <- min_max_normalise(london_cases_ts_all$cases)
# Check normalisation
summary(london_cases_ts_all$cases_normalised)

# Change min to be non-zero
london_cases_ts_all$cases_normalised[london_cases_ts_all$cases_normalised==0]<-min(london_cases_ts_all$cases_normalised[london_cases_ts_all$cases_normalised!=0])
min(london_cases_ts_all$cases_normalised) # Check - o



# Remove the forecasting window

london_cases_ts <- london_cases_ts_all %>% filter(date<end_date)

ggplot(data=london_cases_ts, aes(x=date,y=cases)) +
  geom_line(color="red") + 
  ggtitle("Official cases in London")+
  labs(y="Cases",
       x="Date (2020)",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  theme_light()





### Exploring ARIMA ---------------------------------------------------------

# For Ljung Box test
library(stats)
# H0: data are independently distributed
# H1: data are not independtly distributed; they exhibit serial correlation


# Normalised data
tsplot(london_cases_ts$cases_normalised)
Box.test(london_cases_ts$cases_normalised, lag = 20, type = "Ljung")
# p<0.05 -> not stationary

# Log normalised (to reduce increasing variance)
tsplot(log(london_cases_ts$cases_normalised))
Box.test(log(london_cases_ts$cases_normalised), lag = 20, type = "Ljung")
# p<0.05 -> not stationary

# Non-seasonal differenced log data
tsplot(diff(log(london_cases_ts$cases_normalised)))
Box.test(diff(log(london_cases_ts$cases_normalised)), lag = 20, type = "Ljung")
# p<0.05 -> not stationary


# Additional weekly seasonal differencing
tsplot(diff(diff(log(london_cases_ts$cases_normalised),lag=7)))
Box.test(diff(diff(log(london_cases_ts$cases_normalised),lag=7)), lag = 20, type = "Ljung")
# p<0.05 -> not stationary

# Only a seasonal difference
tsplot(diff(log(london_cases_ts$cases_normalised),lag=7))
Box.test(diff(log(london_cases_ts$cases_normalised),lag=7), lag = 1, type = "Ljung")
# p<0.05 -> not stationary



# ARIMA auto on log

cases_fit_log <- london_cases_ts %>%
  model( auto = ARIMA(log(cases_normalised), stepwise = FALSE, approx = FALSE)
  )

cases_fit_log
# AIC: 1.130232
# BIC: 1.190714

sarima(log(london_cases_ts$cases_normalised),0,1,1,0,1,1,7)
# Fails Ljung-Box test on longer lags 


# Try without log
tsplot(diff(diff(london_cases_ts$cases_normalised,lag=7))) # looks a lot less stationary

cases_fit <- london_cases_ts %>%
  model( auto = ARIMA(cases_normalised, stepwise = FALSE, approx = FALSE)
  )

cases_fit
# AIC: 1.182877
# BIC: 1.26352

sarima(log(london_cases_ts$cases_normalised),0,1,1,2,1,0,7)
# Not much different -> not the best models in general, but just about ok?




### Plotting model/forecsating ------------------------------------------------


#Using astsa package to get predicted data
pred_cases=sarima.for(log(london_cases_ts$cases_normalised),n.ahead=31,01,1,1,2,1,0,7)
pred_cases
length(pred_cases$pred)

pred_final_cases <-cbind(pred_date,pred_cases$pred)
colnames(pred_final_cases)<-c("date","cases_pred")
pred_final_cases <- as.data.frame(pred_final_cases)
pred_final_cases$date <- as.Date(pred_final_cases$date)


# Getting actual data to compare forecast with

cases_ts_pred_actual <- london_cases_ts_all %>% filter(date>=end_date)
cases_ts_pred_actual$date <-as.Date(cases_ts_pred_actual$date)


# Plot on original scale

ggplot() +
  geom_line(data=london_cases_ts, aes(x=date,y=cases_normalised*(max_cases-min_cases)+min_cases)) + 
  geom_line(data=cases_ts_pred_actual, aes(x=date,y=cases_normalised*(max_cases-min_cases)+min_cases), col="blue")+
  geom_line(data=pred_final_cases, aes(x=date,y=(exp(cases_pred) 
                                       *(max_cases-min_cases)+min_cases)), linetype="dashed", col="red")+
  ggtitle("Official cases in London")+
  labs(y="Cases",
       x="Date (2020)",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  theme_light()

# Blue = actual
# Read = forecast


## Smoothed prevalence -----------------------------------------------------
# Currently using data with 10 points per day...
# Just take every 10th row
# seq(1,nrow(prev_smooth_ts),10)
# prev_smooth_ts_10 <- prev_smooth_ts[seq(1,nrow(prev_smooth_ts),10),]


### Getting data ready --------------------------------------------------



london_prev_smooth_ts_all <- prev_smooth_ts %>% filter(region=="LONDON", d_comb>=start_date, d_comb<lockdown_2_start)



# Normalisation
london_prev_smooth_ts_all$prev_normalised <- min_max_normalise(london_prev_smooth_ts_all$p)
# Check normalisation
summary(london_prev_smooth_ts_all$prev_normalised)


min_prev <- min(london_prev_smooth_ts_all$p)
min_prev #  0.0005074915
max_prev <- max(london_prev_smooth_ts_all$p)
max_prev # 0.01363462

london_prev_smooth_ts_all$prev_normalised[london_prev_smooth_ts_all$prev_normalised==0]<-min(london_prev_smooth_ts_all$prev_normalised[london_prev_smooth_ts_all$prev_normalised!=0])
min(london_prev_smooth_ts_all$prev_normalised) 

# Now restrict the date
london_prev_smooth_ts_all$date <- as.Date(london_prev_smooth_ts_all)
london_prev_smooth_ts <- london_prev_smooth_ts_all %>% filter(d_comb<end_date)

ggplot(data=london_prev_smooth_ts, aes(x=d_comb,y=p)) +
  geom_line(color="dark green") + 
  ggtitle("REACT p-spline prevalence")+
  labs(y="Prevalence (%)",
       x="Date (2020)",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  theme_light()



### Exploring ARIMA ---------------------------------------------------------



# For Ljung Box test
library(stats)
# H0: data are independently distributed
# H1: data are not independtly distributed; they exhibit serial correlation

# Normalised data
tsplot(london_prev_smooth_ts$prev_normalised)
Box.test(london_prev_smooth_ts$prev_normalised, lag = 20, type = "Ljung")
# p<0.05 -> not stationary


# Log normalised (to reduce increasing variance)
tsplot(log(london_prev_smooth_ts$prev_normalised))
Box.test(log(london_prev_smooth_ts$prev_normalised), lag = 20, type = "Ljung")
# p<0.05 -> not stationary

# Non-seasonal differenced - not logged
tsplot(diff(london_prev_smooth_ts$prev_normalised))
Box.test(diff(london_prev_smooth_ts$prev_normalised), lag = 20, type = "Ljung")
# p<0.05 -> not stationary

# Non-seasonal differenced -  logged
tsplot(diff(log(london_prev_smooth_ts$prev_normalised)))
Box.test(diff(log(london_prev_smooth_ts$prev_normalised)), lag = 20, type = "Ljung")
# p<0.05 -> not stationary


# Try auto ARIMA

# Not logged
prev_smooth_fit <- london_prev_smooth_ts %>%
  model( auto = ARIMA(prev_normalised, stepwise = FALSE, approx = FALSE)
  )

prev_smooth_fit

sarima(london_prev_smooth_ts$prev_normalised,4,2,2)
# AIC: -13.10584
# BIC: -12.96839
# Fails on the Ljung-Box test -> NOT stationary

# Try log
prev_smooth_fit_log <- london_prev_smooth_ts %>%
  model( auto = ARIMA(log(prev_normalised), stepwise = FALSE, approx = FALSE)
  )

prev_smooth_fit_log

sarima_prev_log <- sarima(log(london_prev_smooth_ts$prev_normalised),1,1,1)
sarima_prev_log
# A lot better!
# AIC: -0.09708353
# BIC: -0.01888209
# Does pretty well on all tests! Definitely better fit logged -> AIC and BIC nat difference scaled


str(sarima_prev_log)
dput(sarima_prev_log$fit) # gives the model and coefficients

### Plotting model/forecsating ------------------------------------------------

london_prev_smooth_ts_all

#Using astsa package to get predicted data
pred_prev=sarima.for(log(london_prev_smooth_ts$prev_normalised),n.ahead=31,1,1,1)
pred_prev
length(pred_prev$pred)


pred_final_prev<-cbind(pred_date,pred_prev$pred)
colnames(pred_final_prev)<-c("date","prev_pred")
pred_final_prev <- as.data.frame(pred_final_prev)
pred_final_prev$date <- as.Date(pred_final_prev$date)


# Getting actual data to compare forecast with

prev_ts_pred_actual <- london_prev_smooth_ts_all %>% filter(d_comb>=end_date)
prev_ts_pred_actual$d_comb <-as.Date(prev_ts_pred_actual$d_comb)


exp(prev_pred)*(max_prev-min_prev)+min_prev

ggplot() +
  geom_line(data=london_prev_smooth_ts, aes(x=d_comb,y=prev_normalised*(max_prev-min_prev)+min_prev)) + 
  geom_line(data=prev_ts_pred_actual, aes(x=d_comb,y=prev_normalised*(max_prev-min_prev)+min_prev), col="blue")+
  geom_line(data=pred_final_prev, aes(x=date,y=exp(prev_pred)*(max_prev-min_prev)+min_prev), linetype="dashed", col="red")+
  ggtitle("REACT Prevalence in London")+
  labs(y="Prevalence (%)",
       x="Date (2020)",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  theme_light()

# Blue = actual
# Read = forecast
















# CCF ---------------------------------------------------------------------


## Workplace mobility and cases --------------------------------------------

# Now aligned in terms of dates
summary(workplace_ts$date)
summary(london_cases_ts$date)


# Match in terms of dates

lag_max <- 200

ccf_summary <- NA
region_list<- unique(cases_ts$region)

# Loop

ccf <- ccf(diff(diff(log(london_cases_ts$cases_normalised)),lag=7),
           diff(workplace_ts$mobility_normalised, lag=7), lag.max=lag_max,na.action=na.pass)


ccf
ccf_results <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_results)[1:2]<-c("acf","lag")


# Just do CCF on normalised data - using rolling mean for both

ccf_norm <- ccf(rollmean(london_cases_ts$cases_normalised,7),
                rollmean(workplace_ts$mobility_normalised,7),
                lag.max=lag_max,na.action=na.pass)

# Should I be normalising then rolling averaging or the other way around?

ccf_norm <- as.data.frame(cbind(ccf_norm$acf,ccf_norm$lag))
colnames(ccf_norm)[1:2]<-c("acf","lag")

ccf_norm$lag[which.max(ccf_norm$acf)]
# Max CCF is 11! -> differs greatly dependent on the date range -> much shorter when I start in May compared to March

# Calculating confidence intervals for lag 1 to 200
n <- nrow(london_cases_ts)
k <- seq(1,200,1)

ccf_norm$n <- nrow(london_cases_ts)

ccf_norm <- ccf_norm %>% 
            mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
            mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))



ccf_norm %>% filter(lag>=0) %>%
ggplot(aes(x=lag/7))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=acf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  # geom_hline(yintercept = qnorm(0.975)/sqrt(nrow(london_cases_ts)), linetype="dashed", color="dark grey")+ # upper CI bound (just uses quantiles)
  # geom_hline(yintercept = -qnorm(0.975)/sqrt(nrow(london_cases_ts)), linetype="dashed", color="dark grey")+ # lower CI bound (just uses quantiles)
  annotate("pointrange", x=ccf_norm$lag[which.max(ccf_norm$acf)]/7,
           y=max(ccf_norm$acf),ymin=0, ymax=max(ccf_norm$acf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_norm$lag[which.max(ccf_norm$acf)]/7,
           y=max(ccf_norm$acf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (weeks)", y="CCF", subtitle="Start of REACT to 1 month before end of lockdown 2")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
    ggtitle("Cross correlation function (CCF) for workplace mobility and official cases in London")+
  theme_light()






## Workplace mobility and smoothed prev --------------------------------------------



ccf_norm <- ccf(london_prev_smooth_ts$prev_normalised,
                rollmean(workplace_ts$mobility_normalised,7),
                lag.max=lag_max,na.action=na.pass)

ccf_norm <- as.data.frame(cbind(ccf_norm$acf,ccf_norm$lag))
colnames(ccf_norm)[1:2]<-c("acf","lag")

ccf_norm$lag[which.max(ccf_norm$acf)]
# lag is now 73 days ~ 10 weeks? Why so much longer??




# Calculating confidence intervals for lag 1 to 200
n <- nrow(london_prev_smooth_ts)
k <- seq(1,max(ccf_norm$lag),1)

ccf_norm$n <- nrow(london_cases_ts)

ccf_norm <- ccf_norm %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
  mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))



ccf_norm %>% filter(lag>=0) %>%
  ggplot(aes(x=lag/7))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=acf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_norm$lag[which.max(ccf_norm$acf)]/7,
           y=max(ccf_norm$acf),ymin=0, ymax=max(ccf_norm$acf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_norm$lag[which.max(ccf_norm$acf)]/7,
           y=max(ccf_norm$acf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (weeks)", y="CCF",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and prevalence in London")+
  theme_light()

# Repeat with logged prevalence

ccf_norm_log <- ccf(log(london_prev_smooth_ts$prev_normalised),
                rollmean(workplace_ts$mobility_normalised,7),
                lag.max=lag_max,na.action=na.pass)

ccf_norm_log <- as.data.frame(cbind(ccf_norm_log$acf,ccf_norm_log$lag))
colnames(ccf_norm_log)[1:2]<-c("acf","lag")

ccf_norm_log$lag[which.max(ccf_norm_log$acf)]
# lag is now 83 days ~ 12 weeks -> why so long? perhaps DTW would be better....


ccf_norm_log <- ccf_norm_log %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
  mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))


ccf_norm_log %>% filter(lag>=0) %>%
  ggplot(aes(x=lag/7))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=acf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_norm_log$lag[which.max(ccf_norm_log$acf)]/7,
           y=max(ccf_norm_log$acf),ymin=0, ymax=max(ccf_norm_log$acf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_norm_log$lag[which.max(ccf_norm_log$acf)]/7,
           y=max(ccf_norm_log$acf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (weeks)", y="CCF",
       subtitle="Start of REACT to 1 month before end of lockdown 2")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and prevalence in London")+
  theme_light()










ccf_norm <- ccf_norm %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
  mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))# Playing with DTW --------------------------------------------------------
alignment <- dtw(workplace_ts$mobility_normalised, london_cases_ts$cases_normalised)
alignment$distance # Get 33 from this -> EXACTLY the same as the lag method!!!

alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=min(london_prev_smooth_ts$d_comb)], 
                 london_prev_smooth_ts$prev_normalised)
alignment$distance # Get 34 from this -> a lot more reasonable than the CCF method?

# Maybe should start after lockdown 1 end date -> couplind varies depending on if in or out of lockdown a lot
# Dates REALLY MATTER
# Think about how to systematically explore dates....use a one month sliding window of start and end date?
alignment <- dtw(workplace_ts$mobility_normalised[workplace_ts$date>=lockdown_1_end], 
                 london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=lockdown_1_end])
alignment$distance 
