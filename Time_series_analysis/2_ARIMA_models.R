# Fitting ARIMA models to each time series tibble

# Date started: 8th June 2022


# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(astsa) # Time series package used in datacamp course

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
start_date <- lockdown_1_start
end_date <- lockdown_2_start



# Exploring ARIMA models --------------------------------------------------



## Workplace mobility ------------------------------------------------------

workplace_ts <- mobility_ts %>% filter(region=="LONDON", date>=start_date, date<end_date,type_mobility=="workplaces")

ggplot(data=workplace_ts, aes(x=date,y=mobility)) +
  geom_line(color="royal blue") + 
  ggtitle("Workplace mobility in London - end of lockdown 1 to start of lockdown 2")+
  labs(y="Mobility change from baseline (%)",
       x="Date (2020)")+
  theme_light()


# 
# acf2(workplace_ts$mobility)
# acf2(diff(workplace_ts$mobility, lag=7))
# acf2(diff(diff(workplace_ts$mobility), lag=7))

# Need to use min max scaling to bring all values between 0 and 1 so that they can be logged

min_max_normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

workplace_ts$mobility_normalised <- min_max_normalise(workplace_ts$mobility)
# Check normalisation
summary(workplace_ts$mobility_normalised)

table(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0])
min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])
# Change any 0s to the non-zero miniumum


workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0]<-min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])
min(workplace_ts$mobility_normalised) # Looks like this worked


# Normalised data
tsplot(workplace_ts$mobility_normalised)
# Log normalised (to reduce increasing variance)
tsplot(log(workplace_ts$mobility_normalised))
# Non-seasonal differenced log data
tsplot(diff(log(workplace_ts$mobility_normalised)))
# Additional weekly seasonal differencing
tsplot(diff(diff(log(workplace_ts$mobility_normalised),lag=7)))

# Can't line this up with dates... not sure what to do about that
mobility_normalised_log_dd <- diff(diff(log(workplace_ts$mobility_normalised),lag=7))

acf2(mobility_normalised_log_dd)

sarima(workplace_ts$mobility_normalised,1,1,1,1,2,1,7)

fit_log <- workplace_ts %>%
  model( auto = ARIMA(log(mobility_normalised), stepwise = FALSE, approx = FALSE)
  )


fit_log
 
# Auto gives
# p = 3
# d = 1
# q = 1
# P = 2
# D = 0
# Q = 0
# S = 7


sarima(log(workplace_ts$mobility_normalised),3,1,1,2,0,0,7)
# AIC 1.572744
# BIC 1.687405

# Looks pretty good - there are however heavy tails for redisuals -> not quite normal

# Could also try without logging and see if it is an improvement

fit <- workplace_ts %>%
  model( auto = ARIMA(mobility_normalised, stepwise = FALSE, approx = FALSE)
  )

fit

sarima(workplace_ts$mobility_normalised,3,0,2,0,1,1,7)

# Better than logged data



## Official cases ----------------------------------------------------------

london_cases_ts <- cases_ts %>% filter(region=="LONDON", date>=start_date, date<end_date)

summary(london_cases_ts)

ggplot(data=london_cases_ts, aes(x=date,y=cases)) +
  geom_line(color="red") + 
  ggtitle("Official cases in London - end of lockdown 1 to start of lockdown 2")+
  labs(y="Cases",
       x="Date (2020)")+
  theme_light()

# Normalisation

london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
# Check normalisation
summary(london_cases_ts$cases_normalised)

min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
# Change any 0s to e.g 0.001


london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) # Looks like this worked


# Normalised data
tsplot(london_cases_ts$cases_normalised)
# Log normalised (to reduce increasing variance)
tsplot(log(london_cases_ts$cases_normalised))
# Non-seasonal differenced log data
tsplot(diff(log(london_cases_ts$cases_normalised)))
# Additional weekly seasonal differencing
tsplot(diff(diff(log(london_cases_ts$cases_normalised),lag=7)))


# ARIMA auto on log

cases_fit_log <- london_cases_ts %>%
  model( auto = ARIMA(log(cases_normalised), stepwise = FALSE, approx = FALSE)
  )

cases_fit_log

sarima(log(london_cases_ts$cases_normalised),0,1,2,0,1,1,7)

# Try without log
tsplot(diff(diff(london_cases_ts$cases_normalised,lag=7))) # looks a lot less stationary

cases_fit <- london_cases_ts %>%
  model( auto = ARIMA(cases_normalised, stepwise = FALSE, approx = FALSE)
  )



## Smoothed prevalence -----------------------------------------------------
# Currently using data with 10 points per day...
# Just take every 10th row

seq(1,nrow(prev_smooth_ts),10)

prev_smooth_ts_10 <- prev_smooth_ts[seq(1,nrow(prev_smooth_ts),10),]

london_prev_smooth_ts <- prev_smooth_ts_10 %>% filter(region=="LONDON", d_comb>=start_date, d_comb<end_date)

ggplot(data=london_prev_smooth_ts, aes(x=d_comb,y=p)) +
  geom_line(color="dark green") + 
  ggtitle("REACT p-spline prevalence - end of lockdown 1 to start of lockdown 2")+
  labs(y="Prevalence (%)",
       x="Date (2020)")+
  theme_light()

# Normalisation

london_prev_smooth_ts$prev_normalised <- min_max_normalise(london_prev_smooth_ts$p)
# Check normalisation
summary(london_prev_smooth_ts$prev_normalised)

london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised==0]<-min(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised!=0])
min(london_cases_ts$cases_normalised) 


# Normalised data
tsplot(london_prev_smooth_ts$prev_normalised)
# Log normalised (to reduce increasing variance)
tsplot(log(london_prev_smooth_ts$prev_normalised))
# Non-seasonal differenced - not logged
tsplot(diff(london_prev_smooth_ts$prev_normalised,lag=10))
# Second difference...
tsplot(diff(diff(london_prev_smooth_ts$prev_normalised,lag=10),lag=10))
# Third difference - starting to look a lot better
tsplot(diff(diff(diff(london_prev_smooth_ts$prev_normalised,lag=10),lag=10)),lag=10)
# Add a log back in...
tsplot(diff(diff(diff(log(london_prev_smooth_ts$prev_normalised),lag=10),lag=10)),lag=10)
# Doesn't look good...

# Try auto ARIMA

# Not logged
prev_smooth_fit <- london_prev_smooth_ts %>%
  model( auto = ARIMA(prev_normalised, stepwise = FALSE, approx = FALSE)
  )
# this doesn't work...

prev_smooth_fit

sarima(london_prev_smooth_ts$prev_normalised,1,2,5)
# Not great at all...

# Try log
prev_smooth_fit_log <- london_prev_smooth_ts %>%
  model( auto = ARIMA(log(prev_normalised), stepwise = FALSE, approx = FALSE)
  )

prev_smooth_fit_log
sarima(log(london_prev_smooth_ts$prev_normalised),1,1,1)
# A lot better!




# CCF ---------------------------------------------------------------------


## Workplace mobility and cases --------------------------------------------

summary(workplace_ts$date)
# Min 26/03/202
# Max 04/11/2020

summary(london_cases_ts$date)
# Min 26/03/2020
# Max 04/11/2020

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

library(zoo)
ggplot(data=ccf_results, aes(x=date,y=rollmean)+
  geom_line()

# Just do CCF on normalised data - using rolling mean for both

ccf_norm <- ccf(rollmean(london_cases_ts$cases_normalised,7),
                rollmean(workplace_ts$mobility_normalised,7),
                lag.max=lag_max,na.action=na.pass)

# Should I be normalising then rolling averaging or the other way around?

ccf_norm <- as.data.frame(cbind(ccf_norm$acf,ccf_norm$lag))
colnames(ccf_norm)[1:2]<-c("acf","lag")

ccf_norm$lag[which.max(ccf_norm$acf)]
# Max CCF is 33! -> more reasonable Very similar to 4 weeks in previous paper :)

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
  labs(x="Lag (weeks)", y="CCF")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
    ggtitle("Cross correlation function (CCF) for workplace mobility and official cases in London")+
  theme_light()


##  Workplace mobility and cases - restricting first date ----------------

workplace_ts_rest <- workplace_ts %>% filter(date>=min(london_prev_smooth_ts$d_comb))

london_cases_ts_rest<- london_cases_ts %>% filter(date>=min(london_prev_smooth_ts$d_comb))


# Just do CCF on normalised data - using rolling mean for both

ccf_norm <- ccf(rollmean(london_cases_ts_rest$cases_normalised,7),
                rollmean(workplace_ts_rest$mobility_normalised,7),
                lag.max=lag_max,na.action=na.pass)

# Should I be normalising then rolling averaging or the other way around?

ccf_norm <- as.data.frame(cbind(ccf_norm$acf,ccf_norm$lag))
colnames(ccf_norm)[1:2]<-c("acf","lag")

ccf_norm$lag[which.max(ccf_norm$acf)]
# Max CCF is 33! -> more reasonable Very similar to 4 weeks in previous paper :)

# Calculating confidence intervals for lag 1 to 200
n <- nrow(london_cases_ts_rest)
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
  labs(x="Lag (weeks)", y="CCF")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and official cases in London")+
  theme_light()



ccf_norm$lag[which.max(ccf_norm$acf)]
# When I start in May -> lag is so much shorter!













## Workplace mobility and smoothed prev --------------------------------------------

summary(workplace_ts$date)
# Min 26/03/2020
# Max 04/11/2020


summary(london_prev_smooth_ts)
# Min date - 01/05/2020
# Max date - 04/11/02020

difftime(min(london_prev_smooth_ts$d_comb),min(workplace_ts$date), units="days") # 36 days lag already -> may need to add this on...




# Repeat this but restrict london to alternative start date

workplace_ts_rest <- workplace_ts %>% filter(date>=min(london_prev_smooth_ts$d_comb))
nrow(workplace_ts_rest)

ccf_norm <- ccf(london_prev_smooth_ts$prev_normalised,
                rollmean(workplace_ts_rest$mobility_normalised,7),
                lag.max=lag_max,na.action=na.pass)

ccf_norm <- as.data.frame(cbind(ccf_norm$acf,ccf_norm$lag))
colnames(ccf_norm)[1:2]<-c("acf","lag")

ccf_norm$lag[which.max(ccf_norm$acf)]
# lag is now 17+36 = 53 days ~ 7.5 weeks




# Calculating confidence intervals for lag 1 to 200
n <- nrow(london_prev_smooth_ts)
k <- seq(1,max(ccf_norm$lag),1)

ccf_norm$n <- nrow(london_cases_ts)

ccf_norm <- ccf_norm %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-lag))) %>%
  mutate(lower_ci = -qnorm(0.975)*sqrt(1/(n-lag)))

#

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
  labs(x="Lag (weeks)", y="CCF")+
  xlim(0,200/7)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("Cross correlation function (CCF) for workplace mobility and prevalence in London")+
  theme_light()

# Actually far shorter
ccf_norm$lag[which.max(ccf_norm$acf)]






# Playing with DTW --------------------------------------------------------
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
