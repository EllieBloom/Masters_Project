# Granger Causality Test

# Testing for causality between mobility and prevalence

# Date started: 14th June 2022

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


workplace_ts <- mobility_ts %>% filter(region=="LONDON",type_mobility=="workplaces")

workplace_ts$mobility_normalised <- min_max_normalise(workplace_ts$mobility)
summary(workplace_ts$mobility_normalised)

# Changing the minimum to be non-zero to allow to be logged
table(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0])
min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])
workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0]<-min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

## REACT prevalence --------------------------------------------------------


london_prev_smooth_ts <- prev_smooth_ts %>% filter(region=="LONDON")

# Normalisation
london_prev_smooth_ts$prev_normalised <- min_max_normalise(london_prev_smooth_ts$p)
summary(london_prev_smooth_ts$prev_normalised)

# Changing the minimum to be non-zero to allow to be logged
london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised==0]<-min(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised!=0])
min(london_cases_ts$cases_normalised) 


# Official cases ----------------------------------------------------------


london_cases_ts <- cases_ts %>% filter(region=="LONDON")


# Normalisation
london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
summary(london_cases_ts$cases_normalised)

# Changing the minimum to be non-zero to allow to be logged
min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) 





# Granger causality test - direct to the data------------------------------------


## Single lag --------------------------------------------------------------


# From Script: 3_Varying_Lags.R
dtw_lag <- 28

# https://www.r-bloggers.com/2021/11/granger-causality-test-in-r-with-example/
#grangertest(x, y, order = 1, na.action = na.omit, ...)

prev_test <- min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date]))
length(prev_test)
prev_test <- prev_test[4:(length(prev_test)-3)] #needed because rollmean takes of values off either side
length(prev_test)
mobility_test <- rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7)
length(mobility_test)
# Check same lengths
length(prev_test)==length(mobility_test)

# Using lag from static DTW
granger_test_mob_prev <- grangertest(mobility_test,prev_test, order=5) #,singular.ok=TRUE
granger_test_mob_prev 
# p>0.05 -> mobility not useful in forecasting cases
# p<0.05 -> mobility useful in forecasting cases


# Loop to see if any are significiant using this method...

lags <- seq(5,80,5)
lags
n_iter <- length(lags)
n_iter

granger_test_summary <- NA


for (i in 1:n_iter){
  granger_test <- grangertest(mobility_test,prev_test, order=lags[i])
  p <- granger_test$`Pr(>F)`[2]
  lag <- lags[i]
  results <- c(p,lag)
  granger_test_summary <- rbind(granger_test_summary,results)
}

granger_test_summary <- granger_test_summary[-1,]
granger_test_summary
# None are significant using this method


## Range of significant lags -----------------------------------------------

sig_lags<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Lags/ccf_prev_mobility_log_sig_lags.csv")
sig_lags

opt_lag <- sig_lags$lag[which.max(sig_lags$acf)]
opt_lag 

min_lag<-min(sig_lags$lag)
min_lag
max_lag<-max(sig_lags$lag)
max_lag
 
n_lags <- max_lag-min_lag
n_lags

start_date_prev <- REACT_start
start_date_prev
end_date_prev <- lockdown_2_start%m+%months(-1)
end_date_prev
difftime(end_date_prev,start_date_prev)

start_date_mobility <- start_date_prev%m+%days(-41)
start_date_mobility 
end_date_mobility <- end_date_prev%m+%days(-41)
end_date_mobility
difftime(end_date_mobility,start_date_mobility)
difftime(end_date_prev,start_date_prev)==difftime(end_date_mobility,start_date_mobility)




prev_test_range <- min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date_prev & london_prev_smooth_ts$d_comb<end_date_prev]))
length(prev_test_range)
prev_test_range <- prev_test_range[4:(length(prev_test_range)-3)] #needed because rollmean takes of values off either side
length(prev_test_range)
mobility_test_range <- rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date_mobility & workplace_ts$date<end_date_mobility],7)
length(mobility_test_range)
# Check same lengths
length(prev_test_range)==length(mobility_test_range)



# Using lag from static DTW
# Works with 1 - significant too
granger_test_mob_prev <- grangertest(mobility_test_range,prev_test_range, order=1) 
granger_test_mob_prev 
# This works -> VAR(1) is useful with a DTW lag of 51

# p>0.05 -> mobility not useful in forecasting cases
# p<0.05 -> mobility useful in forecasting cases

# Check for reverse causation
granger_test_prev_mob <- grangertest(prev_test_range, mobility_test_range,order=10) 
granger_test_prev_mob 






# Granger causality - stationary series -----------------------------------

# Not sure how to input a stationary time series as produced in 2_ARIMA_models -> these are models not time-series data?




# Can't really get this working...?
# 
# prev_test_stationary <- diff((log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date])))
# 
# plot(prev_test_stationary)
# 
# library(LSTS)
# Box.Ljung.Test(prev_test_stationary, lag = 20, main = NULL)
# # Very much not stationary
# 
# 
# # What about mobility
# 
# 
# mobility_test_stationary <- diff(diff(rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7)))
# Box.Ljung.Test(mobility_test, lag = 20, main = NULL)
# # Really doesn't seem to work..always gives zero p values
# 
# length(mobility_test)
# # Check same lengths
# length(prev_test)==length(mobility_test)

