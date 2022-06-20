# Exploring VARIMA

# Date started: 14th June 2022

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(astsa) # Time series package used in datacamp course
library(lubridate) # Using to add months to date
library(lmtest) # For granger causality test
library(vars) # For VARIMA: https://towardsdatascience.com/fun-with-arma-var-and-granger-causality-6fdd29d8391c

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


workplace_ts <- mobility_ts %>% filter(region=="LONDON",type_mobility=="workplaces", date<=lockdown_2_start)

workplace_ts$mobility_normalised <- min_max_normalise(workplace_ts$mobility)
summary(workplace_ts$mobility_normalised)

# Changing the minimum to be non-zero to allow to be logged
table(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0])
min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])
workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0]<-min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

## REACT prevalence --------------------------------------------------------


london_prev_smooth_ts <- prev_smooth_ts %>% filter(region=="LONDON", d_comb<lockdown_2_start)

# Normalisation
london_prev_smooth_ts$prev_normalised <- min_max_normalise(london_prev_smooth_ts$p)
summary(london_prev_smooth_ts$prev_normalised)

# Changing the minimum to be non-zero to allow to be logged
london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised==0]<-min(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised!=0])
min(london_cases_ts$cases_normalised) 


## Official cases ----------------------------------------------------------


london_cases_ts <- cases_ts %>% filter(region=="LONDON", date>=start_date)


# Normalisation
london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
summary(london_cases_ts$cases_normalised)

# Changing the minimum to be non-zero to allow to be logged
min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) 










# Playing around ----------------------------------------------------------
# Following: https://towardsdatascience.com/fun-with-arma-var-and-granger-causality-6fdd29d8391c

series <- cbind(workplace_ts$mobility_normalised[workplace_ts$date>=start_date&workplace_ts$date<end_date],
            min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date&london_prev_smooth_ts$d_comb<end_date])))

colnames(series) <-c("mobility_normalised","prev_log_normalised")
head(series)

##Model selection
var_order <- VARselect(series, lag.max = 100, type = "const")
var_order$selection
# AIC, HQ suggest VAR(50) -> consistent with a lag of around 50...
# FPE suggests VAR(40)
# SC suggests VAR(7)


##Fit VAR model as suggested by VAR select information criterion - VAR(4)
model_var <- VAR(series, type = "const", lag.max = 40, ic = "SC")
summary(model_var)
# Mobility coefficients are not significant...

##Collect coefficient values
coef_list <- coef(model_var)
coef_list

##Method-1
##model_var is the object that contains VAR model
#Almost similar to Lagrange Multiplier test in Stata
library("vars")
method1 <- serial.test(model_var, lags.pt = 50, type = "PT.asymptotic")
method1
# p=0.9912 -> errors have no serial correlation?


##Method-2
#Focused on Robust heteroskedastic errors
library("VARtests")
method2 <- ACtest(fit = model_var, h = 50, HCtype = c("HC1"))
method2
# p=0.9981 -> good?

##Optional - Normality test of errors
norm_test <- normality.test(model_var, multivariate.only = TRUE)
norm_test
# Null hypothesis - sample disitrbution is normal
# -> reject null, not normal, not so good



library("vars")
##Does Consumption mobility cause prevalence?
test1 <- causality(model_var, cause = "mobility_normalised")
test1
# p=0.8426 -> fail to reject null hypotheses -> no evidence that mobility causes infections

##Does GDP growth rate Granger cause Consumption Growth rate?
test2 <- causality(model_var, cause = "prev_log_normalised")
test2
# p=0.8426 -> fail to reject null hypotheses -> no evidence that infections causes mobility

#But I probably need to re-align the time-series using the lag
# Also I have not input STATIONARY time-series - not surew how to input this (can use Ljung Box test to test startionarity)

  
  



  
  
  

# Exploring re-aligning to include all significant CCF lags ---------------


sig_lags<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Lags/ccf_prev_mobility_log_sig_lags.csv")
sig_lags
 
sig_lags$lag[which.max(sig_lags$acf)]

min_lag<-min(sig_lags$lag)
min_lag
max_lag<-max(sig_lags$lag)
max_lag
n_lags <- max_lag-min_lag
n_lags
opt_lag <-sig_lags$lag[which.max(sig_lags$acf)]

start_date_prev <- REACT_start
start_date_prev
end_date_prev <- lockdown_2_start%m+%months(-1)
end_date_prev
difftime(end_date_prev,start_date_prev,units="days")

start_date_mobility <- start_date_prev%m+%days(-opt_lag)
start_date_mobility 
end_date_mobility <- end_date_prev%m+%days(-opt_lag)
end_date_mobility
difftime(end_date_mobility,start_date_mobility,units="days")

min(workplace_ts$date)
min(london_prev_smooth_ts$d_comb) # adding 7 otherwise start date is before the start of the data


mobility_test_range <-  diff(workplace_ts$mobility_normalised[workplace_ts$date>=start_date_mobility & workplace_ts$date<end_date_mobility],lag=7)
length(mobility_test_range)
prev_test_range <- diff(min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date_prev & london_prev_smooth_ts$d_comb<end_date_prev])))
length(prev_test_range)
prev_test_range <- prev_test_range[4:(length(prev_test_range)-3)] #needed because rollmean takes of values off either side
length(prev_test_range)

# Check same lengths
length(prev_test_range)==length(mobility_test_range)

# Trying to check stationarity



acf2(mobility_test_range)
acf2(prev_test_range)

sarima(mobility_test_range,0,0,0)
sarima(prev_test_range,0,0,0)

# Not stationary looking at Ljung Box test plot even though I have differenced using various levels of differencing??

# Need the MA components to make this stationary!!!



#library(MTS)
Eccm(cbind(mobility_test_range, prev_test_range))
# Looks like AR(1)MA(1)
mod <- VARMA(cbind(mobility_test_range, prev_test_range),p=1,q=1)
mod
mod_alt <- VARMA(cbind(prev_test_range,mobility_test_range),p=1,q=1)
mod_alt

mod$coef 
mod_alt$coef
# Different - unclear which way around each model is??

summary(mod)
names(mod)
mod$coef


# Following: https://towardsdatascience.com/fun-with-arma-var-and-granger-causality-6fdd29d8391c

series_realigned <- cbind(prev_test_range, mobility_test_range)

colnames(series_realigned) <-c("prev_diff_log_normalised","mobility_diff_normalised")
head(series_realigned)

##Model selection
var_order_realigned <- VARselect(series, lag.max = 100, type = "const")
var_order_realigned$selection
# AIC suggests 28
# HQ suggests 28
# SC suggests 28
# FPE suggests 29
# -> Use 28


##Fit VAR model as suggested by VAR select information criterion - VAR(4)
model_var_realigned <- VAR(series_realigned, type = "const", p = 28, ic = "AIC")
summary(model_var_realigned)
# Mobility coefficients are not significant...

model_var_realigned$p 
model_var_realigned$K
model_var_realigned$obs



##Collect coefficient values
coef_list_realigned <- coef(model_var_realigned)
coef_list_realigned


##Method-1
##model_var is the object that contains VAR model
#Almost similar to Lagrange Multiplier test in Stata
library("vars")
method1_realigned <- serial.test(model_var_realigned, lags.pt = 50, type = "PT.asymptotic")
method1_realigned
# p=0.0001224 -> errors have serial correlation?


##Method-2
#Focused on Robust heteroskedastic errors
library("VARtests")
method2_realigned <- ACtest(fit = model_var_realigned, h = 50, HCtype = c("HC1"))
method2_realigned
# p=1 -> there aare autocorrelations in the residuals -> not good

##Optional - Normality test of errors
norm_test <- normality.test(model_var, multivariate.only = TRUE)
norm_test
# Null hypothesis - sample disitrbution is normal
# -> reject null, not normal, not so good



library("vars")
##Does Consumption mobility cause prevalence?
test1_realigned <- causality(model_var_realigned, cause = "mobility_diff_normalised")
test1_realigned
# Granger causality H0: mobility_normalised do not Granger-cause prev_log_normalised
# p=0.0174 -> reject null hypotheses -> evidence that mobility causes infections

##Does GDP growth rate Granger cause Consumption Growth rate?
test2_realigned <- causality(model_var_realigned, cause = "prev_diff_log_normalised")
test2_realigned
# Granger causality H0: prev_log_normalised do not Granger-cause mobility_normalised
# p=0.6399 -> reject null hypotheses ->  evidence that infections causes mobility


# So there is evidence that mobility causes infections when mobility is shifted back 42 days (optimal lag)
# However this again may not be stationary...



# Blog on lagged data in Python - https://medium.com/@NatalieOlivo/use-pandas-to-lag-your-timeseries-data-in-order-to-examine-causal-relationships-f8186451b3a9



# https://cran.r-project.org/web/packages/astsa/astsa.pdf
lag1.plot(series[,1], max.lag=1, corr=TRUE, smooth=TRUE, col=gray(.1),
          lwl=1, bgl ='white', box.col=8)
