# Forecasting covid-19 - using multivariate VARIMA

# Date started: 7th June 2022


# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fpp3)

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


#  Loading CCF lag data --------------------------------------------------

CCF_mobility_cases_summary<- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series/Outputs/CCF_mobility_cases_summary.csv")
CCF_mobility_prev_summary<- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series/Outputs/CCF_mobility_prev_summary.csv")


# Finding lag for location of interest - LONDON


# Specifying conditions ---------------------------------------------------

region_of_interest <- "LONDON"
mobility_of_interest <- "workplaces"
mobility_of_interest_av <- "workplaces_av"
start_date <- lockdown_1_start
end_date <- lockdown_2_end

CCF_mobility_cases_summary %>% filter(region==region_of_interest)
lag <- CCF_mobility_cases_summary$max_lag[CCF_mobility_cases_summary$region==region_of_interest&CCF_mobility_cases_summary$type_mobility==mobility_of_interest_av]
lag # lag is 72 days in this scenario


# Loading time series tibbles ---------------------------------------------

# Note raw mobility used here, moving average will be applied as part of the ARIMA model
mobility_tibble_raw <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series/Outputs/mobility_tibble_raw.rds")
cases_tibble <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series/Outputs/cases_tibble.rds")
prev_tibble <-  readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series/Outputs/prev_tibble.rds")

# Doesn't seem to work as expected...
# multi_tibble <- merge(mobility_tibble_raw,cases_tibble, by = c("region","date")) %>% select(-bank_holiday)
# multi_tibble %>% filter(type_mobility==mobility_of_interest)
# # https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/multivariate-ts-analysis.html
# class(multi_tibble)
# multi_tibble <- ts(multi_tibble, frequency = 365)
# class(multi_tibble)
# # Why is date no longer formatted correctly - not sure how to correct this
# plot(multi_tibble)


#https://towardsdatascience.com/prediction-task-with-multivariate-timeseries-and-var-model-47003f629f9



# Auto ARIMA for mobility---------------------------------------------------------


fit_mobility <- mobility_tibble_raw %>%
  filter(type_mobility==mobility_of_interest,
         region==region_of_interest) %>%
  model(auto = ARIMA(mobility, stepwise = FALSE, approx = FALSE))

fit_mobility
# Gives ARIMA(4,0,0)(1,1,1)[7] 

glance(fit_mobility)

# Ljung Box test for stationarity
augment(fit_mobility) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=14, dof=6) # Using rule of thumb from book that lag=2m for seasonal dof=no. parameters estimated -> 6? pdqPDF



# Auto ARIMA for cases ----------------------------------------------------


fit_cases <- cases_tibble %>%
  filter(region==region_of_interest) %>%
  model(auto = ARIMA(cases, stepwise = FALSE, approx = FALSE))

fit_cases
glance(fit_cases)

# Ljung box test for stationarity
augment(fit_cases) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=14, dof=6) # Using rule of thumb from book that lag=2m for seasonal dof=no. parameters estimated -> 6? pdqPDF
# Not stationary -> shouldn't expect a stationary model though with cases??




