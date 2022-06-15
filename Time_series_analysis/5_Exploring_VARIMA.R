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


## Official cases ----------------------------------------------------------


london_cases_ts <- cases_ts %>% filter(region=="LONDON", date>=REACT_start)


# Normalisation
london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
summary(london_cases_ts$cases_normalised)

min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) # Looks like this worked












# Playing around ----------------------------------------------------------
# Following: https://towardsdatascience.com/fun-with-arma-var-and-granger-causality-6fdd29d8391c

series <- cbind(workplace_ts$mobility_normalised[workplace_ts$date>=start_date&workplace_ts$date<end_date],
            min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date&london_prev_smooth_ts$d_comb<end_date])))

colnames(series) <-c("mobility_normalised","prev_log_normalised")
head(series)

##Model selection
var_order <- VARselect(series, lag.max = 5, type = "const")
var_order$selection

##Fit VAR model as suggested by VAR select information criterion
model_var <- VAR(series, type = "const", lag.max = 10, ic = "SC")
summary(model_var)

##Collect coefficient values
coef_list <- coef(model_var)
coef_list


# Perhaps I should re-align? Also I don't want this to go both ways, just one way...