# Exploring the impact of NPIs on mobility

# Date started: 31st June  2022


# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(gtsummary)
library(zoo)
library(reshape2)

# Loading Google data

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)



# Important dates ---------------------------------------------------------



# Adding bank holidays as a field - ref: https://www.gov.uk/bank-holidays

bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                           "%Y-%m-%d")



# Lockdown dates

# Start date - 26/03/2020 (1st lockdown legally comes into force)
# End date - 15/06/2020 (non-essential retail reopens)

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

# Start date - 5/11/2020 (2nd lockdown legally comes into force)
# End date - 02/12/2020 (lockdown ends - into tiered system - so there are still restrictions)

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

# Start date - 06/01/2021 (3rd lockdown legally comes into force)
# End date - 12/04/2021 (non-essential retail reopens)

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")


# Creating date dependent variables ---------------------------------------


google_overall$bank_holiday <- ifelse(google_overall$date %in% bank_holidays,1,0)
google_overall$bank_holiday <- as.factor(google_overall$bank_holiday)

# Adding days of the week into the google_gb dataset
google_overall$day <- weekdays(google_overall$date)

# Make into a factor
google_overall$day <- factor(google_overall$day)

# Sunday as reference category
google_overall$day <- relevel(google_overall$day, ref="Sunday")
str(google_overall$day)


# Adding a variable if in a lockdown or not

google_overall$lockdown1 <- as.factor(ifelse(google_overall$date>=lockdown_1_start & google_overall$date<=lockdown_1_end,1,0))
google_overall$lockdown2 <- as.factor(ifelse(google_overall$date>=lockdown_2_start & google_overall$date<=lockdown_2_end,1,0))
google_overall$lockdown3 <- as.factor(ifelse(google_overall$date>=lockdown_3_start & google_overall$date<=lockdown_3_end,1,0))

# Filterting for GB dataset
google_gb <- google_overall %>% filter(sub_region_1=="") # Where there is no sub-region, the data is for GB overall


# Moving average at GB level ----------------------------------------------

google_gb$retail_and_recreation_rolling_av =
  rollmean(google_gb$retail_and_recreation_percent_change_from_baseline, k=7, fill=NA, align="center")

google_gb$grocery_and_pharmacy_rolling_av =
  rollmean(google_gb$grocery_and_pharmacy_percent_change_from_baseline, k=7, fill=NA, align="center")

google_gb$parks_rolling_av =
  rollmean(google_gb$parks_percent_change_from_baseline, k=7, fill=NA, align="center")

google_gb$transit_stations_rolling_av =
  rollmean(google_gb$transit_stations_percent_change_from_baseline, k=7, fill=NA, align="center")

google_gb$workplaces_rolling_av =
  rollmean(google_gb$workplaces_percent_change_from_baseline, k=7, fill=NA, align="center")

google_gb$residential_rolling_av =
  rollmean(google_gb$residential_percent_change_from_baseline, k=7, fill=NA, align="center")



# Filter for lockdown dates ------------------------------------------------


google_gb_lockdowns <- google_gb %>% filter(date>=lockdown_1_start, date<=lockdown_3_end)

google_gb_lockdowns$days_since_start <- google_gb_lockdowns$date - lockdown_1_start




# Linear model with moving average -------------------------------------

# Not including bank holiday as now moving average so cannot pin-point to one day as well

workplace_model <- lm(workplaces_rolling_av ~ 
                          + days_since_start  +
                        lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start,
                      data=google_gb_lockdowns )

tbl_regression(workplace_model)

# Other mobility measures

# Grocery and pharmacy
grocery_model <- lm(grocery_and_pharmacy_rolling_av ~ 
                        + days_since_start + 
                        lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start,
                      data=google_gb_lockdowns )

tbl_regression(grocery_model)

# Retail and recreation

retail_model <- lm(retail_and_recreation_rolling_av ~ 
                     + days_since_start + 
                     lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start,
                   data=google_gb_lockdowns )

tbl_regression(retail_model)

# Transit stations

transit_model <- lm(transit_stations_rolling_av ~ 
                      + days_since_start + 
                      lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start,
                    data=google_gb_lockdowns )

tbl_regression(transit_model)

# Residential 

residential_model <- lm(residential_rolling_av ~ 
                     + days_since_start +
                     lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start,
                   data=google_gb_lockdowns )

tbl_regression(residential_model)

# Parks - DO NOT USE THIS, VERY MUCH A SEASONAL PATTERN

parks_model <- lm(parks_rolling_av ~ 
                          + days_since_start +
                          lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start,
                        data=google_gb_lockdowns )

tbl_regression(parks_model)


