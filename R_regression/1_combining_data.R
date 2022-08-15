# Combining all of the data required for R regression

# Date started: 15 August 2022




# Setup -------------------------------------------------------------------

library(tidyverse)




# Mobility data -----------------------------------------------------------

# Includes mobility raw and smoothed for England and each region:
google_england_long <- read_rds("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")

# Probably more useful to use wide as will be using multiple columns at the same time:
# This doesn't include England...
google_england_wide <- read_rds("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_wide.rds")

# Replace regions withthe longer version

google_england_wide <- google_england_wide %>%
  mutate(region = 
           case_when(region=="ENGLAND"~"England",
                     region=="EAST"~"East" ,
                     region=="EAST MIDLANDS"~"East Midlands",
                     region=="LONDON"~"London" ,
                     region=="NORTH EAST"~"North East",
                     region=="NORTH WEST"~"North West",
                     region=="SOUTH EAST"~"South East" ,
                     region=="SOUTH WEST"~"South West" ,
                     region=="WEST MIDLANDS"~"West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~"Yorkshire and The Humber" ))

# Vaccine data ------------------------------------------------------------

# This requires merging:

# England - vaccination is by publish dates
# Regional - vaccination by vaccination date

# https://coronavirus.data.gov.uk/details/vaccinations?areaType=nation&areaName=England
# For the % the denominator is the 2020 mid-year population estimate (same as I used for mobility)
# Percentages lower than 0.1 are shown as NA
# Breakdowns are available by age


vacc_england <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_england.csv")

vacc_england$date <- as.Date(vacc_england$date,"%d/%m/%Y" )

# Columns are all the same:
vacc_ee <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_ee.csv")
vacc_em <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_em.csv")
vacc_ln <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_ln.csv")
vacc_ne <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_ne.csv")
vacc_nw <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_nw.csv")
vacc_se <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_se.csv")
vacc_sw <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_sw.csv")
vacc_wm <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_wm.csv")
vacc_yh <- read_csv("~/Desktop/Masters/Project/Data/Vaccinations_gov/Raw_data/vacc_yh.csv")

# Combine all of the regional into one

vacc_regional <- rbind(vacc_ee,vacc_em, vacc_ln, vacc_ne, vacc_nw, vacc_se, vacc_sw, vacc_wm, vacc_yh)





# Combined dataset for England --------------------------------------------

google_just_england <- google_england_wide %>% filter(region=="England")
google_just_england <- google_just_england[,-1]

combined_england <- merge(google_just_england, vacc_england, by="date")


# Useful dates
lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

# Adding in lockdown indicator variables

combined_england$lockdown1 <- as.factor(ifelse(combined_england$date>=lockdown_1_start & combined_england$date<=lockdown_1_end,1,0))
combined_england$lockdown2 <- as.factor(ifelse(combined_england$date>=lockdown_2_start & combined_england$date<=lockdown_2_end,1,0))
combined_england$lockdown3 <- as.factor(ifelse(combined_england$date>=lockdown_3_start & combined_england$date<=lockdown_3_end,1,0))


# Saving combined data for England

setwd("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs")
saveRDS(combined_england, "combined_england.rds")




