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
                     region=="EAST"~"East of England" ,
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

# Replacing NAs with zeros
vacc_england$cumVaccinationFirstDoseUptakeByPublishDatePercentage <-vacc_england$cumVaccinationFirstDoseUptakeByPublishDatePercentage %>% replace_na(0)
vacc_england$cumVaccinationSecondDoseUptakeByPublishDatePercentage <-vacc_england$cumVaccinationSecondDoseUptakeByPublishDatePercentage %>% replace_na(0)
vacc_england$cumVaccinationThirdInjectionUptakeByPublishDatePercentage <-vacc_england$cumVaccinationThirdInjectionUptakeByPublishDatePercentage %>% replace_na(0)

# Checking all NAs gone"
table(vacc_england$cumVaccinationFirstDoseUptakeByPublishDatePercentage, useNA = "always")
table(vacc_england$cumVaccinationSecondDoseUptakeByPublishDatePercentage, useNA = "always")
table(vacc_england$cumVaccinationThirdInjectionUptakeByPublishDatePercentage, useNA = "always")


# Format as date
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



# REACT R  data ------------------------------------------------------------

# England
National_reproduction_R <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

# Remove the  'x' column
National_reproduction_R <- National_reproduction_R[,-1]



# Regional
regional_reproduction_R <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/regional_reproduction_R.RDS")










# Combined dataset for England --------------------------------------------

google_just_england <- google_england_wide %>% filter(region=="England")
google_just_england <- google_just_england[,-1]

combined_england <- merge(google_just_england, vacc_england, by="date", all.x=TRUE)
combined_england <- merge(combined_england, National_reproduction_R, by.x="date", by.y="d_comb", all.x = TRUE)


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

# Adding combined lockdown variable
combined_england <- combined_england %>%
  mutate(lockdown = case_when(lockdown1=="1"~"Lockdown1",
                              lockdown2=="1"~"Lockdown2",
                              lockdown3=="1"~"Lockdown3"))

combined_england$lockdown <- combined_england$lockdown %>% replace_na("No_lockdown")

combined_england$lockdown <- as.factor(combined_england$lockdown)
combined_england$lockdown <- relevel(combined_england$lockdown, ref="No_lockdown")


# Summary of all NAs
combined_england$cumVaccinationFirstDoseUptakeByPublishDatePercentage <-combined_england$cumVaccinationFirstDoseUptakeByPublishDatePercentage %>% replace_na(0)
combined_england$cumVaccinationSecondDoseUptakeByPublishDatePercentage <-combined_england$cumVaccinationSecondDoseUptakeByPublishDatePercentage %>% replace_na(0)
combined_england$cumVaccinationThirdInjectionUptakeByPublishDatePercentage <-combined_england$cumVaccinationThirdInjectionUptakeByPublishDatePercentage %>% replace_na(0)

# Checking NAs

table(combined_england$cumVaccinationFirstDoseUptakeByPublishDatePercentage, useNA = "always")
table(combined_england$cumVaccinationSecondDoseUptakeByPublishDatePercentage, useNA = "always")
table(combined_england$cumVaccinationThirdInjectionUptakeByPublishDatePercentage, useNA = "always")

# Any lockdown 

combined_england$lockdown_any <- ifelse(combined_england$lockdown=="No_lockdown",0,1)



# R>=1 or R<1 variable
combined_england$r


combined_england$r_range <- ifelse(combined_england$r>=1,">=1","<1")
combined_england$r_range <- as.factor(combined_england$r_range) 
str(combined_england$r_range)


# Saving combined data for England

setwd("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs")
saveRDS(combined_england, "combined_england.rds")





# Combined dataset for regions --------------------------------------------


vacc_regional
google_england_wide
regional_reproduction_R

# Inspecting vaccination dataset
# Renaming area to region
colnames(vacc_regional)
vacc_regional <- vacc_regional %>% rename(areaName=region)
names(vacc_regional)[names(vacc_regional) == "areaName"] <- "region"
vacc_regional
str(vacc_regional$date)
vacc_regional$date <- as.Date(vacc_regional$date, "%d/%m/%Y")
str(vacc_regional$date)


unique(vacc_regional$region)


# Inspecting mobility dataset
str(google_england_wide$date)

unique(google_england_wide$region) # Need to change East to East of England

# Removing England
google_regional_wide <- google_england_wide %>% filter(region!="England")


# Inspecting reproduction number
regional_reproduction_R
names(regional_reproduction_R)[names(regional_reproduction_R) == "reg_char"] <- "region"
names(regional_reproduction_R)[names(regional_reproduction_R) == "d_comb"] <- "date"
str(regional_reproduction_R$date)

unique(regional_reproduction_R$region)

# Merging

combined_regional <- merge(google_regional_wide, vacc_regional, by=c("date","region"),all.x=TRUE)
combined_regional <- merge(combined_regional, regional_reproduction_R, by=c("date","region"),all.x=TRUE)


combined_regional$cumVaccinationFirstDoseUptakeByVaccinationDatePercentage <-combined_regional$cumVaccinationFirstDoseUptakeByVaccinationDatePercentage %>% replace_na(0)
combined_regional$cumVaccinationSecondDoseUptakeByVaccinationDatePercentage <-combined_regional$cumVaccinationSecondDoseUptakeByVaccinationDatePercentage %>% replace_na(0)
combined_regional$cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage <-combined_regional$cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage %>% replace_na(0)

table(combined_regional$cumVaccinationFirstDoseUptakeByVaccinationDatePercentage, useNA = "always")
table(combined_regional$cumVaccinationSecondDoseUptakeByVaccinationDatePercentage, useNA = "always")
table(combined_regional$cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage, useNA = "always")
summary(combined_regional$date)



ggplot(combined_regional, aes(x=date)) +
  geom_line(aes(y=workplaces_av))+
  geom_line(aes(y=r))+
  facet_wrap(.~region)


# Adding lockdown variables


combined_regional$lockdown1 <- as.factor(ifelse(combined_regional$date>=lockdown_1_start & combined_regional$date<=lockdown_1_end,1,0))
combined_regional$lockdown2 <- as.factor(ifelse(combined_regional$date>=lockdown_2_start & combined_regional$date<=lockdown_2_end,1,0))
combined_regional$lockdown3 <- as.factor(ifelse(combined_regional$date>=lockdown_3_start & combined_regional$date<=lockdown_3_end,1,0))

# Adding combined lockdown variable
combined_regional <- combined_regional %>%
  mutate(lockdown = case_when(lockdown1=="1"~"Lockdown1",
                              lockdown2=="1"~"Lockdown2",
                              lockdown3=="1"~"Lockdown3"))

combined_regional$lockdown <- combined_regional$lockdown %>% replace_na("No_lockdown")

combined_regional$lockdown <- as.factor(combined_regional$lockdown)
combined_regional$lockdown <- relevel(combined_regional$lockdown, ref="No_lockdown")

# Single lockdown variable
combined_regional$lockdown_any <- ifelse(combined_regional$lockdown=="No_lockdown",0,1)

# R variable

combined_regional$r_range <- ifelse(combined_regional$r>=1,">=1","<1")
combined_regional$r_range <- as.factor(combined_regional$r_range) 
str(combined_regional$r_range)

# Saving combined data for regions

setwd("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs")
saveRDS(combined_regional, "combined_regional.rds")
