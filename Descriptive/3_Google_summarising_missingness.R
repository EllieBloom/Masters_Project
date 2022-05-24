# Summarising missingness in the google mobility dataset

# Date started: 19th May 2022


# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Loading Google data (createdf in script 1_Google_exploration_measures.R)

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")


# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)



# Regional dataset --------------------------------------------------------


google_overall
unique(google_overall$sub_region_1)
length(unique(google_overall$sub_region_1)) # 152 regions - need to define which are England

england_sub_region_1 <- c("Bath and North East Somerset","Bedford", "Blackburn with Darwen",
                          "Blackpool", "Borough of Halton","Bracknell Forest","Brighton and Hove", 
                          "Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire",
                          "Cheshire East","Cheshire West and Chester","Cornwall","County Durham",
                          "Cumbria", "Darlington","Derby","Derbyshire","Devon", "Dorset",
                          "East Riding of Yorkshire", "East Sussex", "Essex","Gloucestershire",
                          "Greater London","Greater Manchester","Hampshire" , "Hartlepool",
                          "Herefordshire", "Isle of Wight", "Kent", "Kingston upon Hull",
                          "Lancashire", "Leicester","Leicestershire", "Lincolnshire","Luton",
                          "Medway", "Merseyside", "Middlesbrough","Milton Keynes", "Norfolk",
                          "North East Lincolnshire", "North Lincolnshire", "North Somerset", 
                          "North Yorkshire", "Northamptonshire", "Northumberland", "Nottingham",
                          "Nottinghamshire", "Oxfordshire", "Peterborough", "Plymouth","Portsmouth",
                          "Reading", "Redcar and Cleveland", "Rutland", "Shropshire", "Slough",
                          "Somerset", "South Gloucestershire", "South Yorkshire", "Southampton",
                          "Southend-on-Sea", "Staffordshire", "Stockton-on-Tees", "Stoke-on-Trent",
                          "Suffolk", "Surrey", "Swindon", "Thurrock", "Torbay","Tyne and Wear",
                          "Warrington", "Warwickshire", "West Berkshire", "West Midlands",
                          "West Sussex", "West Yorkshire", "Wiltshire", "Windsor and Maidenhead",
                          "Wokingham", "Worcestershire","York")

length(england_sub_region_1) # 85 regions in England

google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "")



# Summarising missingness -------------------------------------------------

library(naniar)
library(UpSetR)

# Can include a max of 5 variables - so excluded parks - the most missing variable
missingness_overlap_plot <- gg_miss_upset(google_england[,c(10:11,13:15)])



na_summary<-NA

for (i in 1:length(england_sub_region_1)){
  google_region<-NA
  google_region <- google_england %>% filter(sub_region_1==england_sub_region_1[i])
  missing_table<-as.data.frame(miss_var_summary(google_region))
  colnames(missing_table)<-c("Variable",england_sub_region_1[i],england_sub_region_1[i])
  missing_table<-missing_table[-1]
  na_summary<-cbind(na_summary,missing_table)
}

rownames(na_summary)<-c("metro_area", "census_fips_code", 
                        "parks_percent_change_from_baseline", "residential_percent_change_from_baseline", 
                        "transit_stations_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", 
                        "retail_and_recreation_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
                        "country_region_code", "country_region", "sub_region_1", "sub_region_2", 
                        "iso_3166_2_code", "place_id", "date")

# Only interested in baseline change summary data
na_summary <- na_summary[3:8,]

# Remove first empty column
na_summary <- na_summary[,-1] 

# Transpose
na_summary <-t(na_summary)

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google")
write.csv(na_summary,"Google_summary_missingness_regional.csv")

# Could use linear interpolation of missing points as per Imperial paper 26:
# https://www.nature.com/articles/s41467-021-21358-2#Sec17
# Method: 
# https://www.statology.org/r-interpolate-missing-values/


