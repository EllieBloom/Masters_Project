# Basic time series model


# Date started: 31st May 2022


# Setup

# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fpp3)
library(slider) #for the moving averages
library(reshape) # for melt


# Loading Google data

# Loading and prepping data -----------------------------------------------


google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)


# Adding bank holidays as a field - ref: https://www.gov.uk/bank-holidays

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


google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "") # Blank sub_region_2 means at the overall level only



# Converting to tstibble (time series tibble) -----------------------------

# England regional tstibble
england_tibble <- google_england %>%
  select(-c("sub_region_2","metro_area","country_region","census_fips_code","place_id","X","iso_3166_2_code","country_region_code")) %>%
  as_tsibble(key='sub_region_1',index=date)


google_england_long <- melt(google_england[,c("retail_and_recreation_percent_change_from_baseline", 
                                          "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", 
                                          "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
                                          "residential_percent_change_from_baseline","date","sub_region_1")], id.vars=c("date","sub_region_1"))


# Renaming columns
colnames(google_england_long) <- c("date","region","type_mobility","mobility")

# Changing the mobility type fileds to be shorter

google_england_long <- google_england_long %>% mutate(type_mobility=recode(type_mobility,
                                              "retail_and_recreation_percent_change_from_baseline"="workplace", 
                                              "grocery_and_pharmacy_percent_change_from_baseline"="grocery_pharmacy",
                                              "parks_percent_change_from_baseline"="parks", 
                                              "transit_stations_percent_change_from_baseline"="transit_stations",
                                              "workplaces_percent_change_from_baseline"="workplaces", 
                                              "residential_percent_change_from_baseline"="residential"))
# Data is now tidier and easier to filter

england_tibble <- google_england_long %>%
  as_tsibble(key=c(region,type_mobility),index=date)



# Investigative plot ------------------------------------------------------

# workplace and London

england_tibble %>%
  filter(region=="Greater London") %>%
  filter(type_mobility=="workplaces") %>%
  autoplot(mobility) +
    labs(title = "Workplace mobility",
         subtitle = "London",
         y = "Change in mobility from baseline (%)",
         x= "Date")+
    theme_light()


