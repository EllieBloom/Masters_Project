# Mapping Google counties to REACT regions for prevalence

# Date started: 1st June 2022

# Script to map Google counties (metropolitan and non-metrpolitan counties) to the REACT regions used for daily spline prevalence
# Mapping to region, and to the population so that the Google mobility at county can be aggregated up to regional level to match REACT
# Note: uses mid-2020 populations from ONS, found here:
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland


# Setup:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(readxl)


# Reading data
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
populations <- read_excel("/Users/elliebloom/Desktop/Masters/Project/Data/Population/ukpopestimatesmid2020on2021geography.xls", sheet = "MYE2 - Persons")

# Tidying data
View(populations)
colnames(populations)<-populations[7,]
populations <- populations[-c(1:7),]
View(populations)

# Only need the overall population
populations <- populations[,c("Name","Geography","All ages")]
populations$'All ages' <- as.numeric(populations$'All ages')

# English counties in google data:

england_counties <- c("Bath and North East Somerset","Bedford", "Blackburn with Darwen",
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



# Checking which counties are in the populations spreadsheet
table(england_counties %in% populations$Name)
# Only 12 don't appear - let's look at these 12

# Some may be mapped onto the wrong level - shouldn't be at region level

# Check again
table(england_counties %in% populations$Name[populations$Geography!="Region"])

# Still just 12 missing

england_counties[-c(which(england_counties %in% populations$Name))]
mismatches <- dput(england_counties[-c(which(england_counties %in% populations$Name))])

# Now have a list of the counties that don't appear - look manually

# "Borough of Halton" - maps to "Halton"
# "Bristol City" - maps to "Bristol, City of"
# "Greater London" - maps to "LONDON" (although not needed as this is a region)
# "Greater Manchester" - maps to "Greater Manchester (Met County)"
# "Herefordshire" - maps to "Herefordshire, County of"
# "Kingston upon Hull" - maps to "Kingston upon Hull, City of"
# "Merseyside" - maps to "Merseyside (Met County)"
# "Northamptonshire"- combination of "North Northamptonshire" and "West Northamptonshire"
# "South Yorkshire" - maps to "South Yorkshire (Met County)"
# "Tyne and Wear" - maps to "Tyne and Wear (Met County)"
# "West Midlands" - maps to "West Midlands (Met County)"
# "West Yorkshire" - maps to "West Yorkshire (Met County)"

# Try left join

england_counties <- as.data.frame(england_counties)
colnames(england_counties) <- "Name"

merged_data <- merge(england_counties, populations, by="Name", all.x=TRUE)

nrow(merged_data) # 85 as expected
merged_data

# Manually add each of the mismatches:
mismatches

# In order, manually update
merged_data$'All ages'[merged_data$Name==mismatches[1]] <- populations$'All ages'[populations$Name=="Halton"]
merged_data$'All ages'[merged_data$Name==mismatches[2]] <- populations$'All ages'[populations$Name=="Bristol, City of"]
merged_data$'All ages'[merged_data$Name==mismatches[3]] <- populations$'All ages'[populations$Name=="LONDON"]
merged_data$'All ages'[merged_data$Name==mismatches[4]] <- populations$'All ages'[populations$Name=="Greater Manchester (Met County)"]
merged_data$'All ages'[merged_data$Name==mismatches[5]] <- populations$'All ages'[populations$Name=="Herefordshire, County of"]
merged_data$'All ages'[merged_data$Name==mismatches[6]] <- populations$'All ages'[populations$Name=="Kingston upon Hull, City of"]
merged_data$'All ages'[merged_data$Name==mismatches[7]] <- populations$'All ages'[populations$Name=="Merseyside (Met County)"]
merged_data$'All ages'[merged_data$Name==mismatches[8]] <- populations$'All ages'[populations$Name=="North Northamptonshire"]+
                                                           populations$'All ages'[populations$Name=="West Northamptonshire"]
merged_data$'All ages'[merged_data$Name==mismatches[9]] <- populations$'All ages'[populations$Name=="South Yorkshire (Met County)"]  
merged_data$'All ages'[merged_data$Name==mismatches[10]]<- populations$'All ages'[populations$Name=="Tyne and Wear (Met County)"]  
merged_data$'All ages'[merged_data$Name==mismatches[11]]<- populations$'All ages'[populations$Name=="West Midlands (Met County)"]  
merged_data$'All ages'[merged_data$Name==mismatches[12]]<- populations$'All ages'[populations$Name=="West Yorkshire (Met County)"]  

# Try to add in corresponding region - note I created this spreadsheet from the populations spreadsheet in excel

regions_mapping <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Population/region_mapping.csv")
regions_mapping

merged_data_final <- merge(merged_data, regions_mapping[,c("Name","Region")], by="Name", all.x=TRUE)
nrow(merged_data_final)

# Adding in missing regions manually
merged_data_final$Region[merged_data_final$Name==mismatches[1]] <- regions_mapping$Region[regions_mapping$Name=="Halton"]
merged_data_final$Region[merged_data_final$Name==mismatches[2]] <- regions_mapping$Region[regions_mapping$Name=="Bristol, City of"]
merged_data_final$Region[merged_data_final$Name==mismatches[3]] <- regions_mapping$Region[regions_mapping$Name=="LONDON"]
merged_data_final$Region[merged_data_final$Name==mismatches[4]] <- regions_mapping$Region[regions_mapping$Name=="Greater Manchester (Met County)"]
merged_data_final$Region[merged_data_final$Name==mismatches[5]] <- regions_mapping$Region[regions_mapping$Name=="Herefordshire, County of"]
merged_data_final$Region[merged_data_final$Name==mismatches[6]] <- regions_mapping$Region[regions_mapping$Name=="Kingston upon Hull, City of"]
merged_data_final$Region[merged_data_final$Name==mismatches[7]] <- regions_mapping$Region[regions_mapping$Name=="Merseyside (Met County)"]
merged_data_final$Region[merged_data_final$Name==mismatches[8]] <- regions_mapping$Region[regions_mapping$Name=="North Northamptonshire"]
merged_data_final$Region[merged_data_final$Name==mismatches[9]] <- regions_mapping$Region[regions_mapping$Name=="South Yorkshire (Met County)"]  
merged_data_final$Region[merged_data_final$Name==mismatches[10]]<- regions_mapping$Region[regions_mapping$Name=="Tyne and Wear (Met County)"]  
merged_data_final$Region[merged_data_final$Name==mismatches[11]]<- regions_mapping$Region[regions_mapping$Name=="West Midlands (Met County)"]  
merged_data_final$Region[merged_data_final$Name==mismatches[12]]<- regions_mapping$Region[regions_mapping$Name=="West Yorkshire (Met County)"] 

# Add in abbreviations to match REACT data

merged_data_final <- merged_data_final %>% 
                    mutate(region_code = 
                             case_when(Region=="EAST" ~ "EE",
                                       Region=="EAST MIDLANDS" ~ "EM",
                                       Region=="LONDON" ~ "LN",
                                       Region=="NORTH EAST" ~ "NE",
                                       Region=="NORTH WEST" ~ "NW",
                                       Region=="SOUTH EAST" ~ "SE",
                                       Region=="SOUTH WEST" ~ "SW",
                                       Region=="WEST MIDLANDS" ~ "WM",
                                       Region=="YORKSHIRE AND THE HUMBER" ~"YH"))

# Renaming the population column

colnames(merged_data_final) <- c("Name", "Geography", "Population", "Region", "region_code")


# Export csv 
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs")  
write.csv(merged_data_final,"region_county_popultion_lookup.csv")
                    
          
          
