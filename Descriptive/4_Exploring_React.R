## Exploring REACT and postcode mapping data samples

# Date started: 23rd May 2022

# Exploring REACT data fields and mapping of locations/postcodes to other areas 
# e.g. Google Metrpolitan and Non-Metrpolitan Counties in Engalnd

# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Reading data

R17 <- read_rds("/Users/elliebloom/Desktop/Masters/Project/Data/REACT_sample/R17_shuffle_permute_Ellie.rds")
R18 <- read_rds("/Users/elliebloom/Desktop/Masters/Project/Data/REACT_sample/R18_shuffle_permute_Ellie.rds")
R19 <- read_rds("/Users/elliebloom/Desktop/Masters/Project/Data/REACT_sample/R19_shuffle_permute_Ellie.rds")

# Exploring

colnames(R17)
# Final result column

table(R17$final_result)
# Results either Detected, Not Detected or Void

table(R17$final_result)/sum(table(R17$final_result))
# Raw prevalence of 4.3%

# Looking at possible location mapping - variables of interest

location_vars <- c("region_id", "lacode", 
                   "lsoa", "output_area", "u_region", "postcode")

# Region ID - 1(NE), 2(NW), 3(Yorkshire and Humber), 4(EM), 5(WM), 6(EE), H(LON), J(SE)
table(R17$region_id)

# lacode - not sure what each of these mean...?
table(R17$lacode)

# LSOA - not sure what each of these mean - similar to lacode
table(R17$lsoa)

# output_area - not sure what each of these mean - similar to lacode and LSOA
table(R17$output_area)

# u_region - same as region but with names of regions as well as codes - easier to use
table(R17$u_region)

# postcode
table(R17$postcode)
# May be able to use a postcode lookup to map between areas:
# https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/ukpostcodestownsandcounties

# https://www.ons.gov.uk/methodology/geography/ukgeographies/administrativegeography/england

# Overall postcode lookup is very large!
postcode_lookup <-read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/ONS_postcode_lookup/Data/nhg19nov.csv", header = FALSE)

colnames(postcode_lookup)

table(postcode_lookup$V9)
# E06000001 - E06000059 = England (UA); 
# E07000004 - E07000246 = England (LAD); 
# E08000001 - E08000037 = England (MD); 
# E09000001 - E09000033 = England (LB); 
# W06000001 - W06000024 = Wales (UA); 
# S12000005 - S12000050 = Scotland (CA); 
# N09000001 – N09000011 = Northern Ireland (DCA); 
# L99999999 (pseudo) = Channel Islands;
# M99999999 (pseudo) = Isle of Man; 
# null = no information available