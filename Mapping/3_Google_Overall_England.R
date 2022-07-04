# Creating a total England level Google mobility measure

# Date started: 3rd July 2022


# Setup 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(reshape)

# Reading mapping data
region_mapping <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/region_county_popultion_lookup.csv")  

head(region_mapping)


# Reading google data and small edits
google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")


bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                        "%Y-%m-%d")

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)

google_overall$bank_holiday <- ifelse(google_overall$date %in% bank_holidays,1,0)
google_overall$bank_holiday <- as.factor(google_overall$bank_holiday)

# Filtering just for England


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


# Adding the population and thinking about best way to aggregate...

google_england
region_mapping

google_england_combined <- merge(google_england,region_mapping,by.x="sub_region_1", by.y="Name", all.x=TRUE, all.y=TRUE)

# Check right number of rows
nrow(google_england)==nrow(google_england_combined)

# Removing random column
colnames(google_england_combined)

google_england_combined<- google_england_combined %>%
  select(-c("X.x","X.y"))


# Exploring how to aggregate mobility measures

# Total populations for regions
region_population_totals<- region_mapping %>%
  group_by(Region) %>%
  summarise(Freq=sum(Population))

region_population_totals
sum(region_population_totals$Freq)  #54 million -> not too far off populaiton of England

# Add regional populations to the overall dataset

google_england_combined <- merge(google_england_combined, region_population_totals, by.x = "Region", by.y = "Region")
View(google_england_combined)

# Rename Freq to regional_population
names(google_england_combined)[names(google_england_combined) == "Freq"] <- "regional_population"




# Loop to create combined data for England

date_range <-dput(unique(google_england_combined$date))

england_summary <-NA

for (i in 1:length(date_range)){
    google_date <- google_england_combined %>% filter(date==date_range[i])
    # Creating new variable for each date and region combination:
    retail <- sum((google_date$retail_and_recreation_percent_change_from_baseline*
                     google_date$regional_population)/sum(google_date$regional_population[!is.na(google_date$retail_and_recreation_percent_change_from_baseline)]), na.rm=TRUE)
    grocery <- sum((google_date$grocery_and_pharmacy_percent_change_from_baseline*
                      google_date$regional_population)/sum(google_date$regional_population[!is.na(google_date$grocery_and_pharmacy_percent_change_from_baseline)]), na.rm=TRUE)
    parks <- sum((google_date$parks_percent_change_from_baseline*
                    google_date$regional_population)/sum(google_date$regional_population[!is.na(google_date$parks_percent_change_from_baseline)]), na.rm=TRUE)     
    transit <- sum((google_date$transit_stations_percent_change_from_baseline*
                      google_date$regional_population)/sum(google_date$regional_population[!is.na(google_date$transit_stations_percent_change_from_baseline)]), na.rm=TRUE)    
    workplaces <- sum((google_date$workplaces_percent_change_from_baseline*
                         google_date$regional_population)/sum(google_date$regional_population[!is.na(google_date$workplaces_percent_change_from_baseline)]), na.rm=TRUE)
    residential <- sum((google_date$residential_percent_change_from_baseline*
                          google_date$regional_population)/sum(google_date$regional_population[!is.na(google_date$residential_percent_change_from_baseline)]), na.rm=TRUE) 
    
    new <- c(date_range[i],retail,grocery,parks,transit,workplaces,residential) # regions as numbers for the moment
    england_summary<-rbind(england_summary,new)
}


# Reformatting output of the loop
england_summary<-as.data.frame(england_summary)
colnames(england_summary) <-c("date","retail_recreation","grocery_pharmacy","parks","transit_stations","workplaces","residential")
england_summary$date <- as.Date(england_summary$date)
england_summary<- england_summary[-1,]

head(england_summary)

# Quick check - should be similar-ish to UK

# All look sufficiently similar for a sense check


google_uk <- google_overall %>% filter(sub_region_1=="")


england_summary %>% 
  ggplot(aes(x=date,y=retail_recreation))+
  geom_line()+
  geom_line(data=google_uk, aes(x=date,y=retail_and_recreation_percent_change_from_baseline),col="blue")


england_summary %>% 
  ggplot(aes(x=date,y=grocery_pharmacy))+
  geom_line()+
  geom_line(data=google_uk, aes(x=date,y=grocery_and_pharmacy_percent_change_from_baseline),col="blue")

england_summary %>% 
  ggplot(aes(x=date,y=parks))+
  geom_line()+
  geom_line(data=google_uk, aes(x=date,y=parks_percent_change_from_baseline),col="blue")

england_summary %>% 
  ggplot(aes(x=date,y=transit_stations))+
  geom_line()+
  geom_line(data=google_uk, aes(x=date,y=transit_stations_percent_change_from_baseline),col="blue")

england_summary %>% 
  ggplot(aes(x=date,y=workplaces))+
  geom_line()+
  geom_line(data=google_uk, aes(x=date,y=workplaces_percent_change_from_baseline),col="blue")

england_summary %>% 
  ggplot(aes(x=date,y=residential))+
  geom_line()+
  geom_line(data=google_uk, aes(x=date,y=residential_percent_change_from_baseline),col="blue")

# Add in 7-day moving average

library(lubridate)
str(england_summary$date)
england_summary <- england_summary[order(as.Date(england_summary$date, format="%Y-%m-%d")),]
head(england_summary)


england_summary <- england_summary %>%
    mutate(retail_recreation_av = rollmean(retail_recreation, k=7, fill=NA, align="center") ,
           grocery_pharmacy_av = rollmean(grocery_pharmacy, k=7, fill=NA, align="center") ,
           parks_av = rollmean(parks, k=7, fill=NA, align="center"),
           transit_stations_av = rollmean(transit_stations, k=7, fill=NA, align="center"),
           workplaces_av = rollmean(workplaces, k=7, fill=NA, align="center"),
           residential_av = rollmean(residential, k=7, fill=NA, align="center")) 



# Check looks reasonable
ggplot(data=england_summary, aes(x=date, y=retail_recreation_av)) +
  geom_line() +
  geom_line(aes(x=date,y=retail_recreation), col="blue")


# Combine with regional dataset from previous script

google_regional_wide <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_regional_wide.csv")

google_regional_wide

# Check the column names are the same
colnames(google_regional_wide)
colnames(england_summary)

# Random extra column - will add in to england to save any issues later
england_summary$X<-NA
england_summary$region <- "ENGLAND"
length(colnames(google_regional_wide))
length(colnames(england_summary))
length(colnames(google_regional_wide))==length(colnames(england_summary))

# Reorder columns to match

col_order <- dput(colnames(google_regional_wide))
england_summary <- england_summary[,col_order]
length(colnames(google_regional_wide))==length(colnames(england_summary)) # now columns of dataframes match


# Combining the two
google_england_wide <- rbind(england_summary,google_regional_wide)

# Saving the wide data
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs")  
write.csv(google_england_wide,"google_england_wide.csv")
saveRDS(google_england_wide,"google_england_wide.rds")





# Creating the equivalent long dataframe in the same way as the previous script


google_england_long <- melt(google_england_wide[,c("date", "region", "retail_recreation", "grocery_pharmacy", 
                                                  "parks", "transit_stations", "workplaces", "residential", "retail_recreation_av", 
                                                  "grocery_pharmacy_av", "parks_av", "transit_stations_av", "workplaces_av", 
                                                  "residential_av")],
                              id.vars = c("date","region"))

colnames(google_england_long) <- c("date","region","type_mobility","mobility")

head(google_england_long)

# plot to check
google_england_long %>% filter(region=="ENGLAND") %>%
  ggplot(aes(x=date,y=mobility,col=type_mobility))+ geom_line()

# Saving the data
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs")  
write.csv(google_england_wide,"google_england_long.csv")
saveRDS(google_england_wide,"google_england_long.rds")

        