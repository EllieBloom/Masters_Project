# Converting Google data to regional level

# Date started: 1st June 2022

# Use weighted average to convert % change in mobility at county level to regional level to match REACT data







# Setup 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(reshape)

# Reading mapping data
region_mapping <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/region_county_popultion_lookup.csv")  

region_mapping


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




# Weighted average for mobility

# google_england_combined$parks_regional <- google_england_combined$parks_percent_change_from_baseline *
# 
# # note there will be NAs...
# google_england_weighted <- google_england_combined %>%
#                                 mutate(retail_recreation_weighted = (retail_and_recreation_percent_change_from_baseline*Population)) %>%
#                                 mutate(grocery_pharmacy_weighted = (grocery_and_pharmacy_percent_change_from_baseline*Population)) %>%
#                                 mutate(parks_weighted = (parks_percent_change_from_baseline*Population)/regional_population) %>%
#                                 mutate(transit_stations_weighted = (transit_stations_percent_change_from_baseline*Population)) %>%
#                                 mutate(workplaces_weighted = (workplaces_percent_change_from_baseline*Population)) %>%
#                                 mutate(residential_weighted = (residential_percent_change_from_baseline*Population)) 
# 
# # The NAs do matter!! they should be included in the population sum...! Come back to this - shouldn't be dividing by that population when the mobility is NA
# 
# # Example of how to get weighted retail overall 
# retail_regional <- sum((google_england_combined$retail_and_recreation_percent_change_from_baseline*
#                               google_england_combined$Population)/sum(google_england_combined$Population[!is.na(google_england_combined$retail_and_recreation_percent_change_from_baseline)]), na.rm=TRUE)
# 
# retail_regional

region_list <- sort(unique(region_mapping$Region))
regional_summary <- NA
date_range <-dput(unique(google_england_combined$date))

# Loop

for (i in 1:length(region_list)){
  regional <- google_england_combined %>% filter(Region==region_list[i])
    for (j in 1:length(date_range)){
      regional_date <- regional %>% filter(date==date_range[j])
      # Creating new variable for each date and region combination:
      retail <- sum((regional_date$retail_and_recreation_percent_change_from_baseline*
                       regional_date$regional_population)/sum(regional_date$regional_population[!is.na(regional_date$retail_and_recreation_percent_change_from_baseline)]), na.rm=TRUE)
      grocery <- sum((regional_date$grocery_and_pharmacy_percent_change_from_baseline*
                       regional_date$regional_population)/sum(regional_date$regional_population[!is.na(regional_date$grocery_and_pharmacy_percent_change_from_baseline)]), na.rm=TRUE)
      parks <- sum((regional_date$parks_percent_change_from_baseline*
                        regional_date$regional_population)/sum(regional_date$regional_population[!is.na(regional_date$parks_percent_change_from_baseline)]), na.rm=TRUE)     
      transit <- sum((regional_date$transit_stations_percent_change_from_baseline*
                      regional_date$regional_population)/sum(regional_date$regional_population[!is.na(regional_date$transit_stations_percent_change_from_baseline)]), na.rm=TRUE)    
      workplaces <- sum((regional_date$workplaces_percent_change_from_baseline*
                        regional_date$regional_population)/sum(regional_date$regional_population[!is.na(regional_date$workplaces_percent_change_from_baseline)]), na.rm=TRUE)
      residential <- sum((regional_date$residential_percent_change_from_baseline*
                           regional_date$regional_population)/sum(regional_date$regional_population[!is.na(regional_date$residential_percent_change_from_baseline)]), na.rm=TRUE) 
      
      new <- c(date_range[j],i,retail,grocery,parks,transit,workplaces,residential) # regions as numbers for the moment
      regional_summary<-rbind(regional_summary,new)
    }
  
}






# Reformatting output of the loop
regional_summary<-as.data.frame(regional_summary)
colnames(regional_summary) <-c("date","region","retail_recreation","grocery_pharmacy","parks","transit_stations","workplaces","residential")
regional_summary$date <- as.Date(regional_summary$date)
regional_summary<- regional_summary[-1,]

# Replacing with region names

regional_summary <- regional_summary %>% 
  mutate(region = 
           case_when(region==1~"EAST" ,
                     region==2~"EAST MIDLANDS" ,
                     region==3~"LONDON" ,
                     region==4~"NORTH EAST",
                     region==5~"NORTH WEST",
                     region==6~"SOUTH EAST" ,
                     region==7~"SOUTH WEST" ,
                     region==8~"WEST MIDLANDS",
                     region==9~"YORKSHIRE AND THE HUMBER" ))



# Rolling averages needed too, need different for each region

region_list <- dput(unique(regional_summary$region))
df_combined <- NA

# Loop to add rolling averages
for (i in 1:length(region_list)){
  df_region <- regional_summary %>% filter(region==region_list[i])
  df_region <- df_region %>%
    mutate(retail_recreation_av = rollmean(retail_recreation, k=7, fill=NA, align="center") ,
           grocery_pharmacy_av = rollmean(grocery_pharmacy, k=7, fill=NA, align="center") ,
           parks_av = rollmean(parks, k=7, fill=NA, align="center"),
           transit_stations_av = rollmean(transit_stations, k=7, fill=NA, align="center"),
           workplaces_av = rollmean(workplaces, k=7, fill=NA, align="center"),
           residential_av = rollmean(residential, k=7, fill=NA, align="center")) 
  
  df_combined <- rbind(df_combined, df_region)
}

df_combined <- df_combined[-1,]
regional_summary <- df_combined

# Plotting to check

ggplot(data=regional_summary, aes(x=date,y=residential_av,col=region))+
  geom_line()


# Saving the wide dataset

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs")  
write.csv(regional_summary,"google_regional_wide.csv")


# Creating a long version to plot all combinations

regional_summary_long <- melt(regional_summary[,c("date", "region", "retail_recreation", "grocery_pharmacy", 
                                                  "parks", "transit_stations", "workplaces", "residential", "retail_recreation_av", 
                                                  "grocery_pharmacy_av", "parks_av", "transit_stations_av", "workplaces_av", 
                                                  "residential_av")],
                                                id.vars = c("date","region"))

colnames(regional_summary_long) <- c("date","region","type_mobility","mobility")


# Plotting faceted version of the long data


# labels_list <- as_labeller( c("retail_recreation"="Retail and recreation", 
#                               "grocery_pharmacy"="Grocery and pharmacy", 
#                               "parks"="Parks", 
#                               "transit_stations"="Transit stations", 
#                               "workplaces"="Workplaces", 
#                               "residential"="Residential"))
# 
# 
# plot_regions_mobility <-ggplot(data=regional_summary_long, aes(x=date,y=mobility,col=factor(type_mobility)))+
#                         geom_line() +
#                         facet_wrap(.~region, scales = "free") +
#                         scale_x_date(breaks = function(x) seq.Date(from = min(x), 
#                                                                    to = max(x), 
#                                                                    by = "1 year"),
#                                      minor_breaks = function(x) seq.Date(from = min(x), 
#                                                                          to = max(x), 
#                                                                          by = "6 months"),
#                                      date_labels="%Y") +
#                         theme_light() +
#                         labs(x="Date",y="Mobility change from baseline (%)",
#                              title = "Google mobility mapped to English regions")+
#                         theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
#                               legend.title=element_blank(),
#                               axis.title.x=element_text(size=10),
#                               strip.background=element_rect(color="white", fill="white"),
#                               strip.text=element_text(color="black", size=10, face="bold"))+
#                         scale_color_hue(labels = labels_list) 
# 
# plot_regions_mobility 
# 
# setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs")
# ggsave("Google_regions_wrap.pdf", plot=plot_regions_mobility)
  
# Saving the wide dataset

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs")  
write.csv(regional_summary_long,"google_regional_long.csv")


# Could also do the rolling average here for plotting...