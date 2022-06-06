# Calculating the lag between mobility and covid-19 for different combinations

# Date started: 6th June 2022


# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fpp3)
library(slider) #for the moving averages
library(reshape) # for melt
library(gtsummary) # for tbl_regression

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



# Data preparation --------------------------------------------------------

# Creating 5 datasets:
# 1. mobility_tibble -> includes both raw and moving average mobility
# 2. mobility_tibble_raw -> only includes raw mobility
# 3. mobility_tibble_av -> only includes 7-day centered rolling average mobility
# 4. cases_tibble -> official covid-19 cases in England by specimen date
# 5. prev_tibble -> REACT-1 B-Spline daily prevalence by region



## Loading mobility data --------------------------------------------------------

google_regional <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_regional_long.csv")

# List of
google_regional <- google_regional[,-1]

# Converting dates from string format to date
google_regional$date <- as.Date(google_regional$date,format="%Y-%m-%d")


# ref: https://www.gov.uk/bank-holidays
google_regional$bank_holiday <- ifelse(google_regional$date %in% bank_holidays,1,0)
google_regional$bank_holiday <- as.factor(google_regional$bank_holiday)

# Creating list of regions to use elswhere
region_list <- unique(google_regional$region)

# Making it into a tstibble


mobility_tibble <- google_regional %>%
  as_tsibble(key=c(region, type_mobility),index=date)


# Raw data tibble only

mobility_tibble_raw <- mobility_tibble %>%
  filter(!grepl('av',type_mobility))


# Rolling average tibble only

mobility_tibble_av <- mobility_tibble %>%
  filter(grepl('av',type_mobility))



# Plot to check - raw

labels_list_raw <- as_labeller( c("retail_recreation"="Retail and recreation", 
                              "grocery_pharmacy"="Grocery and pharmacy", 
                              "parks"="Parks", 
                              "transit_stations"="Transit stations", 
                              "workplaces"="Workplaces", 
                              "residential"="Residential"))

plot_regions_mobility_raw <- ggplot(data=mobility_tibble_raw, aes(x=date,y=mobility,col=factor(type_mobility)))+
  geom_line() +
  facet_wrap(.~region, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 year"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "6 months"),
               date_labels="%Y") +
  theme_light() +
  labs(x="Date",y="Mobility change from baseline (%)",
       title = "Google mobility mapped to English regions")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.x=element_text(size=10),
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", size=10, face="bold"))+
  scale_color_hue(labels = labels_list_raw) 

plot_regions_mobility_raw 

# Plot to check - moving average

labels_list_av <- as_labeller( c("retail_recreation_av"="Retail and recreation", 
                                  "grocery_pharmacy_av"="Grocery and pharmacy", 
                                  "parks_av"="Parks", 
                                  "transit_stations_av"="Transit stations", 
                                  "workplaces_av"="Workplaces", 
                                  "residential_av"="Residential"))

plot_regions_mobility_av <- ggplot(data=mobility_tibble_av, aes(x=date,y=mobility,col=factor(type_mobility)))+
  geom_line() +
  facet_wrap(.~region, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 year"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "6 months"),
               date_labels="%Y") +
  theme_light() +
  labs(x="Date",y="Mobility change from baseline (%)",
       title = "Google mobility mapped to English regions")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.x=element_text(size=10),
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", size=10, face="bold"))+
  scale_color_hue(labels = labels_list_av) 

plot_regions_mobility_av



## Loading official cases data ------------------------------------------------------
# https://coronavirus.data.gov.uk/details/download


mypath <- "/Users/elliebloom/Desktop/Masters/Project/Data/Gov_cases"
filenames=list.files(path=mypath, full.names=TRUE)

library(vroom)
df_cases <- vroom(filenames)

df_cases <- df_cases %>% select (-c(areaCode, areaType))

df_cases <- df_cases %>% mutate(areaName)

df_cases <- df_cases %>% 
  mutate(areaName = 
           case_when(areaName=="East of England" ~"EAST" ,
                     areaName=="East Midlands"~"EAST MIDLANDS" ,
                     areaName=="London"~"LONDON" ,
                     areaName=="North East"~"NORTH EAST",
                     areaName=="North West"~"NORTH WEST",
                     areaName=="South East"~"SOUTH EAST" ,
                     areaName=="South West"~"SOUTH WEST" ,
                     areaName=="West Midlands"~"WEST MIDLANDS",
                     areaName=="Yorkshire and The Humber"~"YORKSHIRE AND THE HUMBER" ))

# Rename area name and cases as region to make consistent/simple
colnames(df_cases) <- c("region","date","cases")

df_cases$date<-as.Date(df_cases$date,format="%d/%m/%Y")

cases_tibble <- df_cases %>%
  as_tsibble(key=region,index=date)


# Plot to check

plot_regions_cases <- ggplot(data=cases_tibble, aes(x=date,y=cases))+
  geom_line(col="#006EAF") +
  facet_wrap(.~region) + # removed scales="free"
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 year"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "6 months"),
               date_labels="%Y") +
  theme_light() +
  labs(x="Date",y="Cases (by specimen date)",
       title = "Official covid-19 cases in England")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.x=element_text(size=10),
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", size=10, face="bold"))+
  scale_color_hue(labels = labels_list) 

plot_regions_cases 


## Loading REACT prevalence data -------------------------------------------

prevpath <- "/Users/elliebloom/Desktop/Masters/Project/Data/REACT_prevalence"

prevfilenames <- list.files(path=prevpath, full.names=TRUE)

df_prev <- NA

for (i in 1:length(prevfilenames)){
  df<-read_rds(prevfilenames[i])
  df$region <- region_list[i]
  print(ncol(df))
  df_prev <- rbind(df_prev,df)
}

df_prev <- df_prev[-1,]
df_prev$date <- rownames(df_prev)
df_prev$date <- as.Date(df_prev$date,format="%Y-%m-%d")

prev_tibble <- df_prev %>% select(c(p,lb,ub,date,region)) %>%
                    as_tsibble(key=region,index=date)

# Plot to check
 
plot_regions_prev <- ggplot(data=prev_tibble, aes(x=date,y=p*100))+
  geom_line(col="#02893B") +
  facet_wrap(.~region,scales="free") + 
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 year"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "6 months"),
               date_labels="%Y") +
  theme_light() +
  labs(x="Date",y="Prevalence (%)",
       title = "REACT B-Spline covid-19 prevalence in England")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.x=element_text(size=10),
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", size=10, face="bold"))+
  scale_color_hue(labels = labels_list) 

plot_regions_prev 







# CCF breakdown -----------------------------------------------------------


