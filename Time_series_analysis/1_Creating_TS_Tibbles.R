# Creating time series tibble to use for raw data

# Date started: 8th June 2022

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fpp3)




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

labels_list <- as_labeller( c("retail_recreation"="Retail and recreation", 
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
  scale_color_hue(labels = labels_list) 

plot_regions_mobility_raw 
setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
ggsave("plot_regions_mobility_raw.pdf",plot_regions_mobility_raw, device="pdf")

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
setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
ggsave("plot_regions_mobility_av.pdf",plot_regions_mobility_av, device="pdf")



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
        strip.text=element_text(color="black", size=10, face="bold"))

plot_regions_cases 
setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
ggsave("plot_regions_cases.pdf",plot_regions_cases , device="pdf")



## Loading REACT point estimate prevalence data --------------------------------

prevpath <- "/Users/elliebloom/Desktop/Masters/Project/Data/REACT_prevalence/point_estimates"

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
       title = "REACT points estimates COVID-19 prevalence in England")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.x=element_text(size=10),
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", size=10, face="bold"))

plot_regions_prev 
setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
ggsave("plot_regions_prev.pdf",plot_regions_prev,device="pdf")


## Loading REACT smoothed prevalence data --------------------------------

# Updated data - one point per day

regional_prevalence <- readRDS("~/Desktop/Masters/Project/Data/REACT_prevalence/regional_prevalence.RDS")

# Change the names of the regions to match google
regional_prevalence <- regional_prevalence %>% 
  mutate(region = 
           case_when(region=="EE" ~"EAST" ,
                     region=="EM"~"EAST MIDLANDS" ,
                     region=="LN"~"LONDON" ,
                     region=="NE"~"NORTH EAST",
                     region=="NW"~"NORTH WEST",
                     region=="SE"~"SOUTH EAST" ,
                     region=="SW"~"SOUTH WEST" ,
                     region=="WM"~"WEST MIDLANDS",
                     region=="YH"~"YORKSHIRE AND THE HUMBER" ))


# Inverse logit the relevant columns

library(pubh)

regional_prevalence[ ,2:6] <-
  inv_logit(regional_prevalence[ ,2:6])


# Creating the ts  tibble
prev_smooth_tibble <- regional_prevalence %>%
  as_tsibble(key=region,index=d_comb)

plot_regions_prev_smooth <- ggplot(data=prev_smooth_tibble, aes(x=d_comb,y=p*100))+
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
        strip.text=element_text(color="black", size=10, face="bold"))

plot_regions_prev_smooth

setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
ggsave("plot_regions_prev_smooth.pdf",plot_regions_prev_smooth,device="pdf")

plot_regions_prev_smooth_overlay <- ggplot(data=prev_smooth_tibble, aes(x=d_comb,y=p*100, color=region))+
  geom_line() +
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
        strip.text=element_text(color="black", size=10, face="bold"))

plot_regions_prev_smooth_overlay
setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
ggsave("plot_regions_prev_smooth_overlay.pdf",plot_regions_prev_smooth_overlay,device="pdf")




# Old data - had ten points per days
#prevsmoothpath <- "/Users/elliebloom/Desktop/Masters/Project/Data/REACT_prevalence/b_splines"
# 
# prevsmoothfilenames <- list.files(path=prevsmoothpath, full.names=TRUE)
# 
# df_prev_smooth <- NA
# 
# for (i in 1:length(prevsmoothfilenames)){
#   df<-readRDS(prevsmoothfilenames[i])
#   df$region <- region_list[i]
#   print(ncol(df))
#   df_prev_smooth <- rbind(df_prev_smooth,df)
# }
# 
# df_prev_smooth <- df_prev_smooth[-1,]
# str(df_prev_smooth$d_comb)
# df_prev_smooth$d_comb <- as.Date(df_prev_smooth$d_comb,format="%Y-%m-%d")
# 
# # inverse logit the prevalence figures
# df_prev_smooth
# 
# 
# 
# library(pubh)
# 
# df_prev_smooth[,c("p", "lb_2.5", "lb_5",
#                   "lb_25", "ub_97.5", "ub_95", "ub_75" )] <-
#   inv_logit(df_prev_smooth[,c("p", "lb_2.5", "lb_5",
#                     "lb_25", "ub_97.5", "ub_95", "ub_75" )])
# 
# prev_smooth_tibble <- df_prev_smooth %>%
#   as_tsibble(key=region,index=d_comb)x
# 
# # Plot to check
# 
# plot_regions_prev_smooth <- ggplot(data=prev_smooth_tibble, aes(x=d_comb,y=p*100))+
#   geom_line(col="#02893B") +
#   facet_wrap(.~region,scales="free") + 
#   scale_x_date(breaks = function(x) seq.Date(from = min(x), 
#                                              to = max(x), 
#                                              by = "1 year"),
#                minor_breaks = function(x) seq.Date(from = min(x), 
#                                                    to = max(x), 
#                                                    by = "6 months"),
#                date_labels="%Y") +
#   theme_light() +
#   labs(x="Date",y="Prevalence (%)",
#        title = "REACT B-Spline covid-19 prevalence in England")+
#   theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
#         legend.title=element_blank(),
#         axis.title.x=element_text(size=10),
#         strip.background=element_rect(color="white", fill="white"),
#         strip.text=element_text(color="black", size=10, face="bold"))
# 
# plot_regions_prev_smooth
# 
# setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Descriptive_plots")
# ggsave("plot_regions_prev_smooth.pdf",plot_regions_prev_smooth,device="pdf")



# Summary and saving 6 datasets -------------------------------------------

# Creating 5 datasets:
# 1. mobility_tibble -> includes both raw and moving average mobility
# 2. mobility_tibble_raw -> only includes raw mobility
# 3. mobility_tibble_av -> only includes 7-day centered rolling average mobility
# 4. cases_tibble -> official covid-19 cases in England by specimen date
# 5. prev_tibble -> REACT-1 B-Spline daily prevalence by region
# 6. TO ADD PREVALENCE SMOOTH TIBBLE

setwd("~/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles")
saveRDS(mobility_tibble,"mobility_tibble.rds")
saveRDS(mobility_tibble_raw,"mobility_tibble_raw.rds")
saveRDS(mobility_tibble_av,"mobility_tibble_av.rds")
saveRDS(cases_tibble,"cases_tibble.rds")
saveRDS(prev_tibble,"prev_tibble.rds")
saveRDS(prev_smooth_tibble,"prev_smooth_tibble.rds")
