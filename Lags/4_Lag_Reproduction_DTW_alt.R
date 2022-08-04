# Understanding the lag between mobility and reproduction number (R)

# Date started: 15th July 2022





# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(stringr)
library(dtw)
library(BBmisc)

min_max_normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



# Useful dates ------------------------------------------------------------

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

REACT_start <-as.Date("2020-05-01","%Y-%m-%d")


# Loading mobility tibble ---------------------------------------------------------

mobility_google <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")

# Only want the rolling averages
mobility_av <- mobility_google %>% filter(str_detect(type_mobility, 'av'))

# Defining a list of types of mobility
types_mobility <- c("retail_recreation_av", "grocery_pharmacy_av", "parks_av","transit_stations_av", "workplaces_av","residential_av")

mobility_av <- mobility_av %>% 
  mutate(type_mobility = 
           case_when(type_mobility=="retail_recreation_av"~"Retail & recreation" ,
                     type_mobility=="grocery_pharmacy_av"~ "Grocery & pharmacy",
                     type_mobility=="parks_av"~ "Parks",
                     type_mobility=="transit_stations_av"~ "Transit stations",
                     type_mobility=="workplaces_av"~ "Workplaces",
                     type_mobility=="residential_av"~ "Residential"))


types_mobility <- dput(unique(mobility_av$type_mobility))    
types_mobility

# Load REACT reproduction number data -------------------------------------

react_reprod <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

REACT_reprod_start <- as.Date("2020-05-15","%Y-%m-%d")




# Functions
setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results")
# DTW open and close end

dtw_open <- function(query=mobility_av, reference=react_reprod, 
                     mobility_place="Workplaces", start_date, end_date, region="ENGLAND"){
  # Filtering data
  mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
  reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
  # DTW function
  dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
  print("Mean distance between indices is:")
  print(mean(dtw_lag$index1 - dtw_lag$index2))
  print("Median distance between indices is:")
  print(median(dtw_lag$index1 - dtw_lag$index2))
  # DTW path plot
  dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
  colnames(dtw_data)<- c("mob_index","r_index")
  write.csv(dtw_data,"dtw_indices.csv" )
  dtw_plot <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
                      geom_line(col="blue") + 
                      theme_bw() +
                      labs(x="Mobility series index",
                           y="R(t) series index")+  
                      xlim(0,nrow(mob))+
                      ylim(0,nrow(mob))+
                      geom_abline(slope=1, intercept=0, col="red", linetype="dashed")
  plot_name <- paste0("plot_dtw_",region,"_",start_date,"_",end_date,"_",mobility_place,".png")
  return(dtw_plot)
  ggsave(plot_name, dtw_plot)
  }



# Experimenting with open and close end arguments

# Lockdown 3

dtw_open(query=mobility_av, reference=react_reprod, 
         mobility_place="Workplaces", start_date=lockdown_3_start, lockdown_3_end, region="ENGLAND")

query=mobility_av
reference=react_reprod
mobility_place="Workplaces"
start_date=lockdown_3_start
end_date=lockdown_3_end
region="ENGLAND"

mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)


# Step pattern: https://www.rdocumentation.org/packages/dtw/versions/1.22-3/topics/stepPattern

dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r),step.pattern = asymmetric, open.begin=TRUE, open.end=TRUE) 
dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
colnames(dtw_data)<- c("mob_index","r_index")
dtw_plot <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
  geom_line(col="blue") + 
  theme_bw() +
  labs(x="Mobility series index",
       y="R(t) series index")+
  xlim(0,nrow(mob))+
  ylim(0,nrow(mob))+
  geom_abline(slope=1, intercept=0, col="red", linetype="dashed")
dtw_plot

mean(dtw_data$r_index-dtw_data$mob_index)
median(dtw_data$r_index-dtw_data$mob_index)
