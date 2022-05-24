# #### Exploratory Analysis

# Date started: 18th May 2022

# Exploring Apple mobility data and the effect of different NPIs


# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Loading Apple data

apple_all <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Apple/apple_mobility_report.csv")

# Filter for England only
apple <- apple_all %>% filter (country=="United Kingdom") %>% filter(sub.region=="England") %>% filter(subregion_and_city=="England")

head(apple)

# Rolling average

library(zoo) # Used for moving average

apple <- apple %>%
  mutate(driving_av = rollmean(driving, k=7, fill=NA, align="center") ,
         transit_av = rollmean(transit, k=7, fill=NA, align="center") ,
         walking_av = rollmean(walking, k=7, fill=NA, align="center")) 

colnames(apple)


# Converting dates from string format to date

str(apple$date) # Dates are currently stored as characters
apple$date <- as.Date(apple$date,format="%Y-%m-%d")
str(apple$date)


## Reshaping the data to long --------------------------------------------------

library(reshape2)

apple_long <- melt(apple[,c("driving_av", "transit_av",
                                "walking_av", "date")], id.vars="date")

# Baseline plot

library(grid)

labels=c("Driving","Transit","Walking")

plot_baseline <- ggplot(data=apple_long, aes(x=date,y=value,color=variable)) +
  geom_line() +
  xlab("") +
  ylab ("7-day rolling average change from baseline movement (%)") +
  ggtitle("") +
  labs(color = "") + # This means that the title for the legent is blank
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "3 months"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "1 months"),
               date_labels="%b %Y") +
  scale_color_hue(labels = labels)+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") 

plot_baseline
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Apple")
ggsave("Apple_overall_plot.pdf", plot=plot_baseline)



plot_baseline_lockdowns <- plot_baseline + annotate("rect", xmin=as.Date("2020-03-26","%Y-%m-%d"), xmax = as.Date("2020-06-15","%Y-%m-%d"), ymin=-90, ymax=115, alpha=0.2) +
                                          annotate("rect", xmin=as.Date("2020-11-05","%Y-%m-%d"), xmax = as.Date("2020-12-02","%Y-%m-%d"), ymin=-90, ymax=115, alpha=0.2) +
                                          annotate("rect", xmin=as.Date("2021-01-06","%Y-%m-%d"), xmax = as.Date("2021-04-12","%Y-%m-%d"), ymin=-90, ymax=115, alpha=0.2) +
                                          annotate("rect", xmin=as.Date("2021-12-8","%Y-%m-%d"), xmax = as.Date("2022-01-27","%Y-%m-%d"), ymin=-90, ymax=115, alpha=0.2) +
                                          annotate("text",x=as.Date("2020-03-26","%Y-%m-%d"), y=125, label="1st lockdown", hjust=0, color="gray23") +
                                          annotate("text",x=as.Date("2020-12-20","%Y-%m-%d"), y=125, label="2nd lockdown",  hjust=1, color="gray23") +
                                          annotate("text",x=as.Date("2021-02-15","%Y-%m-%d"), y=125, label="3rd lockdown",  hjust=0.5, color="gray23") +
                                          annotate("text",x=as.Date("2022-01-05","%Y-%m-%d"), y=125, label="Plan B measures",  hjust=0.5, color="gray23")
plot_baseline_lockdowns
ggsave("Apple_baseline_lockdowns.pdf",plot=plot_baseline_lockdowns, device="pdf")


## Facet wrap baseline plot ------------------------------------------------


labels_list <- as_labeller( c("driving_av"="Driving", "transit_av"="Transit", 
                              "walking_av"="Walking"))

location_labeller <- function(variable,value){
  return(labels_list[value])
} 
  
  
  
plot_wrap <-ggplot(data=apple_long, aes(x=date,y=value)) +
    facet_wrap(variable~., ncol=1, labeller=labels_list, strip.position="top", scales = "free") +
    geom_line(color="Dark blue") +
    xlab("") +
    ylab ("7-day rolling average change from baseline movement (%)") +
    #ggtitle("Apple mobility in England") +
    labs(color = "") + # This means that the title for the legent is blank
    scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                               to = max(x), 
                                               by = "3 months"),
                 minor_breaks = function(x) seq.Date(from = min(x), 
                                                     to = max(x), 
                                                     by = "1 months"),
                 date_labels="%b %Y") +
    scale_color_hue(labels = labels)+
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),legend.position = "none",
          strip.background=element_rect(color="white", fill="white"),
          strip.text=element_text(color="black", size=10, face="bold")) 
  
  
plot_wrap

ggsave("Apple_overall_wrap_plot.pdf", plot=plot_wrap)




# Adding lockdown dates 
                     

lockdown_dates=c("2020-03-26","2020-11-05","2021-01-06","2021-12-08")
                     
plot_lockdowns <- plot_wrap + geom_vline(xintercept = as.numeric(as.Date(lockdown_dates)), linetype="dashed", colour="red",size=0.25)
plot_lockdowns
ggsave("Apple_wrap_lockdowns_plot.pdf", plot=plot_lockdowns)
                     
                     
# Adding easing dates
                     
easing_dates=c("2020-06-15", "2020-12-02", "2021-04-12", "2022-01-27")
                     
plot_dates <- plot_lockdowns + geom_vline(xintercept=as.numeric(as.Date(easing_dates)), linetype="dashed", colour="dark green",size=0.25)
plot_dates

                     
# Adding labels for lockdown and easing
labels_1 <- data.frame(variable=c("driving_av", "transit_av", "walking_av"),
                                            label=c("Measures","",""))
                     
                     
labels_2 <- data.frame(variable=c("driving_av", "transit_av", "walking_av"),
                                            label=c("Easing","",""))
                     
plot_labelled <- plot_dates + geom_text(x=as.Date("2022-03-01","%Y-%m-%d"), y=-35, aes(label=label), data=labels_1, color="Red", hjust=0) +
                               geom_text(x=as.Date("2022-03-01","%Y-%m-%d"), y=-50, aes(label=label), data=labels_2, color="Dark green", hjust=0) 
plot_labelled
ggsave("Apple_wrap_measures_plot.pdf", plot=plot_labelled)
                     
                   

# Average apple mobility measure ------------------------------------------

# Mean excluding parks and residential
apple$mobility_overall <- rowMeans(apple[,9:11], na.rm=TRUE)

apple$mobility_overall_av <- rollmean(apple$mobility_overall, k=7, fill=NA, align="center")

plot_overall <- ggplot(data=apple, aes(x=date,y=mobility_overall_av)) +
                geom_line(color="navy blue") +
                xlab("") +
                ylab ("7-day rolling average change from baseline average mobility (%)") +
                ggtitle("Apple - combined mobility measure") +
                scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "3 months"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "1 months"),
               date_labels="%b %Y") +
              theme_light() +
              theme(plot.title = element_text(hjust = 0.5),legend.position = "none") 
plot_overall     

plot_overall_lockdowns <- plot_overall + annotate("rect", xmin=as.Date("2020-03-26","%Y-%m-%d"), xmax = as.Date("2020-06-15","%Y-%m-%d"), ymin=-90, ymax=75, alpha=0.2) +
                                    annotate("rect", xmin=as.Date("2020-11-05","%Y-%m-%d"), xmax = as.Date("2020-12-02","%Y-%m-%d"), ymin=-90, ymax=75, alpha=0.2) +
                                    annotate("rect", xmin=as.Date("2021-01-06","%Y-%m-%d"), xmax = as.Date("2021-04-12","%Y-%m-%d"), ymin=-90, ymax=75, alpha=0.2) +
                                    annotate("rect", xmin=as.Date("2021-12-8","%Y-%m-%d"), xmax = as.Date("2022-01-27","%Y-%m-%d"), ymin=-90, ymax=75, alpha=0.2) +
                                    annotate("text",x=as.Date("2020-03-26","%Y-%m-%d"), y=80, label="1st lockdown", hjust=0, color="gray23") +
                                    annotate("text",x=as.Date("2020-12-20","%Y-%m-%d"), y=80, label="2nd lockdown",  hjust=1, color="gray23") +
                                    annotate("text",x=as.Date("2021-02-15","%Y-%m-%d"), y=80, label="3rd lockdown",  hjust=0.5, color="gray23") +
                                    annotate("text",x=as.Date("2022-01-05","%Y-%m-%d"), y=80, label="Plan B measures",  hjust=0.5, color="gray23")

plot_overall_lockdowns
ggsave("Apple_plot_overall_mobility.pdf", plot=plot_overall_lockdowns)



# Regional ----------------------------------------------------------------

apple_regional <- apple_all %>% filter (country=="United Kingdom") %>% filter(sub.region=="England")

apple_regional$date<-as.Date(apple_regional$date,"%Y-%m-%d")


unique(apple_regional$sub.region) # all England
unique(apple_regional$subregion_and_city) # 13 major cities

england_subregion <- c("Birmingham", "Bradford", "Bristol", "England", "Leeds", "Liverpool", 
                       "London", "Manchester", "Newcastle upon Tyne", "Nottingham", 
                       "Portsmouth", "Reading", "Sheffield")


apple_new <-NA

for (i in 1:length(england_subregion)){
  # Creating new subdataset for each region
  apple_temp <- apple_regional %>% filter(subregion_and_city==england_subregion[i])
  # Creating the 3 new moving average variables
  apple_temp$driving_av =
    rollmean(apple_temp$driving, k=7, fill=NA, align="center")
  apple_temp$transit_av =
    rollmean(apple_temp$transit, k=7, fill=NA, align="center")
  apple_temp$walking_av =
    rollmean(apple_temp$walking, k=7, fill=NA, align="center")
  # Appending this dataset for the particular region to the new dataset
  apple_new <- rbind(apple_new,apple_temp)
  # Printing for progress
  print(england_subregion[i])
}

# Remove empty first row
apple_new<-apple_new[-1,]


# Make a long dataset


apple_long_regional <- melt(apple_new[,c("driving_av", "transit_av",
                                              "walking_av", "date","subregion_and_city")], 
                            id.vars=c("date","subregion_and_city"))


# Plot

labels_list <- as_labeller( c("driving_av"="Driving", 
                              "transit_av"="Transit", 
                              "walking_av"="Walking"))

location_labeller <- function(variable,value){
  return(labels_list[value])
} 


plot_wrap_regional <- ggplot(data=apple_long_regional, aes(x=date, y=value, color=variable)) +
                      facet_wrap(.~subregion_and_city, scales = "free") +                  
                      geom_line(size=0.25) +
                      xlab("") +
                      ylab ("7-day rolling average change from baseline movement (%)") +
                      scale_color_hue(labels = labels_list) +
                      scale_x_date(breaks = function(x) seq.Date(from = as.Date("2020-01-01","%Y-%m-%d"), 
                                                                 to = as.Date("2022-06-01","%Y-%m-%d"), 
                                                                 by = "1 year"),
                                   minor_breaks = function(x) seq.Date(from = as.Date("2020-01-01","%Y-%m-%d"), 
                                                                       to = as.Date("2022-06-01","%Y-%m-%d"), 
                                                                       by = "6 months"),
                                   date_labels="%Y") +                    
                      theme_light() +
                      theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
                            legend.title=element_blank(),
                            axis.title.x=element_text(size=5),
                            strip.background=element_rect(color="white", fill="white"),
                            strip.text=element_text(color="black", size=5, face="bold")) 



plot_wrap_regional


ggsave("Apple_regional_wrap_plot.pdf", plot=plot_wrap_regional)


