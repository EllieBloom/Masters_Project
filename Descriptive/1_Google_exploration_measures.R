#### Exploratory Analysis

# Date started: 16th May 2022

# Exploring Google mobility data and the effect of different NPIs


# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(reshape)

# Loading Google data

google_2020<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/2020_GB_Region_Mobility_Report.csv")
google_2021<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/2021_GB_Region_Mobility_Report.csv")
google_2022<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/2022_GB_Region_Mobility_Report.csv")

# Combining 2020, 2021 and 2022

google_overall <- rbind(google_2020,google_2021,google_2022)
setwd("/Users/elliebloom/Desktop/Masters/Project/Data/Google")
write.csv(google_overall,"google_overall.csv")


# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)


# Exploration - GB level--------------------------------------------------------

# GB as a whole

google_gb <- google_overall %>% filter(sub_region_1=="") # Where there is no sub-region, the data is for GB overall


# Simple plot -------------------------------------------------------------


# Movement compared to baseline

plot(x=google_gb$date, y=google_gb$retail_and_recreation_percent_change_from_baseline, type="l", xlab="Date (2020)", ylab="Percentage change from baseline (%)",
     main="Google data - change from baseline movement UK in 2020", col=" navy blue", ylim=c(-100,150))
abline(h=0, col="dark gray", lty="dashed")
lines(x=google_gb$date, y=google_gb$grocery_and_pharmacy_percent_change_from_baseline, col="dark green")
lines(x=google_gb$date, y=google_gb$parks_percent_change_from_baseline, col="red")
lines(x=google_gb$date, y=google_gb$transit_stations_percent_change_from_baseline, col="orange")
lines(x=google_gb$date, y=google_gb$workplaces_percent_change_from_baseline, col="purple")
lines(x=google_gb$date, y=google_gb$residential_percent_change_from_baseline, col="green")
legend("topright", legend=c("Retail and recreation", "Grocery and pharmacy" , "Parks", "Transit stations", "Workplaces", "Residential"),
       col=c("navy blue", "dark green", "red", "orange" , "purple", "green"), pch=4, cex=0.75)



# Moving average ----------------------------------------------------------

# Creating a 7 day moving average for each point (average 6 days afterwards - can change to centre if not)

library(zoo) # Used for moving average

google_gb <- google_gb %>%
  mutate(retail_and_recreation_rolling_av = rollmean(retail_and_recreation_percent_change_from_baseline, k=7, fill=NA, align="center") ,
         grocery_and_pharmacy_rolling_av = rollmean(grocery_and_pharmacy_percent_change_from_baseline, k=7, fill=NA, align="center") ,
         parks_rolling_av = rollmean(parks_percent_change_from_baseline, k=7, fill=NA, align="center"),
         transit_stations_rolling_av = rollmean(transit_stations_percent_change_from_baseline, k=7, fill=NA, align="center"),
         workplaces_rolling_av = rollmean(workplaces_percent_change_from_baseline, k=7, fill=NA, align="center"),
        residential_rolling_av = rollmean(residential_percent_change_from_baseline, k=7, fill=NA, align="center")) 
  
colnames(google_gb)

# Moving average plot - base R -----------------------------------------------------


# Movement compared to baseline


colour_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                    "#0072B2", "#D55E00", "#CC79A7", "#000000")

plot(x=google_gb$date, y=google_gb$retail_and_recreation_rolling_av, type="l", xlab="Time", ylab="Percentage change from baseline (%)",
     main="Change from baseline movement UK (%)", col=colour_palette[1], ylim=c(-100,150))
abline(h=0, col="dark gray", lty="dashed")
lines(x=google_gb$date, y=google_gb$grocery_and_pharmacy_rolling_av, col=colour_palette[2])
lines(x=google_gb$date, y=google_gb$parks_rolling_av, col=colour_palette[3])
lines(x=google_gb$date, y=google_gb$transit_stations_rolling_av, col=colour_palette[4])
lines(x=google_gb$date, y=google_gb$workplaces_rolling_av, col=colour_palette[5])
lines(x=google_gb$date, y=google_gb$residential_rolling_av, col=colour_palette[6])
legend("topright", legend=c("Retail and recreation", "Grocery and pharmacy" , "Parks", "Transit stations", "Workplaces", "Residential"),
       col=colour_palette, pch=4, cex=0.75)

# Trying in ggplot instead

# Improvement - moving average plot to ggplot --------------------------------

## Reshaping the data to long --------------------------------------------------

library(reshape2)

google_gb_long <- melt(google_gb[,c("retail_and_recreation_rolling_av", 
                                    "grocery_and_pharmacy_rolling_av", "parks_rolling_av", 
                                    "transit_stations_rolling_av", "workplaces_rolling_av", 
                                    "residential_rolling_av","date")], id.vars="date")

# Plot

## Baseline plot ---------------------------------------------------------


library(grid)

labels=c("Retail and recreation","Grocery and pharmacy",
         "Parks","Transit stations",
         "Workplaces","Residential")
    
plot_baseline <- ggplot(data=google_gb_long, aes(x=date,y=value,color=variable)) +
        geom_line() +
        xlab("") +
        ylab ("7-day rolling average change from baseline movement (%)") +
        ggtitle("Google mobility in the UK") +
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


## Facet wrap baseline plot ------------------------------------------------


labels_list <- as_labeller( c("retail_and_recreation_rolling_av"="Retail and recreation",
                    "grocery_and_pharmacy_rolling_av"="Grocery and pharmacy",
                    "parks_rolling_av" = "Parks",
                    "transit_stations_rolling_av"="Transit stations",
                    "workplaces_rolling_av"= "Workplaces",
                    "residential_rolling_av" = "Residential" ))

location_labeller <- function(variable,value){
  return(labels_list[value])
}

# Baseline plot

plot_wrap <- ggplot(data=google_gb_long, aes(x=date,y=value)) +
                facet_wrap(variable~., ncol=1, labeller=labels_list, strip.position="top", scales = "free") +
                geom_line(color="dark blue") +
                xlab("") +
                ylab ("7-day rolling average change from baseline movement (%)") +
                #ggtitle("Google mobility in the UK") +
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

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google")
ggsave("Google_basic_wrap_plot.pdf",plot=plot_wrap, device="pdf")



## Adding date lines  ---------------------------------------------
# https://www.instituteforgovernment.org.uk/sites/default/files/timeline-coronavirus-lockdown-december-2021.pdf

# Adding lockdown dates 
                     
lockdown_dates=c("2020-03-26","2020-11-05","2021-01-06","2021-12-08")

plot_lockdowns <- plot_wrap + geom_vline(xintercept = as.numeric(as.Date(lockdown_dates)), linetype="dashed", colour="red",size=0.25)
plot_lockdowns

ggsave("Google_lockdown_dates_wrap_plot.pdf",plot=plot_lockdowns, device="pdf")



plot_baseline_lockdowns <- plot_baseline + annotate("rect", xmin=as.Date("2020-03-26","%Y-%m-%d"), xmax = as.Date("2020-06-15","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
  annotate("rect", xmin=as.Date("2020-11-05","%Y-%m-%d"), xmax = as.Date("2020-12-02","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-01-06","%Y-%m-%d"), xmax = as.Date("2021-03-29","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-12-08","%Y-%m-%d"), xmax = as.Date("2022-01-27","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2)


# Adding easing dates

easing_dates=c("2020-06-15", "2020-12-02", "2021-04-12", "2022-01-27")

plot_easing <- plot_wrap + geom_vline(xintercept=as.numeric(as.Date(easing_dates)), linetype="dashed", colour="dark green",size=0.25)
plot_easing

ggsave("Google_easing_dates_wrap_plot.pdf",plot=plot_easing, device="pdf")

# Both lockdown and easing

plot_dates <- plot_lockdowns + geom_vline(xintercept=as.numeric(as.Date(easing_dates)), linetype="dashed", colour="dark green",size=0.25)
plot_dates

ggsave("Google_dates_wrap_plot.pdf",plot=plot_dates, device="pdf")

# Adding labels for lockdown and easing

labels_1 <- data.frame(variable=c("retail_and_recreation_rolling_av", 
                                "grocery_and_pharmacy_rolling_av", "parks_rolling_av", "transit_stations_rolling_av", 
                                "workplaces_rolling_av", "residential_rolling_av"),
                    label=c("","Restrictions","","","",""))


labels_2 <- data.frame(variable=c("retail_and_recreation_rolling_av", 
                                  "grocery_and_pharmacy_rolling_av", "parks_rolling_av", "transit_stations_rolling_av", 
                                  "workplaces_rolling_av", "residential_rolling_av"),
                       label=c("","Easing","","","",""))

plot_labelled <- plot_dates + geom_text(x=as.Date("2022-03-20","%Y-%m-%d"), y=-20, aes(label=label), data=labels_1, color="Red",hjust=0) +
                              geom_text(x=as.Date("2022-03-20","%Y-%m-%d"), y=-30, aes(label=label), data=labels_2, color="Dark green",hjust=0) 


plot_labelled

ggsave("Google_dates_labelled_wrap_plot.pdf",plot=plot_labelled, device="pdf")


# Adding shaded boxes for lockdowns ---------------------------------------

plot_wrap + annotate("rect", xmin=as.Date("2020-03-26","%Y-%m-%d"), xmax = as.Date("2020-06-26","%Y-%m-%d"), ymin=-100, ymax=100, alpha=0.2)
# Unfortunately this changes the axis to all be the same

plot_baseline_lockdowns <- plot_baseline + annotate("rect", xmin=as.Date("2020-03-26","%Y-%m-%d"), xmax = as.Date("2020-06-15","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
                                          annotate("rect", xmin=as.Date("2020-11-05","%Y-%m-%d"), xmax = as.Date("2020-12-02","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
                                          annotate("rect", xmin=as.Date("2021-01-06","%Y-%m-%d"), xmax = as.Date("2021-04-12","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
                                          annotate("rect", xmin=as.Date("2021-12-8","%Y-%m-%d"), xmax = as.Date("2022-01-27","%Y-%m-%d"), ymin=-80, ymax=125, alpha=0.2) +
                                          annotate("text",x=as.Date("2020-03-26","%Y-%m-%d"), y=130, label="1st lockdown", hjust=0, color="gray23") +
                                          annotate("text",x=as.Date("2020-12-20","%Y-%m-%d"), y=130, label="2nd lockdown",  hjust=1, color="gray23") +
                                          annotate("text",x=as.Date("2021-02-15","%Y-%m-%d"), y=130, label="3rd lockdown",  hjust=0.5, color="gray23") +
                                          annotate("text",x=as.Date("2022-01-05","%Y-%m-%d"), y=130, label="Plan B measures",  hjust=0.5, color="gray23")
plot_baseline_lockdowns
ggsave("Google_baseline_lockdowns.pdf",plot=plot_baseline_lockdowns, device="pdf")



## Just workplace measure --------------------------------------------------

google_gb_long_workplace <- google_gb_long[google_gb_long$variable=="workplaces_rolling_av",]

plot_baseline_workplace <- ggplot(data=google_gb_long_workplace, aes(x=date,y=value)) +
  geom_line(color="navy blue") +
  xlab("") +
  ylab ("7-day rolling average change from baseline movement (%)") +
  ggtitle("Google workplace mobility in the UK") +
  labs(color = "") + # This means that the title for the legent is blank
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "3 months"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "1 months"),
               date_labels="%b %Y") +
  scale_y_continuous(c(-75,25))
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") 

plot_baseline_workplace

plot_baseline_lockdowns_workplace <- plot_baseline_workplace + annotate("rect", xmin=as.Date("2020-03-26","%Y-%m-%d"), xmax = as.Date("2020-06-15","%Y-%m-%d"), ymin=-80, ymax=5, alpha=0.2) +
  annotate("rect", xmin=as.Date("2020-11-05","%Y-%m-%d"), xmax = as.Date("2020-12-02","%Y-%m-%d"), ymin=-80, ymax=5, alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-01-06","%Y-%m-%d"), xmax = as.Date("2021-04-12","%Y-%m-%d"), ymin=-80, ymax=5, alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-12-8","%Y-%m-%d"), xmax = as.Date("2022-01-27","%Y-%m-%d"), ymin=-80, ymax=5, alpha=0.2) +
  annotate("text",x=as.Date("2020-03-26","%Y-%m-%d"), y=10, label="1st lockdown", hjust=0, color="gray23") +
  annotate("text",x=as.Date("2020-12-20","%Y-%m-%d"), y=10, label="2nd lockdown",  hjust=1, color="gray23") +
  annotate("text",x=as.Date("2021-02-15","%Y-%m-%d"), y=10, label="3rd lockdown",  hjust=0.5, color="gray23") +
  annotate("text",x=as.Date("2022-01-05","%Y-%m-%d"), y=10, label="Plan B measures",  hjust=0.5, color="gray23")

plot_baseline_lockdowns_workplace




#Combined google measure, as per imperial covid report 26 ------------------------

colnames(google_gb)

# Mean excluding parks and residential
google_gb$mobility_overall <- (google_gb$retail_and_recreation_rolling_av+ 
                               google_gb$grocery_and_pharmacy_rolling_av+
                               google_gb$transit_stations_rolling_av+
                               google_gb$workplaces_rolling_av)/4




plot_overall <- ggplot(data=google_gb, aes(x=date,y=mobility_overall)) +
                geom_line(color="navy blue") +
                xlab("") +
                ylab ("7-day rolling average change from baseline average mobility (%)") +
                ggtitle("Google combined mobility measure in the UK") +
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

ggsave("Google_combined_mobility_lockdowns_plot.pdf", plot=plot_overall_lockdowns, device="pdf")





# Regions within the UK/England ------------------------------------------------

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

google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "") # Blank sub_region_2 means at the overall level only

length(unique(google_england$sub_region_1)) # now 85 sub-regions

# Now need to do rolling average for each of the 85 regions

### Need to reorder as it is first orderered by 2020, 2021 and 2022, then by subregion...

google_england<-google_england[order(google_england$sub_region_1),]



google_new <-NA

for (i in 1:length(england_sub_region_1)){
  # Creating new subdataset for each region
  google_region <- google_england %>% filter(sub_region_1==england_sub_region_1[i])
  # Creating the 6 new moving average variables
  google_region$retail_and_recreation_rolling_av =
  rollmean(google_region$retail_and_recreation_percent_change_from_baseline, k=7, fill=NA, align="center")
  
  google_region$grocery_and_pharmacy_rolling_av =
  rollmean(google_region$grocery_and_pharmacy_percent_change_from_baseline, k=7, fill=NA, align="center")
  
  google_region$parks_rolling_av =
  rollmean(google_region$parks_percent_change_from_baseline, k=7, fill=NA, align="center")
  
  google_region$transit_stations_rolling_av =
  rollmean(google_region$transit_stations_percent_change_from_baseline, k=7, fill=NA, align="center")
  
  google_region$workplaces_rolling_av =
  rollmean(google_region$workplaces_percent_change_from_baseline, k=7, fill=NA, align="center")
  
  google_region$residential_rolling_av =
  rollmean(google_region$residential_percent_change_from_baseline, k=7, fill=NA, align="center")
  
  # Appending this dataset for the particular region to the new dataset
  google_new <- rbind(google_new,google_region)
  # Printing for progress
  print(england_sub_region_1[i])
}

# Remove empty first row
google_new<-google_new[-1,]


# Apepars to be working -> inspect the NAs (there are quite a lot) -> use another function that allows NAs to be ignored??



# Regional plots ----------------------------------------------------------

google_new_long <- melt(google_new[,c("retail_and_recreation_rolling_av", 
                                    "grocery_and_pharmacy_rolling_av", "parks_rolling_av", 
                                    "transit_stations_rolling_av", "workplaces_rolling_av", 
                                    "residential_rolling_av","sub_region_1","date")], id.vars=c("date","sub_region_1"))


labels_list <- as_labeller( c("retail_and_recreation_rolling_av"="Retail and recreation", 
                              "grocery_and_pharmacy_rolling_av"="Grocery and pharmacy", 
                              "parks_rolling_av"="Parks", 
                              "transit_stations_rolling_av"="Transit stations", 
                              "workplaces_rolling_av"="Workplaces", 
                              "residential_rolling_av"="Residential"))

location_labeller <- function(variable,value){
  return(labels_list[value])
} 


plot_wrap_regional <- ggplot(data=google_new_long, aes(x=date, y=value, color=variable)) +
                      facet_wrap(.~sub_region_1, scales = "free") +
                      geom_line(size=0.25) +
                      xlab("") +
                      ylab ("7-day rolling average change from baseline movement (%)") +
                      scale_color_hue(labels = labels_list) +
                      scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                                                 to = max(x), 
                                                                 by = "1 year"),
                                   minor_breaks = function(x) seq.Date(from = min(x), 
                                                                       to = max(x), 
                                                                       by = "6 months"),
                                   date_labels="%Y") +
                      theme_light() +
                      theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
                            legend.title=element_blank(),
                            axis.title.x=element_text(size=5),
                            strip.background=element_rect(color="white", fill="white"),
                            strip.text=element_text(color="black", size=5, face="bold")) 


plot_wrap_regional
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google")
ggsave("Google_regional_wrap_overall.pdf", plot=plot_wrap_regional)

# Removing parks 

# Change by excluding those that contain parks
google_new_long_excl_parks <- melt(google_new[,c("retail_and_recreation_rolling_av", 
                                                 "grocery_and_pharmacy_rolling_av",  
                                                 "transit_stations_rolling_av", "workplaces_rolling_av", 
                                                 "residential_rolling_av","sub_region_1","date")], id.vars=c("date","sub_region_1"))




# Plotting overall mobility by reigon -------------------------------------

head(google_england)

google_england$month <- months(google_england$date)
google_england$year <- format(google_england$date, format="%y")


mean_rows <-aggregate(workplaces_percent_change_from_baseline ~ month + year,google_england,mean)[,1:2]


means <- NA
monthly_means <-NA


for (i in 1:length(england_sub_region_1)){
  # Creating new subdataset for each region
  google_region <- google_england %>% filter(sub_region_1==england_sub_region_1[i])
  means <- aggregate(workplaces_percent_change_from_baseline ~ month + year,google_region,mean)
  monthly_means <- cbind(monthly_means,means[,3])
}

monthly_means <- monthly_means[,-1]
colnames(monthly_means) <- england_sub_region_1
monthly_means <- cbind(mean_rows,monthly_means)

# Combined month and date
library(zoo)
monthly_means$date <- as.yearmon(paste(monthly_means$year, monthly_means$month), "%y %B")




# Monthly boxplot

monthly_means$date <- as.Date(monthly_means$date, format="%y %B")

# Make long data
monthly_means_long <- as.data.frame(melt(monthly_means[,c("Bath and North East Somerset", "Bedford", 
                                                        "Blackburn with Darwen", "Blackpool", "Borough of Halton", "Bracknell Forest", 
                                                        "Brighton and Hove", "Bristol City", "Buckinghamshire", "Cambridgeshire", 
                                                        "Central Bedfordshire", "Cheshire East", "Cheshire West and Chester", 
                                                        "Cornwall", "County Durham", "Cumbria", "Darlington", "Derby", 
                                                        "Derbyshire", "Devon", "Dorset", "East Riding of Yorkshire", 
                                                        "East Sussex", "Essex", "Gloucestershire", "Greater London", 
                                                        "Greater Manchester", "Hampshire", "Hartlepool", "Herefordshire", 
                                                        "Isle of Wight", "Kent", "Kingston upon Hull", "Lancashire", 
                                                        "Leicester", "Leicestershire", "Lincolnshire", "Luton", "Medway", 
                                                        "Merseyside", "Middlesbrough", "Milton Keynes", "Norfolk", "North East Lincolnshire", 
                                                        "North Lincolnshire", "North Somerset", "North Yorkshire", "Northamptonshire", 
                                                        "Northumberland", "Nottingham", "Nottinghamshire", "Oxfordshire", 
                                                        "Peterborough", "Plymouth", "Portsmouth", "Reading", "Redcar and Cleveland", 
                                                        "Rutland", "Shropshire", "Slough", "Somerset", "South Gloucestershire", 
                                                        "South Yorkshire", "Southampton", "Southend-on-Sea", "Staffordshire", 
                                                        "Stockton-on-Tees", "Stoke-on-Trent", "Suffolk", "Surrey", "Swindon", 
                                                        "Thurrock", "Torbay", "Tyne and Wear", "Warrington", "Warwickshire", 
                                                        "West Berkshire", "West Midlands", "West Sussex", "West Yorkshire", 
                                                        "Wiltshire", "Windsor and Maidenhead", "Wokingham", "Worcestershire", 
                                                        "York", "date")],id.vars=c("date")))


# Create plot

date_labels=c("" ,"March 2020", "" ,"", "June 2020" ,"" ,"","Sept 2020" ,""
,"", "Dec 2020", "", "" ,"March 2021", "" ,"", "June 2021", "", "", "Sept 2020", "" ,"", 
"Dec 2021","", "", "March 2022", "","")

boxplot_monthly <- ggplot(data=monthly_means_long, aes(x=as.factor(date),y=value))+
                  geom_boxplot(outlier.shape=4, color="royal blue")+
                  xlab("Date")+
                  ylab("Mobility compared to baseline (%)")+
                  ggtitle("Boxplot of mean workplace mobility for each region in England")+
                  scale_x_discrete(labels=date_labels)+
                  theme_light()+
                  theme(plot.title = element_text(hjust = 0.5))

boxplot_monthly  

# Add in England too

cols <- c("retail_recreation_av", 
          "grocery_pharmacy_av", "parks_av", "transit_stations_av", "workplaces_av", 
          "residential_av")

google_england_long <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")
google_england_whole <- google_england_long %>% filter(region=="ENGLAND", type_mobility %in% cols)
google_england_whole_workplace <- google_england_whole %>% filter(type_mobility=="workplaces_av")
  
  
boxplot_monthly + geom_line(data=google_england_whole_workplace, aes(x=date,y=mobility))



setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google")
ggsave("Google_regional_wrokplace_boxplot.pdf", plot=boxplot_monthly)
