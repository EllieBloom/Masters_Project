# Time-series for England

# Date started: 11th July 2022


library(tidyverse)
library(lubridate)

# England data
google_england_long <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")



# Removing NAs


# Useful dates
lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

labels=c("Retail and recreation","Grocery and pharmacy",
         "Parks","Transit stations",
         "Workplaces","Residential")


# Selecting just England and moving averages, and giving end date of end of lockdown 3

cols <- c("retail_recreation_av", 
          "grocery_pharmacy_av", "parks_av", "transit_stations_av", "workplaces_av", 
          "residential_av")

google_england <- google_england_long %>% filter(region=="ENGLAND", type_mobility %in% cols) %>% filter(date<=lockdown_3_end)




# Plot for England with lockdowns labelled - only within lockdowns ----------------------


plot_baseline <- ggplot(data=google_england, aes(x=date,y=mobility,color=type_mobility)) +
  geom_line() +
  xlab("") +
  ylab ("Mobility compared to baseline (%)") +
  ggtitle("") +
  labs(color = "") + # This means that the title for the legent is blank
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "3 months"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "1 months"),
               date_labels="%b %Y",
               limits=c(lockdown_1_start, lockdown_3_end)) +
  scale_color_hue(labels = labels)+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "right")

plot_baseline

# Adding shaded areas for lockdowns
plot_baseline_lockdowns <- plot_baseline + annotate("rect", xmin=lockdown_1_start, xmax = lockdown_1_end, ymin=-80, ymax=125, alpha=0.2) +
                                          annotate("rect", xmin=lockdown_2_start, xmax = lockdown_2_end, ymin=-80, ymax=125, alpha=0.2) +
                                          annotate("rect", xmin=lockdown_3_start, xmax = lockdown_3_end, ymin=-80, ymax=125, alpha=0.2) 

plot_baseline_lockdowns

# Add labels

plot_baseline_lockdowns <- plot_baseline + annotate("rect", xmin=lockdown_1_start, xmax = lockdown_1_end, ymin=-80, ymax=125, alpha=0.2) +
                                            annotate("rect", xmin=lockdown_2_start, xmax = lockdown_2_end, ymin=-80, ymax=125, alpha=0.2) +
                                            annotate("rect", xmin=lockdown_3_start, xmax = lockdown_3_end, ymin=-80, ymax=125, alpha=0.2) +
                                            annotate("text",x=lockdown_1_start+days(30), y=130, label="Lockdown 1", hjust=0, color="gray23") +
                                            annotate("text",x=lockdown_2_start+days(12), y=130, label="Lockdown 2",  hjust=0.5, color="gray23") +
                                            annotate("text",x=lockdown_3_start+days(40), y=130, label="Lockdown 3",  hjust=0, color="gray23") 
plot_baseline_lockdowns

# Limit until the end of lockdown 3

plot_baseline_lockdowns
setwd("~/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google/Final_plots")
ggsave("England_mobility_lockdowns_only.png",plot_baseline_lockdowns)







# Plot for England with lockdowns labelled - whole period to 2022-------------------

google_england_whole <- google_england_long %>% filter(region=="ENGLAND", type_mobility %in% cols)


plot_baseline <- ggplot(data=google_england_whole, aes(x=date,y=mobility,color=type_mobility)) +
  geom_line() +
  xlab("") +
  ylab ("Mobility compared to baseline (%)") +
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
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  geom_hline(yintercept=0, linetype="dashed", size=0.5, col="grey")

plot_baseline

# Adding shaded areas for lockdowns

plan_b_start <-as.Date("2021-12-08","%Y-%m-%d")
plan_b_end <- as.Date("2022-01-27","%Y-%m-%d")


# Add labels

plot_baseline_lockdowns_labelled <- plot_baseline + annotate("rect", xmin=lockdown_1_start, xmax = lockdown_1_end, ymin=-80, ymax=125, alpha=0.2) +
  annotate("rect", xmin=lockdown_2_start, xmax = lockdown_2_end, ymin=-80, ymax=125, alpha=0.2) +
  annotate("rect", xmin=lockdown_3_start, xmax = lockdown_3_end, ymin=-80, ymax=125, alpha=0.2) +
  annotate("rect", xmin=plan_b_start, xmax = plan_b_end, ymin=-80, ymax=125, alpha=0.2) +
  annotate("text",x=lockdown_1_start+days(15), y=130, label="Lockdown 1", hjust=0, color="gray23") +
  annotate("text",x=lockdown_2_start+days(12), y=130, label="Lockdown 2",  hjust=0.5, color="gray23") +
  annotate("text",x=lockdown_3_start+days(25), y=130, label="Lockdown 3",  hjust=0, color="gray23") +
  annotate("text",x=plan_b_start+days(6), y=130, label="Plan B",  hjust=0, color="gray23")


plot_baseline_lockdowns_labelled

# Limit until the end of lockdown 3

plot_baseline_lockdowns_labelled

setwd("~/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google/Final_plots")
ggsave("England_mobility_whole_period.png",plot_baseline_lockdowns_labelled)

