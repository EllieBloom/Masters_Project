# Heatmaps

# Reproducing heatmap as in network paper 6 (Global and local mobility as a barometer for COVID‐19 dynamics)

# Date started: 20th June 2022

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)



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

REACT_start <-as.Date("2020-05-01","%Y-%m-%d")



# Loading mobility data ---------------------------------------------------

# Regional google mobility data

mobility_regional <-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_regional_long.csv")

head(mobility_regional)
table(mobility_regional$type_mobility)
min(mobility_regional$date)



# Expanding to all regions ------------------------------------------------

regions_list <- dput(unique(mobility_regional$region))
mobility_list <- dput(unique(mobility_regional$type_mobility))
week_numbers <- dput(unique(mobility_regional$week))

# Adding week no.

first_year <- year(min(mobility_regional$date))

mobility_regional <- mobility_regional %>%
  mutate(week = week(date)+53*(year(date)-first_year))

# Adding first date of week

# Start of the first week

min(mobility_regional$week)
max(mobility_regiona$week)

mobility_regional[mobility_regional$week==7,]
mobility_regional[mobility_regional$week==125,]

start_week_date <- as.Date("2020-02-12","%Y-%m-%d")
end_week_date <- as.Date("2022-05-07","%Y-%m-%d")


week_dates_list <- seq(start_week_date, end_week_date, by="week")


# Loop for weekly average


start <- Sys.time()
weekly_summary <- NA

for (i in 1:length(week_numbers)) {
  for (j in 1: length(regions_list)){
    for (k in 1:length(mobility_list)){
  week_no <- week_numbers[i]
  week_date <- week_dates_list[i]
  region_name <- regions_list[j]
  mobility_name <- mobility_list[k]
  data<- mobility_regional %>% filter(week==week_no, region==region_name, type_mobility==mobility_name)
  weekly_av <- mean(data$mobility)
  results <- c(week_no, week_date, region_name,mobility_name, weekly_av)
  weekly_summary <- rbind(weekly_summary, results)
    }
  }
}

end <- Sys.time()
end-start # Time difference of 1.756465 mins

weekly_summary <- weekly_summary[-1,]
colnames(weekly_summary) <- c("week","week_start_date","region","type_mobility","mobility_av")


weekly_summary_df <- as.data.frame(weekly_summary)
weekly_summary_df$week <- as.numeric(weekly_summary_df$week)
weekly_summary_df$mobility_av <- as.numeric(weekly_summary_df$mobility_av)
weekly_summary_df$week_start_date <- as.numeric(weekly_summary_df$week_start_date)
weekly_summary_df$week_start_date <- as.Date(weekly_summary_df$week_start_date,format="%Y-%m-%d")
weekly_summary_df

weekly_summary_df <- weekly_summary_df %>%
  mutate(region = 
           case_when(region=="EAST"~"East" ,
                     region=="EAST MIDLANDS"~"East Midlands",
                     region=="LONDON"~"London" ,
                     region=="NORTH EAST"~"North East",
                     region=="NORTH WEST"~"North West",
                     region=="SOUTH EAST"~"South East" ,
                     region=="SOUTH WEST"~"South West" ,
                     region=="WEST MIDLANDS"~"West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~"Yorkshire and the Humber" ))

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Heatmap")
write.csv(weekly_summary_df,"regional_mobility_weekly_av.csv")


# Mobility heatmaps ---------------------------------------------------------------

unique(weekly_summary_df$type_mobility)


library(scales)

# I have changed the midpoints of scale for each to the mean -> could use median


##  1.Workplace ------------------------------------------------------------

workplace_midpoint <- mean(weekly_summary_df$mobility_av[weekly_summary_df$type_mobility=="workplaces"])
workplace_midpoint


workplace_plot <- weekly_summary_df %>% filter(type_mobility=="workplaces") %>%
ggplot(aes(x=week_start_date,y=region, fill=mobility_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=color_scale_midpoint) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Workplace mobility") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels
        
workplace_plot

## 2.Retail and recreation ------------------------------------------------------------

retail_midpoint <- mean(weekly_summary_df$mobility_av[weekly_summary_df$type_mobility=="retail_recreation"])
retail_midpoint

retail_plot <- weekly_summary_df %>% filter(type_mobility=="retail_recreation") %>%
  ggplot(aes(x=week_start_date,y=region, fill=mobility_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=retail_midpoint) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Retail and recreation mobility") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

retail_plot

## 3.Grocery and pharmacy ------------------------------------------------------------

grocery_midpoint <- mean(weekly_summary_df$mobility_av[weekly_summary_df$type_mobility=="grocery_pharmacy"])
grocery_midpoint

grocery_plot <- weekly_summary_df %>% filter(type_mobility=="grocery_pharmacy") %>%
  ggplot(aes(x=week_start_date,y=region, fill=mobility_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=grocery_midpoint) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Grocery and pharmacy mobility") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

grocery_plot


## 4.Transit ------------------------------------------------------------
transit_midpoint <- mean(weekly_summary_df$mobility_av[weekly_summary_df$type_mobility=="transit_stations"])
transit_midpoint


transit_plot <- weekly_summary_df %>% filter(type_mobility=="transit_stations") %>%
  ggplot(aes(x=week_start_date,y=region, fill=mobility_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=transit_midpoint) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Transit stations mobility") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

transit_plot

## 5. Parks ------------------------------------------------------------
parks_midpoint <- mean(weekly_summary_df$mobility_av[weekly_summary_df$type_mobility=="parks"])
parks_midpoint

parks_plot <- weekly_summary_df %>% filter(type_mobility=="parks") %>%
  ggplot(aes(x=week_start_date,y=region, fill=mobility_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=parks_midpoint) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Parks mobility") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

parks_plot

## 6. Residential ------------------------------------------------------------
residential_midpoint <- mean(weekly_summary_df$mobility_av[weekly_summary_df$type_mobility=="residential"])
residential_midpoint

residential_plot <- weekly_summary_df %>% filter(type_mobility=="residential") %>%
  ggplot(aes(x=week_start_date,y=region, fill=mobility_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=residential_midpoint) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Residential mobility") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

residential_plot




### Saving all plots --------------------------------------------------------



setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Heatmap")
workplace_plot
ggsave("heatmap_workplace_weekly.pdf", workplace_plot)
retail_plot
ggsave("heatmap_retail_weekly.pdf", retail_plot)
grocery_plot
ggsave("heatmap_grocery_weekly.pdf", grocery_plot)
transit_plot
ggsave("heatmap_transit_weekly.pdf", transit_plot)
parks_plot
ggsave("heatmap_parks_weekly.pdf", parks_plot)
residential_plot
ggsave("heatmap_residential_weekly.pdf", residential_plot)





# Prevalence heatmaps -----------------------------------------------------

## Preparing data ----------------------------------------------------------

prevalence <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Data/REACT_prevalence/regional_prevalence.RDS")

prevalence <- prevalence[,c('p','d_comb','region')]
library(pubh)

prevalence$p <- inv_logit(prevalence$p)
colnames(prevalence) <- c("prevalence","date","region")

region_list <- dput(unique(prevalence$region))




# Adding week variable
first_year <- min(year(prevalence$date))

prevalence <- prevalence %>%
  mutate(week = week(date)+53*(year(date)-first_year))



# Start of the first week

min(prevalence$week)
max(prevalence$week)

prevalence[prevalence$week==18,]
prevalence[prevalence$week==113,]

start_week_date <- as.Date("2020-04-29","%Y-%m-%d")
end_week_date <- as.Date("2022-02-07","%Y-%m-%d")

week_dates_list <- seq(start_week_date, end_week_date, by="week")
regions_list <- dput(unique(prevalence$region))

week_numbers <- dput(unique(prevalence$week))

# Loop for weekly average


start <- Sys.time()
weekly_summary_prev <- NA

for (i in 1:length(week_numbers)) {
  for (j in 1: length(regions_list)){
      week_no <- week_numbers[i]
      week_date <- week_dates_list[i]
      region_name <- regions_list[j]
      data<- prevalence %>% filter(week==week_no, region==region_name)
      weekly_av <- mean(data$prevalence)
      results <- c(week_no, week_date, region_name, weekly_av)
      weekly_summary_prev <- rbind(weekly_summary_prev, results)
  }
}

end <- Sys.time()
end-start 

weekly_summary_prev

sum(is.na(prevalence$prevalence)) # Nothing is missing so no region for NAs

weekly_summary_prev <- weekly_summary_prev[-1,]
colnames(weekly_summary_prev) <- c("week","week_start_date","region","prevalence_av")


weekly_summary_prev_df <- as.data.frame(weekly_summary_prev)
weekly_summary_prev_df$week <- as.numeric(weekly_summary_prev_df$week)
weekly_summary_prev_df$prevalence_av <- as.numeric(weekly_summary_prev_df$prevalence_av)
weekly_summary_prev_df$week_start_date <- as.numeric(weekly_summary_prev_df$week_start_date)
weekly_summary_prev_df$week_start_date <- as.Date(weekly_summary_prev_df$week_start_date,format="%Y-%m-%d")
weekly_summary_prev_df

weekly_summary_prev_df <- weekly_summary_prev_df %>%
  mutate(region = 
           case_when(region=="EE"~"East" ,
                     region=="EM"~"East Midlands",
                     region=="LN"~"London" ,
                     region=="NE"~"North East",
                     region=="NW"~"North West",
                     region=="SE"~"South East" ,
                     region=="SW"~"South West" ,
                     region=="WM"~"West Midlands",
                     region=="YH"~"Yorkshire and the Humber" ))

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Heatmap")
write.csv(weekly_summary_prev_df,"regional_prevalence_weekly_av.csv")


##  Heatmap -----------------------------------------------------------------

library(scales)

# I have changed the midpoints of scale for each to the mean -> could use median


##  Heatmap ------------------------------------------------------------

week_cutoff <- 103 # limiting date as cases so much higher at the end of 2021 into 2022

prevalence_midpoint <- mean(weekly_summary_prev_df$prevalence_av[weekly_summary_df$week<week_cutoff], na.rm = TRUE) # There are 3 NAs -> check this out
prevalence_midpoint
max(prevalence$prevalence)


prevalence_plot <- weekly_summary_prev_df %>% filter(week<week_cutoff) %>% # limiting date as cases so much higher at the end of 2021 into 2022
  ggplot(aes(x=week_start_date,y=region, fill=prevalence_av*100))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=prevalence_midpoint*100) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("REACT prevalence") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

prevalence_plot

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Heatmap")
prevalence_plot
ggsave("heatmap_prevalence_weekly.pdf", prevalence_plot)





# Official cases heatmap --------------------------------------------------

## Data preparation --------------------------------------------------------


cases <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/cases_tibble.rds")

cases

# Adding week variable
first_year <- min(year(cases$date))

cases <- cases %>%
  mutate(week = week(date)+53*(year(date)-first_year))



# Start of the first week

min(cases$week)
max(cases$week)

cases[cases$week==5,]
cases[cases$week==128,]

start_week_date <- as.Date("2020-01-27","%Y-%m-%d")
end_week_date <- as.Date("2022-05-28","%Y-%m-%d")

week_dates_list <- seq(start_week_date, end_week_date, by="week")
regions_list <- dput(unique(cases$region))
week_numbers <- dput(unique(cases$week))


# Loop

start <- Sys.time()
weekly_summary_cases <- NA

for (i in 1:length(week_numbers)) {
  for (j in 1: length(regions_list)){
    week_no <- week_numbers[i]
    week_date <- week_dates_list[i]
    region_name <- regions_list[j]
    data<- cases %>% filter(week==week_no, region==region_name)
    weekly_av <- mean(data$cases)
    results <- c(week_no, week_date, region_name, weekly_av)
    weekly_summary_cases <- rbind(weekly_summary_cases, results)
  }
}

end <- Sys.time()
end-start #Time difference of 9.857337 secs

weekly_summary_cases



weekly_summary_cases <- weekly_summary_cases[-1,]
colnames(weekly_summary_cases) <- c("week","week_start_date","region","cases_av")


weekly_summary_cases_df <- as.data.frame(weekly_summary_cases)
weekly_summary_cases_df$week <- as.numeric(weekly_summary_cases_df$week)
weekly_summary_cases_df$cases_av <- as.numeric(weekly_summary_cases_df$cases_av)
weekly_summary_cases_df$week_start_date <- as.numeric(weekly_summary_cases_df$week_start_date)
weekly_summary_cases_df$week_start_date <- as.Date(weekly_summary_cases_df$week_start_date,format="%Y-%m-%d")
weekly_summary_cases_df

weekly_summary_cases_df <- weekly_summary_cases_df %>%
  mutate(region = 
           case_when(region=="EAST"~"East" ,
                     region=="EAST MIDLANDS"~"East Midlands",
                     region=="LONDON"~"London" ,
                     region=="NORTH EAST"~"North East",
                     region=="NORTH WEST"~"North West",
                     region=="SOUTH EAST"~"South East" ,
                     region=="SOUTH WEST"~"South West" ,
                     region=="WEST MIDLANDS"~"West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~"Yorkshire and the Humber" ))

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Heatmap")
write.csv(weekly_summary_cases_df,"regional_cases_weekly_av.csv")




## Heatmap -----------------------------------------------------------------

week_cutoff <- 103 # limiting date as cases so much higher at the end of 2021 into 2022

cases_midpoint <- mean(weekly_summary_cases_df$cases_av[weekly_summary_cases_df$week<week_cutoff&weekly_summary_cases_df$week>=19], na.rm=TRUE) # There are 3 NAs -> check this out
cases_midpoint

table(weekly_summary_cases_df$week[weekly_summary_cases_df$week<week_cutoff])


cases_plot <- weekly_summary_cases_df %>% filter(week < week_cutoff, week >=19) %>%# limiting date as cases so much higher at the end of 2021 into 2022
  ggplot(aes(x=week_start_date,y=region, fill=cases_av))+
  geom_tile(color="black") +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=3000) +
  guides(fill = guide_colourbar(title = "")) +
  labs(x="", y="") +
  ggtitle("Official cases") +
  scale_x_date(date_labels = "%d %b %Y", breaks=date_breaks("4 weeks"), expand=c(0,0)) +
  theme(axis.ticks.y=element_line(color = c(NA, NA, NA, NA)),
        axis.text.x=element_text(angle=60, hjust=1),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5)) # Removes tick marks next to regions, angles x axis labels

cases_plot

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Heatmap")
cases_plot
ggsave("heatmap_cases_weekly.pdf", cases_plot)


