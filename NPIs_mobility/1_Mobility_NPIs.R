# Exploring the impact of NPIs on mobility

# Date started: 23rd May 2022

# Inspiration for some models:
# https://www.nature.com/articles/s41598-021-02133-1.pdf

# Thoughts:
# Maybe this should have been fitted with a time series instead 
# Plot comparison of regions

# Could do a boxplot of average for each month March 2020-May 2022 by region


# Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(gtsummary)
library(zoo)
library(reshape2)

# Loading Google data

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)


# Adding bank holidays as a field - ref: https://www.gov.uk/bank-holidays

bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                           "%Y-%m-%d")


google_overall$bank_holiday <- ifelse(google_overall$date %in% bank_holidays,1,0)
google_overall$bank_holiday <- as.factor(google_overall$bank_holiday)

# Adding days of the week into the google_gb dataset
google_overall$day <- weekdays(google_overall$date)

# Make into a factor
google_overall$day <- factor(google_overall$day)

# Sunday as reference category
google_overall$day <- relevel(google_overall$day, ref="Sunday")
str(google_overall$day)




# Filterting for GB dataset
google_gb <- google_overall %>% filter(sub_region_1=="") # Where there is no sub-region, the data is for GB overall

# Looking at correlations between metrics

#  Correlations between metrics at GB level -----------------------------


# Correlation matrix

# Rename the columns to make visualisation easier


google_gb_renamed <- google_gb  
colnames(google_gb_renamed)[11:16]<-c("Retail","Grocery and Pharmacy","Parks",
                                      "Transit Stations", "Workplaces", "Residential")

# Correlation matrix
cormat <- round(cor(google_gb_renamed[,c("Retail","Grocery and Pharmacy","Parks",
                                 "Transit Stations", "Workplaces", "Residential")]),2)
# Make long

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Preparing visualisation

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri


# Melt the correlation matrix

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Reorder the correlation matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

# Add correlation coefficients

ggheatmap_final<- ggheatmap + 
                  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
                  ggtitle("Correlation heatmap of Google Mobility at GB level") +
                  theme(
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    legend.justification = c(1, 0),
                    legend.position = c(0.6, 0.7),
                    legend.direction = "horizontal")+
                  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                               title.position = "top", title.hjust = 0.5))



ggheatmap_final
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
ggsave("GB_mobility_heatmap.pdf",ggheatmap_final)

# First lockdown - workplace mobility---------------------------------------------

## GB as a whole ---------------------------------------------------------

# No longer using 7-day rolling average as now have model taking into account days of the week
colnames(google_gb)

# Find the average mobility in the two weeks prior to lockdown enforcement, excluding 23-26th March where the lockdown was already announced

pre_lockdown1_end_date <-as.Date("2020-03-23","%Y-%m-%d") # Not using 26th March at the moment, but the date it was announced

average_pre_lockdown1<-mean(google_gb$workplaces_percent_change_from_baseline[google_gb$date<pre_lockdown1_end_date&google_gb$date>=pre_lockdown1_end_date-14])
average_pre_lockdown1

# Looking at the first lockdown:
# Start date - 26/03/2020 (1st lockdown legally comes into force)
# End date - 15/06/2020 (non-essential retail reopens)

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

google_lockdown1 <- google_gb %>% filter(date<=lockdown_1_end) %>% filter (date>=lockdown_1_start)
nrow(google_lockdown1) #82 days of data

# Days since lockdown
google_lockdown1$days_since_lockdown <- google_lockdown1$date - lockdown_1_start
summary(google_lockdown1$days_since_lockdown)

# Minimum workplace mobility reached in this period
min_lockdown1<-min(google_lockdown1$workplaces_percent_change_from_baseline)

# Change as a result of lockdown
drop_lockdown1 <- average_pre_lockdown1-min_lockdown1 
drop_lockdown1 # 79.7 percentage point max decrease in mobility


# Linear model
workplace_model_1 <- lm(workplaces_percent_change_from_baseline ~ day  + bank_holiday + days_since_lockdown, data=google_lockdown1 )
summary(workplace_model_1) # mobility significantly increases with days from lockdown
summary(workplace_model_1)$coef[9] # increase per day in mobility
summary(workplace_model_1)$coef[9]*30 # increase after 30 days -> 9.23 percentage points, out of 79.7 drop -> 12% increase

# Saving results
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
tbl_regression(workplace_model_1)
gt::gtsave(as_gt(tbl_regression(workplace_model_1)), "GB_workplace_lockdown1_regression_results.png")


# Checking assumptions
plot(workplace_model_1,1) # A bit heteroskedastic
plot(workplace_model_1,2) # Fat tails (kurtosis) - seems ok though

google_lockdown1$workplace_pred_1 <- predict(workplace_model_1, google_lockdown1)

# Plot of the model
plot_gb_lockdown1<- ggplot(data=google_lockdown1, aes(x=date,y=workplaces_percent_change_from_baseline)) +
                        geom_line(aes(color="Mobility")) + 
                        geom_line(aes(x=date, y=workplace_pred_1,color="Linear model"))+
                        geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
                        scale_color_manual(name="",
                                            breaks=c("Mobility","Linear model","Smoothed line"),
                                           values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
                        xlab("Date (2020)") +
                        ylab("Change from baseline (%)") +
                        ggtitle("Workplace mobility during lockdown 1") +
                        theme_light() +
                        theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")


plot_gb_lockdown1
ggsave("GB_workplace_lockdown1_regression_plot.pdf", plot=plot_gb_lockdown1, device="pdf")



## England - regional regressions ---------------------------------------------------

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

# Looking at missingness
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Descriptive/Ouputs/Google")
google_missingness <- read.csv("Google_summary_missingness_regional_%.csv")
  
# Removing areas with >5% missing for workplaces
colnames(google_missingness) <- c("region", "parks_percent_change_from_baseline", "residential_percent_change_from_baseline", 
                                  "transit_stations_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", 
                                  "retail_and_recreation_percent_change_from_baseline", "workplaces_percent_change_from_baseline")


# Checking how many have >5% missing
length(google_missingness$region[google_missingness$workplaces_percent_change_from_baseline>=5])
# None of the workplace ones have >5% missing - great!

google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "")


# Defining data for first lockdown
google_england_lockdown1 <- google_england %>% filter(date<=lockdown_1_end) %>% filter (date>=lockdown_1_start)

# Defining days since lockdown
google_england_lockdown1$days_since_lockdown <- google_england_lockdown1$date - lockdown_1_start




# Fitting a model for each region

library(lme4)

models <- lmList(workplaces_percent_change_from_baseline ~ day + bank_holiday + days_since_lockdown | sub_region_1, data=google_england_lockdown1)

# Gives all models
models_coefs<-coef(models, augFrame = TRUE)


# Changing the column order
col_order <-c("(Intercept)", "days_since_lockdown", "bank_holiday1",
              "dayMonday","dayTuesday", "dayWednesday", 
              "dayThursday",  "dayFriday","daySaturday")


models_coefs <- models_coefs[, col_order]
models_coefs

# Exporting this as csv
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
write.csv(models_coefs,"Regional_workplace_lockdown1_regression_results.csv")

# Exploring the trends in the increase in mobility since lockdown
min(models_coefs$days_since_lockdown)
rownames(models_coefs)[which.min(models_coefs$days_since_lockdown)] # York has the smallest increase per day at 0.233

max(models_coefs$days_since_lockdown)
rownames(models_coefs)[which.max(models_coefs$days_since_lockdown)] # Isle of Wight has the greatest increase per day at 0.364

# Exploring the trends in the intercept (point at the start of lockdown)
min(models_coefs$'(Intercept)')
rownames(models_coefs)[which.min(models_coefs$'(Intercept)')] # Bath has the lowest intercept at -66.37

max(models_coefs$'(Intercept)')
rownames(models_coefs)[which.max(models_coefs$'(Intercept)')] # Peterborough has the highest intercept at -43.39









## Quantifying mobility drops ---------------------

# Minimum workplace mobility reached in this period
min_england_lockdown1<-min(google_england_lockdown1$workplaces_percent_change_from_baseline)

# Change as a result of lockdown
drop_lockdown1 <- average_pre_lockdown1-min_england_lockdown1 
drop_lockdown1 # 79.7 percentage point max decrease in mobility

regional_drop_summary<-NA
min=NA
average_pre=NA
drop=NA

for (i in 1:length(england_sub_region_1)){
  google_regional <- google_england_lockdown1 %>% filter(sub_region_1==england_sub_region_1[i])
  google_regional_prelockdown <- google_england %>% filter(sub_region_1==england_sub_region_1[i])
  min = min(google_regional$workplaces_percent_change_from_baseline[!(google_regional$date %in% bank_holidays)], na.rm=TRUE)
  print(min)
  min_id = which.min(google_regional$workplaces_percent_change_from_baseline)
  mobility_30days = mean(google_regional$workplaces_percent_change_from_baseline[min_id+27:min_id+33]) # Centre rolling average to smooth
  rebound_30days = mobility_30days -min
  print(rebound_30days)
  average_pre = mean(google_regional_prelockdown$workplaces_percent_change_from_baseline[google_regional_prelockdown$date<pre_lockdown1_end_date &google_regional_prelockdown$date>=pre_lockdown1_end_date-14], na.rm=TRUE)
  print(average_pre)
  drop = average_pre - min 
  regional_drop_summary <- rbind(regional_drop_summary,c(england_sub_region_1[i],min,average_pre,drop,min_id,mobility_30days, rebound_30days))
}


regional_drop_summary <- regional_drop_summary[-1,]

colnames(regional_drop_summary) <- c("sub_region_1","Min_during_lockdown","Pre_lockdown_av","Drop","Min_days","Mobility_30days","Rebound_30days")
regional_drop_summary <- as.data.frame(regional_drop_summary)


regional_drop_summary
# Exporting this as csv
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
write.csv(regional_drop_summary,"Regional_lockdown1_drop.csv")

# Smallest drop for... Greater London at 67.5 percentage points
min(regional_drop_summary$Drop)
regional_drop_summary$sub_region_1[which.min(regional_drop_summary$Drop)]

# Greatest drop for... Rutland
max(regional_drop_summary$Drop)
regional_drop_summary$sub_region_1[which.max(regional_drop_summary$Drop)]

# Excluding Rutland (as this is too small)!
max(regional_drop_summary$Drop[regional_drop_summary$sub_region_1!="Rutland"])
regional_drop_summary$sub_region_1[which.max(regional_drop_summary$Drop[regional_drop_summary$sub_region_1!="Rutland"])] # Central Bedforshire has the greatest drop at 75.7











# First lockdown - other mobility measures ------------------------------------

mobility_vars <-c("retail_and_recreation_percent_change_from_baseline", 
                  "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", 
                  "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
                  "residential_percent_change_from_baseline")


## GB as a whole ---------------------------------------------------------------


### Retail and recreation -------------------------------------------------


retail_model_1 <- lm(retail_and_recreation_percent_change_from_baseline ~ day + bank_holiday + days_since_lockdown, data=google_lockdown1)
google_lockdown1$retail_pred_1 <- predict(retail_model_1, google_lockdown1)



ggplot(data=google_lockdown1, aes(x=date,y=retail_and_recreation_percent_change_from_baseline)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=retail_pred_1,color="Linear model"))+
  geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Retail and recreation") +
  theme_light()  


### Grocery and pharmacy ----------------------------------------------------


grocery_model_1 <- lm(grocery_and_pharmacy_percent_change_from_baseline ~ day + days_since_lockdown + bank_holiday, data=google_lockdown1)
google_lockdown1$grocery_pred_1 <- predict(grocery_model_1, google_lockdown1)
tbl_regression(grocery_model_1)

ggplot(data=google_lockdown1, aes(x=date,y=grocery_and_pharmacy_percent_change_from_baseline)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=grocery_pred_1,color="Linear model"))+
  geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Grocery and pharmacy") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")


### Parks -------------------------------------------------------------------


parks_model_1 <- lm(parks_percent_change_from_baseline ~ day + days_since_lockdown + bank_holiday, data=google_lockdown1)
google_lockdown1$parks_pred_1 <- predict(parks_model_1, google_lockdown1)
tbl_regression(parks_model_1)

ggplot(data=google_lockdown1, aes(x=date,y=parks_percent_change_from_baseline)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=parks_pred_1,color="Linear model"))+
  geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Parks") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")


### Transit stations --------------------------------------------------------


transit_model_1 <- lm(transit_stations_percent_change_from_baseline ~ day + days_since_lockdown + bank_holiday, data=google_lockdown1)
google_lockdown1$transit_pred_1 <- predict(transit_model_1, google_lockdown1)
tbl_regression(transit_model_1)

ggplot(data=google_lockdown1, aes(x=date,y=transit_stations_percent_change_from_baseline)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=transit_pred_1,color="Linear model"))+
  geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Transit stations") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")




#  Second lockdown - workplace mobility -----------------------------------


## GB as a whole ---------------------------------------------------------


pre_lockdown2_end_date <-as.Date("2020-10-31","%Y-%m-%d") # Not using 5th November at the moment, but the date it was announced

average_pre_lockdown2<-mean(google_gb$workplaces_percent_change_from_baseline[google_gb$date<pre_lockdown2_end_date&google_gb$date>=pre_lockdown2_end_date-14])
average_pre_lockdown2
# Note substantially lower than lockdown1 baseline mobility (makes sense)

# Looking at the first lockdown:
# Start date - 5/11/2020 (2nd lockdown legally comes into force)
# End date - 15/06/2020 (lockdown ends - into tiered system - so there are still restrictions)

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

google_lockdown2 <- google_gb %>% filter(date<=lockdown_2_end) %>% filter (date>=lockdown_2_start)
nrow(google_lockdown2) #28 days of data - very short

# Days since lockdown
google_lockdown2$days_since_lockdown <- google_lockdown2$date - lockdown_2_start
summary(google_lockdown1$days_since_lockdown)

# Minimum workplace mobility reached in this period
min_lockdown2<-min(google_lockdown2$workplaces_percent_change_from_baseline)
min_lockdown2

# Change as a result of lockdown
drop_lockdown2<- average_pre_lockdown2-min_lockdown2 
drop_lockdown2 # 7.92 percentage point max decrease in mobility - already starting a lot lower with this lockdown



# Linear model
# NOTE - bank holidays not included as there are no bank holidays during this period

workplace_model_2 <- lm(workplaces_percent_change_from_baseline ~ day  + days_since_lockdown, data=google_lockdown2 )
summary(workplace_model_2) # mobility significantly increases with days from lockdown
summary(workplace_model_2)$coef[8] # increase per day in mobility
summary(workplace_model_2)$coef[8]*30 # increase after 30 days -> 4.04 percentage points

# Saving results
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
tbl_regression(workplace_model_2)
gt::gtsave(as_gt(tbl_regression(workplace_model_2)), "GB_workplace_lockdown2_regression_results.png")
dev.off()

# Checking assumptions
plot(workplace_model_2,1)
plot(workplace_model_2,2) # Assumptions pretty violated...

google_lockdown2$workplace_pred_2 <- predict(workplace_model_2, google_lockdown2)

# Plot of the model
plot_gb_lockdown2 <- ggplot(data=google_lockdown2, aes(x=date,y=workplaces_percent_change_from_baseline)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=workplace_pred_2,color="Linear model"))+
  geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Workplace mobility during lockdown 2") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")


plot_gb_lockdown2
ggsave("GB_workplace_lockdown2_regression_plot.pdf", plot=plot_gb_lockdown2, device="pdf")




## England - regional regressions---------------------------------------------------


# Defining data for first lockdown
google_england_lockdown2 <- google_england %>% filter(date<=lockdown_2_end) %>% filter (date>=lockdown_2_start)

# Defining days since lockdown
google_england_lockdown2$days_since_lockdown <- google_england_lockdown2$date - lockdown_2_start




# Fitting a model for each region (no bank holidays in this period)

library(lme4)

models_2 <- lmList(workplaces_percent_change_from_baseline ~ 
                   day +  days_since_lockdown | sub_region_1, 
                   data=google_england_lockdown2)

# Gives all models
models_2_coefs<-coef(models_2, augFrame = TRUE)


# Changing the column order
models_2_coefs

# Exporting this as csv
write.csv(models_2_coefs,"Regional_workplace_lockdown2_regression_results.csv")

# Exploring the trends in the increase in mobility since 2nd lockdown
min(models_2_coefs$days_since_lockdown)
rownames(models_2_coefs)[which.min(models_2_coefs$days_since_lockdown)] # York has the smallest increase again

max(models_2_coefs$days_since_lockdown)
rownames(models_coefs)[which.max(models_2_coefs$days_since_lockdown)] # Isle of Wight has the greatest increase again

# Exploring the trends in the intercept (point at the start of lockdown)
min(models_2_coefs$'(Intercept)')
rownames(models_2_coefs)[which.min(models_2_coefs$'(Intercept)')] # Bath has the lowest intercept again

max(models_2_coefs$'(Intercept)')
rownames(models_2_coefs)[which.max(models_2_coefs$'(Intercept)')] # Peterborough has the highest intercept again...





## Quantifying mobility drops ---------------------

# Minimum workplace mobility reached in this period
min_england_lockdown2<-min(google_england_lockdown2$workplaces_percent_change_from_baseline)

# Change as a result of lockdown
drop_lockdown2 <- average_pre_lockdown2-min_england_lockdown2 
drop_lockdown2 # 20.93 percentage point max decrease in mobility

regional_drop_summary_2<-NA
min<-NA
average_pre<-NA
drop<-NA
mobility_14days<-NA
rebound_14days<-NA

for (i in 1:length(england_sub_region_1)){
  print(i)
  google_regional_2 <- google_england_lockdown2 %>% filter(sub_region_1==england_sub_region_1[i])
  print(england_sub_region_1[i])
  google_regional_prelockdown2 <- google_england %>% filter(sub_region_1==england_sub_region_1[i])
  min = min(google_regional_2$workplaces_percent_change_from_baseline[!(google_regional_2$date %in% bank_holidays)], na.rm=TRUE)
  print(min)
  min_id = which.min(google_regional_2$workplaces_percent_change_from_baseline)
  mobility_14days = mean(google_regional_2$workplaces_percent_change_from_baseline[min_id+14]) # Centre rolling average to smooth
  print(mobility_14days)
  rebound_14days = mobility_14days -min
  print(rebound_14days)
  average_pre = mean(google_regional_prelockdown2$workplaces_percent_change_from_baseline[google_regional_prelockdown2$date<pre_lockdown2_end_date &google_regional_prelockdown2$date>=pre_lockdown2_end_date-14], na.rm=TRUE)
  print(average_pre)
  drop = average_pre - min 
  regional_drop_summary_2 <- rbind(regional_drop_summary_2,c(england_sub_region_1[i],min,average_pre,drop,min_id,mobility_14days, rebound_14days))
}


regional_drop_summary_2 <- regional_drop_summary_2[-1,]

colnames(regional_drop_summary_2) <- c("sub_region_1","Min_during_lockdown","Pre_lockdown_av","Drop","Min_days","Mobility_14days","Rebound_14days")
regional_drop_summary_2 <- as.data.frame(regional_drop_summary_2)

regional_drop_summary_2

## The lockdown may be too short for this to work well - not all get beyond 14 days after the minium...

# Exporting this as csv
write.csv(regional_drop_summary_2,"Regional_lockdown2_drop.csv")

# Smallest drop for... Rutland at 10
min(regional_drop_summary_2$Drop)
regional_drop_summary_2$sub_region_1[which.min(regional_drop_summary_2$Drop)]

# Smallest drop excluding Rutland (as so small) -> should I look at excluding other small regions?
min(regional_drop_summary_2$Drop[regional_drop_summary_2$sub_region_1!="Rutland"])
regional_drop_summary_2$sub_region_1[which.max(regional_drop_summary_2$Drop[regional_drop_summary_2$sub_region_1!="Rutland"])] # Now Thurrock


# Greatest drop for... Torbay
max(regional_drop_summary_2$Drop)
regional_drop_summary_2$sub_region_1[which.max(regional_drop_summary_2$Drop)]







# Third lockdown - workplace mobility -------------------------------------

## GB as a whole ---------------------------------------------------------

pre_lockdown3_end_date <-as.Date("2021-04-01","%Y-%m-%d") # Using the warning date

average_pre_lockdown3<-mean(google_gb$workplaces_percent_change_from_baseline[google_gb$date<pre_lockdown3_end_date&google_gb$date>=pre_lockdown3_end_date-14])
average_pre_lockdown3
# Note substantially lower than lockdown1, a bit lower than lockdown 2 - note this was however christmas period!

# Looking at the first lockdown:
# Start date - 06/01/2021 (3rd lockdown legally comes into force)
# End date - 12/04/2021 (non-essential retail reopens)

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

google_lockdown3 <- google_gb %>% filter(date<=lockdown_3_end) %>% filter (date>=lockdown_3_start)
nrow(google_lockdown3) #106 days of data - decent

# Days since lockdown
google_lockdown3$days_since_lockdown <- google_lockdown3$date - lockdown_3_start
summary(google_lockdown3$days_since_lockdown)

# Minimum workplace mobility reached in this period
min_lockdown3<-min(google_lockdown3$workplaces_percent_change_from_baseline)
min_lockdown3

# Change as a result of lockdown
drop_lockdown3<- average_pre_lockdown3-min_lockdown3 
drop_lockdown3 # 40.29 percentage point max decrease in mobility - quite a drop again




# Linear model
# NOTE - bank holidays added back in, although there are only two during this period...

workplace_model_3 <- lm(workplaces_percent_change_from_baseline ~ day  + bank_holiday + days_since_lockdown, data=google_lockdown3 )
summary(workplace_model_3) # mobility significantly increases with days from lockdown
summary(workplace_model_3)$coef[9] # increase per day in mobility
summary(workplace_model_3)$coef[9]*30 # increase after 30 days -> 4.35 percentage points

# Saving results
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
tbl_regression(workplace_model_3)
gt::gtsave(as_gt(tbl_regression(workplace_model_3)), "GB_workplace_lockdown3_regression_results.png")
dev.off()

# Checking assumptions
plot(workplace_model_3,1)
plot(workplace_model_3,2) # heavy left tail

google_lockdown3$workplace_pred_3 <- predict(workplace_model_3, google_lockdown3)

# Plot of the model
plot_gb_lockdown3 <- ggplot(data=google_lockdown3, aes(x=date,y=workplaces_percent_change_from_baseline)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=workplace_pred_3,color="Linear model"))+
  geom_smooth(se=FALSE, aes(color="Smoothed line"),) +
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red","Smoothed line"="Dark gray")) +
  xlab("Date (2021)") +
  ylab("Change from baseline (%)") +
  ggtitle("Workplace mobility during lockdown 3") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")


plot_gb_lockdown3
ggsave("GB_workplace_lockdown3_regression_plot.pdf", plot=plot_gb_lockdown3, device="pdf")





## England - regional regressions ------------------------------------------------


# Defining data for first lockdown
google_england_lockdown3 <- google_england %>% filter(date<=lockdown_3_end) %>% filter (date>=lockdown_3_start)

# Defining days since lockdown
google_england_lockdown3$days_since_lockdown <- google_england_lockdown3$date - lockdown_3_start




# Fitting a model for each region - bank holidays includes (although only 2)

models_3 <- lmList(workplaces_percent_change_from_baseline ~ 
                     day + bank_holiday + days_since_lockdown | sub_region_1, 
                   data=google_england_lockdown3)

# Gives all models
models_3_coefs<-coef(models_3, augFrame = TRUE)

models_3_coefs

# Exporting this as csv
write.csv(models_3_coefs,"Regional_workplace_lockdown3_regression_results.csv")

# Exploring the trends in the increase in mobility since 2nd lockdown
min(models_3_coefs$days_since_lockdown)
rownames(models_3_coefs)[which.min(models_3_coefs$days_since_lockdown)] # York has the smallest increase again

max(models_3_coefs$days_since_lockdown)
rownames(models_coefs)[which.max(models_3_coefs$days_since_lockdown)] # Isle of Wight has the greatest increase again

# Exploring the trends in the intercept (point at the start of lockdown)
min(models_3_coefs$'(Intercept)')
rownames(models_3_coefs)[which.min(models_3_coefs$'(Intercept)')] # Bath has the lowest intercept again

max(models_3_coefs$'(Intercept)')
rownames(models_3_coefs)[which.max(models_3_coefs$'(Intercept)')] # Peterborough has the highest intercept again...




## Quantifying mobility drops ---------------------

# Minimum workplace mobility reached in this period
min_england_lockdown3<-min(google_england_lockdown3$workplaces_percent_change_from_baseline)

# Change as a result of lockdown
drop_lockdown3 <- average_pre_lockdown3-min_england_lockdown3 
drop_lockdown3 # 49.29 percentage point max decrease in mobility

regional_drop_summary_3<-NA
min<-NA
average_pre<-NA
drop<-NA


for (i in 1:length(england_sub_region_1)){
  google_regional_3 <- google_england_lockdown3 %>% filter(sub_region_1==england_sub_region_1[i])
  google_regional_prelockdown3 <- google_england %>% filter(sub_region_1==england_sub_region_1[i])
  min = min(google_regional_3$workplaces_percent_change_from_baseline[!(google_regional_3$date %in% bank_holidays)], na.rm=TRUE)
  print(min)
  min_id = which.min(google_regional_3$workplaces_percent_change_from_baseline[!(google_regional_3$date %in% bank_holidays)])
  mobility_30days = mean(google_regional_3$workplaces_percent_change_from_baseline[min_id+27:min_id+33],na.rm=TRUE) # Centre rolling average to smooth
  rebound_30days = mobility_30days -min
  print(rebound_30days)
  average_pre = mean(google_regional_prelockdown3$workplaces_percent_change_from_baseline[google_regional_prelockdown3$date<pre_lockdown3_end_date &google_regional_prelockdown3$date>=pre_lockdown3_end_date-14], na.rm=TRUE)
  print(average_pre)
  drop = average_pre - min 
  regional_drop_summary_3 <- rbind(regional_drop_summary_3,c(england_sub_region_1[i],min,average_pre,drop,min_id,mobility_30days, rebound_30days))
}


regional_drop_summary_3 <- regional_drop_summary_3[-1,]

colnames(regional_drop_summary_3) <- c("sub_region_1","Min_during_lockdown","Pre_lockdown_av","Drop","Min_days","Mobility_30days","Rebound_30days")
regional_drop_summary_3 <- as.data.frame(regional_drop_summary_3)

regional_drop_summary_3



# Exporting this as csv
write.csv(regional_drop_summary_3,"Regional_lockdown3_drop.csv")

# Smallest drop for... York
min(regional_drop_summary_3$Drop)
regional_drop_summary_3$sub_region_1[which.min(regional_drop_summary_3$Drop)]

# Greatest drop for... Shropshire
max(regional_drop_summary_3$Drop)
regional_drop_summary_3$sub_region_1[which.max(regional_drop_summary_3$Drop)]



