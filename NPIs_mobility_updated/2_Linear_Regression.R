# Exploring the impact of NPIs on mobility

# Date started: 23rd May 2022

# Inspiration for some models:
# https://www.nature.com/articles/s41598-021-02133-1.pdf

# Additions - NEED TO EXTRACT ALL OF THE COEFFICIENT CONFIDENCE INTERVALS AND P VALUES


# Setup -------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(gtsummary)
library(zoo)
library(reshape2)
library(lme4)



# Loading data ------------------------------------------------------------


google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Filterting for GB dataset
google_gb <- google_overall %>% filter(sub_region_1=="") # Where there is no sub-region, the data is for GB overall


# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)


# Adding key dates --------------------------------------------------------


bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                           "%Y-%m-%d")

# Useful dates
lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")



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



# Defining mobility types -------------------------------------------------


mobility_types <- c("retail_and_recreation_percent_change_from_baseline", 
                    "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", 
                    "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
                    "residential_percent_change_from_baseline")



mobility_types_neat <- c("Retail and recreation", 
                         "Grocery and pharmacy", 
                         "Parks", 
                         "Transit stations", 
                         "Workplaces", 
                         "Residential")




# First lockdown - workplace mobility---------------------------------------------

## GB as a whole ---------------------------------------------------------


## Linear model ------------------------------------------------------------

### Lockdown 1 --------------------------------------------------------------
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression")

GB_lockdown1_summary <- NA

for (i in 1:length(mobility_types)){
  period = "Lockdown_1"
  start_date = lockdown_1_start
  end_date = lockdown_1_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_gb %>% filter(date>=start_date, date<= end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types[i])) ~ day  + bank_holiday + days_since_lockdown, data=data)
  summary <- summary(model)$coef
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]))
  GB_lockdown1_summary <- cbind(GB_lockdown1_summary,summary)
}

GB_lockdown1_summary <- GB_lockdown1_summary[,-1] 
write.csv(GB_lockdown1_summary,"UK_total_Lockdown_1.csv")



### Lockdown 2 --------------------------------------------------------------

# Note that lockdown 2 doesn't have bank holidays

GB_lockdown2_summary <- NA

for (i in 1:length(mobility_types)){
  period = "Lockdown_2"
  start_date = lockdown_2_start
  end_date = lockdown_2_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_gb %>% filter(date>=start_date, date<= end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types[i])) ~ day  +  days_since_lockdown, data=data)
  summary <- summary(model)$coef
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]))
  GB_lockdown2_summary <- cbind(GB_lockdown2_summary,summary)
}

GB_lockdown2_summary <- GB_lockdown2_summary[,-1] 
write.csv(GB_lockdown2_summary,"UK_total_Lockdown_2.csv")



### Lockdown 3 --------------------------------------------------------------

GB_lockdown3_summary <- NA

for (i in 1:length(mobility_types)){
  period = "Lockdown_3"
  start_date = lockdown_3_start
  end_date = lockdown_3_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_gb %>% filter(date>=start_date, date<= end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types[i])) ~ day  + bank_holiday + days_since_lockdown, data=data)
  summary <- summary(model)$coef
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]))
  GB_lockdown3_summary <- cbind(GB_lockdown3_summary,summary)
}

GB_lockdown3_summary <- GB_lockdown3_summary[,-1] 
write.csv(GB_lockdown3_summary,"UK_total_Lockdown_3.csv")












# England as a whole ------------------------------------------------------

google_england <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_wide.rds")

google_england <- google_england %>% filter(region=="ENGLAND")
google_england$bank_holiday <- ifelse(google_england$date %in% bank_holidays,1,0)
google_england$bank_holiday <- as.factor(google_england$bank_holiday)

# Adding days of the week into the google_gb dataset
google_england$day <- weekdays(google_england$date)

# Make into a factor
google_england$day <- factor(google_england$day)

# Sunday as reference category
google_england$day <- relevel(google_england$day, ref="Sunday")
str(google_england$day)


## Linear model ------------------------------------------------------------


### Lockdown 1 --------------------------------------------------------------
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression")
England_lockdown1_summary <- NA

mobility_types_england <- c("retail_recreation", "grocery_pharmacy", 
                            "parks", "transit_stations", "workplaces", "residential")

for (i in 1:length(mobility_types)){
  period = "Lockdown_1"
  start_date = lockdown_1_start
  end_date = lockdown_1_end
  mobility_type <- mobility_types_england[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_england %>% filter(date>=start_date, date<= end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types_england[i])) ~ day  + bank_holiday + days_since_lockdown, data=data)
  summary <- summary(model)$coef
  summary <- cbind(summary,confint(model))
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]),
                         paste0(mobility_type_neat,colnames(summary)[5]),
                         paste0(mobility_type_neat,colnames(summary)[6]))
  England_lockdown1_summary <- cbind(England_lockdown1_summary,summary)
}

England_lockdown1_summary <- England_lockdown1_summary[,-1]
England_lockdown1_summary
write.csv(England_lockdown1_summary,"England_total_Lockdown_1.csv")



# Workplace plot

google_england_lockdown1 <- google_england %>% filter(date>=lockdown_1_start, date<= lockdown_1_end)
google_england_lockdown1$days_since_lockdown <- as.numeric(google_england_lockdown1$date - lockdown_1_start)
workplaces_model_1 <- lm(workplaces ~ day + bank_holiday + days_since_lockdown, data=google_england_lockdown1)
google_england_lockdown1$workplaces_pred_1 <- predict(workplaces_model_1)



ggplot(data=google_england_lockdown1, aes(x=date,y=workplaces)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=workplaces_pred_1,color="Linear model"))+
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Workplaces mobility") +
  theme_light()  



### Lockdown 2 --------------------------------------------------------------

England_lockdown2_summary <- NA

for (i in 1:length(mobility_types)){
  period = "Lockdown_2"
  start_date = lockdown_2_start
  end_date = lockdown_2_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_england %>% filter(date>=start_date, date<= end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types_england[i])) ~ day  +  days_since_lockdown, data=data)
  summary <- summary(model)$coef
  summary <- cbind(summary,confint(model))
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]),
                         paste0(mobility_type_neat,colnames(summary)[5]),
                         paste0(mobility_type_neat,colnames(summary)[6]))
  England_lockdown2_summary <- cbind(England_lockdown2_summary,summary)
}

England_lockdown2_summary <- England_lockdown2_summary[,-1]
England_lockdown2_summary
write.csv(England_lockdown2_summary,"England_total_Lockdown_2.csv")

# Workplace plot

google_england_lockdown2 <- google_england %>% filter(date>=lockdown_2_start, date<= lockdown_2_end)
google_england_lockdown2$days_since_lockdown <- as.numeric(google_england_lockdown2$date - lockdown_2_start)
workplaces_model_2 <- lm(workplaces ~ day + days_since_lockdown, data=google_england_lockdown2)
google_england_lockdown2$workplaces_pred_2 <- predict(workplaces_model_2)



ggplot(data=google_england_lockdown2, aes(x=date,y=workplaces)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=workplaces_pred_2,color="Linear model"))+
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Workplace mobility") +
  theme_light()  

### Lockdown 3 --------------------------------------------------------------

England_lockdown3_summary <- NA

for (i in 1:length(mobility_types)){
  period = "Lockdown_3"
  start_date = lockdown_3_start
  end_date = lockdown_3_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_england %>% filter(date>=start_date, date<= end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types_england[i])) ~ day  + bank_holiday + days_since_lockdown, data=data)
  summary <- summary(model)$coef
  summary <- cbind(summary,confint(model))
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]),
                         paste0(mobility_type_neat,colnames(summary)[5]),
                         paste0(mobility_type_neat,colnames(summary)[6]))
  England_lockdown3_summary <- cbind(England_lockdown3_summary,summary)
}

England_lockdown3_summary <- England_lockdown3_summary[,-1]
England_lockdown3_summary
write.csv(England_lockdown3_summary,"England_total_Lockdown_3.csv")


# Workplace plot

google_england_lockdown3 <- google_england %>% filter(date>=lockdown_3_start, date<= lockdown_3_end)
google_england_lockdown3$days_since_lockdown <- as.numeric(google_england_lockdown3$date - lockdown_3_start)
workplaces_model_3 <- lm(workplaces ~ day + bank_holiday + days_since_lockdown, data=google_england_lockdown3)
google_england_lockdown3$workplaces_pred_1 <- predict(workplaces_model_3)



ggplot(data=google_england_lockdown3, aes(x=date,y=workplaces)) +
  geom_line(aes(color="Mobility")) + 
  geom_line(aes(x=date, y=workplaces_pred_1,color="Linear model"))+
  scale_color_manual(name="",
                     breaks=c("Mobility","Linear model","Smoothed line"),
                     values=c("Mobility"="Dark blue","Linear model"="Red")) +
  xlab("Date (2020)") +
  ylab("Change from baseline (%)") +
  ggtitle("Workplaces mobility") +
  theme_light()  



# Regions in England ------------------------------------------------------

england_regions <- c("Bath and North East Somerset","Bedford", "Blackburn with Darwen",
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


google_regional_england <- google_overall %>% filter(sub_region_1%in%england_regions, sub_region_2=="")


## Lockdown 1 --------------------------------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression")

for (i in 1:length(mobility_types)){
  period <- "Lockdown_1"
  start_date <- lockdown_1_start
  end_date <- lockdown_1_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_regional_england %>% filter(date>=start_date, date<=end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  models <- lmList(eval(parse(text=mobility_types[i])) ~ day + bank_holiday + days_since_lockdown | sub_region_1, data=data)
  models_coefs<-coef(models, augFrame = TRUE)
  col_order <-c("(Intercept)", "days_since_lockdown", "bank_holiday1",
                "dayMonday","dayTuesday", "dayWednesday", 
                "dayThursday",  "dayFriday","daySaturday")
  models_coefs <- models_coefs[, col_order]
  results_name <- paste0("England_regional_",period,"_",mobility_type_neat,".csv")
  write.csv(models_coefs,results_name)
  #coefs_confint <- summary(models)$coef
  #confint_name <- paste0("confint_england_regional_",period,"_",mobility_type_neat,".rds")
  #saveRDS(coefs_confint,confint_name)
}

# Saving the RDSs manually - fails for some n- leaving out for the moment
# i=6
# period <- "Lockdown_1"
# start_date <- lockdown_1_start
# end_date <- lockdown_1_end
# mobility_type <- mobility_types[i]
# mobility_type_neat <- mobility_types_neat[i]
# data <- google_regional_england %>% filter(date>=start_date, date<=end_date)
# data$days_since_lockdown <- as.numeric(data$date - start_date)
# models <- lmList(eval(parse(text=mobility_types[i])) ~ day + bank_holiday + days_since_lockdown | sub_region_1, data=data)
# models_coefs<-coef(models, augFrame = TRUE)
# col_order <-c("(Intercept)", "days_since_lockdown", "bank_holiday1",
#               "dayMonday","dayTuesday", "dayWednesday", 
#               "dayThursday",  "dayFriday","daySaturday")
# 
# coefs_confint <- summary(models)$coef
# confint_name <- paste0("confint_england_regional_",period,"_",mobility_type_neat,".rds")
# saveRDS(coefs_confint,confint_name)




## Lockdown 2 --------------------------------------------------------------


for (i in 1:length(mobility_types)){
  period <- "Lockdown_2"
  start_date <- lockdown_2_start
  end_date <- lockdown_2_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_regional_england %>% filter(date>=start_date, date<=end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  models <- lmList(eval(parse(text=mobility_types[i])) ~ day + days_since_lockdown | sub_region_1, data=data)
  models_coefs<-coef(models, augFrame = TRUE)
  col_order <-c("(Intercept)", "days_since_lockdown", 
                "dayMonday","dayTuesday", "dayWednesday", 
                "dayThursday",  "dayFriday","daySaturday")
  models_coefs <- models_coefs[, col_order]
  results_name <- paste0("England_regional_",period,"_",mobility_type_neat,".csv")
  write.csv(models_coefs,results_name)
}

## Lockdown 3 --------------------------------------------------------------


for (i in 1:length(mobility_types)){
  period <- "Lockdown_3"
  start_date <- lockdown_3_start
  end_date <- lockdown_3_end
  mobility_type <- mobility_types[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_regional_england %>% filter(date>=start_date, date<=end_date)
  data$days_since_lockdown <- as.numeric(data$date - start_date)
  models <- lmList(eval(parse(text=mobility_types[i])) ~ day + bank_holiday + days_since_lockdown | sub_region_1, data=data)
  models_coefs<-coef(models, augFrame = TRUE)
  col_order <-c("(Intercept)", "days_since_lockdown", "bank_holiday1",
                "dayMonday","dayTuesday", "dayWednesday", 
                "dayThursday",  "dayFriday","daySaturday")
  models_coefs <- models_coefs[, col_order]
  results_name <- paste0("England_regional_",period,"_",mobility_type_neat,".csv")
  write.csv(models_coefs,results_name)
}





# For whole period - England ------------------------------------------------------

# Note that this is without the moving average

mobility_types_av <- c("retail_recreation_av", 
                       "grocery_pharmacy_av", "parks_av", "transit_stations_av", "workplaces_av", 
                       "residential_av")


# Add lockdown variables

google_england$lockdown1 <- as.factor(ifelse(google_england$date>=lockdown_1_start & google_england$date<=lockdown_1_end,1,0))
google_england$lockdown2 <- as.factor(ifelse(google_england$date>=lockdown_2_start & google_england$date<=lockdown_2_end,1,0))
google_england$lockdown3 <- as.factor(ifelse(google_england$date>=lockdown_3_start & google_england$date<=lockdown_3_end,1,0))


# Linear model for each mobility types

England_whole_summary <- NA

for (i in 1:length(mobility_types_av)){
  period = "Whole period"
  start_date = lockdown_1_start
  end_date = lockdown_3_end
  mobility_type <- mobility_types_av[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- google_england %>% filter(date>=start_date,date<= end_date)
  data$days_since_start <- as.numeric(data$date - start_date)
  model <- lm(eval(parse(text=mobility_types_av[i])) ~  days_since_start +lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start, data=data)
  summary <- summary(model)$coef
  summary <- cbind(summary,confint(model))
  colnames(summary) <- c(paste0(mobility_type_neat,colnames(summary)[1]),
                         paste0(mobility_type_neat,colnames(summary)[2]),
                         paste0(mobility_type_neat,colnames(summary)[3]),
                         paste0(mobility_type_neat,colnames(summary)[4]),
                         paste0(mobility_type_neat,colnames(summary)[5]),
                         paste0(mobility_type_neat,colnames(summary)[6]))
  England_whole_summary <- cbind(England_whole_summary,summary)
}

England_whole_summary <- England_whole_summary[,-1]
England_whole_summary


setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression")
write.csv(England_whole_summary,"England-total_whole_time.csv")







# For whole period - regional ---------------------------------------------

library(lubridate)
str(google_regional_england$date)
google_regional_england <- google_regional_england[order(as.Date(google_regional_england$date, format="%Y-%m-%d")),]
head(google_regional_england)

region_list <- dput(unique(google_regional_england$sub_region_1))
df_combined <- NA

# Loop to add rolling averages
for (i in 1:length(region_list)){
  df_region <- google_regional_england %>% filter(sub_region_1==region_list[i])
  df_region <- df_region %>%
    mutate(retail_recreation_av = rollmean(retail_and_recreation_percent_change_from_baseline, k=7, fill=NA, align="center") ,
           grocery_pharmacy_av = rollmean(grocery_and_pharmacy_percent_change_from_baseline, k=7, fill=NA, align="center") ,
           parks_av = rollmean(parks_percent_change_from_baseline, k=7, fill=NA, align="center"),
           transit_stations_av = rollmean(transit_stations_percent_change_from_baseline, k=7, fill=NA, align="center"),
           workplaces_av = rollmean(workplaces_percent_change_from_baseline, k=7, fill=NA, align="center"),
           residential_av = rollmean(residential_percent_change_from_baseline, k=7, fill=NA, align="center")) 
  
  df_combined <- rbind(df_combined, df_region)
}

df_combined <- df_combined[-1,]
regional_summary <- df_combined


























# Add lockdown variables

regional_summary$lockdown1 <- as.factor(ifelse(regional_summary$date>=lockdown_1_start & regional_summary$date<=lockdown_1_end,1,0))
regional_summary$lockdown2 <- as.factor(ifelse(regional_summary$date>=lockdown_2_start & regional_summary$date<=lockdown_2_end,1,0))
regional_summary$lockdown3 <- as.factor(ifelse(regional_summary$date>=lockdown_3_start & regional_summary$date<=lockdown_3_end,1,0))


# Linear model for each mobility types

England_whole_summary <- NA

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression")

for (i in 1:length(mobility_types_av)){
  period <- "Whole period"
  start_date <- lockdown_1_start
  end_date <- lockdown_3_end
  mobility_type <- mobility_types_av[i]
  mobility_type_neat <- mobility_types_neat[i]
  data <- regional_summary %>% filter(date>=start_date, date<=end_date)
  data$days_since_start <- as.numeric(data$date - start_date)
  models <- lmList(eval(parse(text=mobility_types_av[i])) ~ days_since_start +lockdown1*days_since_start + lockdown2*days_since_start + lockdown3*days_since_start | sub_region_1, data=data)
  models_coefs<-coef(models, augFrame = TRUE)
  results_name <- paste0("England_regional_",period,"_",mobility_type_neat,".csv")
  write.csv(models_coefs,results_name)
}

