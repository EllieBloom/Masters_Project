# Univariate regression with each mobility place metric

# Date started: 16 August 2022




# Setup --------------------------------------------------------------------

library(tidyverse)




# Data --------------------------------------------------------------------


combined_england <- readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_england.rds")
combined_regional <- readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_regional.rds")

colnames(combined_england)
colnames(combined_regional)



# Univariate - england ----------------------------------------------------

lm_england_workplaces <- lm(r ~ workplaces, combined_england)

summary(lm_england_workplaces)$coef

dput(colnames(combined_england))

mobility_av_types <- c("retail_recreation_av", "grocery_pharmacy_av", "transit_stations_av", "workplaces_av","residential_av")

england_univariate_summary <- NA

for (i in 1:length(mobility_av_types)){
  mobility_type = mobility_av_types[i]
  model <- lm(r ~ eval(parse(text=mobility_av_types[i])) , combined_england)
  summary <- as.data.frame(cbind(summary(model)$coef, confint(model)))
  summary$mobility <- mobility_av_types[i]
  england_univariate_summary <- rbind(england_univariate_summary, summary)
}

england_univariate_summary <- england_univariate_summary[-1,]
england_univariate_summary



# Just the coefficients of interest

england_univariate_summary_mob <- england_univariate_summary %>% filter(grepl("eval", rownames(england_univariate_summary)))
england_univariate_summary_mob$region <- "England"
england_univariate_summary_mob




# Univariate - regional ---------------------------------------------------

regions_list <- dput(unique(combined_regional$region))
mobility_av_types

regional_univariate_summary <- NA

for (i in 1:length(mobility_av_types)){
  for (j in 1:length(regions_list)){
  mobility_type = mobility_av_types[i]
  region_interest = regions_list[j]
  data <- combined_regional %>% filter(region == region_interest)
  model <- lm(r ~ eval(parse(text=mobility_type)) , data)
  summary <- as.data.frame(cbind(summary(model)$coef, confint(model)))
  summary$mobility <- mobility_type
  summary$region <- region_interest
  regional_univariate_summary <- rbind(regional_univariate_summary, summary)
  }
}

regional_univariate_summary <- regional_univariate_summary[-1,]
regional_univariate_summary


# Just looking at the coefficients of interest
regional_univariate_summary_mob <- regional_univariate_summary %>% filter(grepl("eval", rownames(regional_univariate_summary)))
regional_univariate_summary_mob




# Combining England and regional ------------------------------------------

colnames(england_univariate_summary_mob)==colnames(regional_univariate_summary_mob)

univariate_combined_results <- rbind(england_univariate_summary_mob,regional_univariate_summary_mob)
univariate_combined_results

univariate_combined_results <- univariate_combined_results %>% arrange(region, mobility)

setwd("~/Desktop/Masters/Project/Analysis/R_regression/Ouputs/Univariate")
write.csv(univariate_combined_results, "univariate_combined_results.csv")
