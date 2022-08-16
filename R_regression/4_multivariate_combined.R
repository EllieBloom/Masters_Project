# Regression with all metrics combined

# Date started: 16 August 2022



# Setup -------------------------------------------------------------------


library(tidyverse)


# Data and dates ----------------------------------------------------------


combined_england <- readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_england.rds")
combined_regional <-readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_regional.rds")



# Useful dates
lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

react_reprod_start <- as.Date("2020-05-15","%Y-%m-%d")


# Filtering the data to the period of interest

combined_england <- combined_england %>% filter(date>=react_reprod_start, date<=lockdown_3_end)
combined_regional <- combined_regional %>% filter(date>=react_reprod_start, date<=lockdown_3_end)

dput(colnames(combined_england))

# England -----------------------------------------------------------------

## Model a -> just mobliity and vaccination rates:------------------------------

model_england_a <- lm(r ~ retail_recreation_av + grocery_pharmacy_av + transit_stations_av + workplaces_av + residential_av +
                    cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england)

model_england_summary_a <- as.data.frame(cbind(summary(model_england_a)$coef, confint(model_england_a)))
model_england_summary_a$model <- "a"
model_england_summary_a$region <- "england"
model_england_summary_a

## Model b -> adding in lockdowns ------------------------------

model_england_b <- lm(r ~ retail_recreation_av + grocery_pharmacy_av + transit_stations_av + workplaces_av + residential_av +
                        cumVaccinationFirstDoseUptakeByPublishDatePercentage + lockdown, data=combined_england)

model_england_summary_b <- as.data.frame(cbind(summary(model_england_b)$coef, confint(model_england_b)))
model_england_summary_b$model <- "b"
model_england_summary_b$region <- "england"
model_england_summary_b





# Regional ---------------------------------------------------------------

regions <- dput(unique(combined_regional$region))

# Note that the vaccination metric is DIFFERENT!!

## Model a  ----------------------------------------------------------------


model_regional_a_summary <- NA

for (i in 1:length(regions)){
  data <- combined_regional %>% filter(region==regions[i])
  model <- lm(r ~ retail_recreation_av + grocery_pharmacy_av + transit_stations_av + workplaces_av + residential_av +
                cumVaccinationFirstDoseUptakeByVaccinationDatePercentage, data=data)
  summary <- as.data.frame(cbind(summary(model)$coef, confint(model)))
  summary$model <- "a"
  summary$region <- regions[i]
  model_regional_a_summary <- rbind(model_regional_a_summary, summary)
}

model_regional_a_summary <- model_regional_a_summary[-1,]
model_regional_a_summary

## Model b  ----------------------------------------------------------------


model_regional_b_summary <- NA

for (i in 1:length(regions)){
  data <- combined_regional %>% filter(region==regions[i])
  model <- lm(r ~ retail_recreation_av + grocery_pharmacy_av + transit_stations_av + workplaces_av + residential_av +
                cumVaccinationFirstDoseUptakeByVaccinationDatePercentage + lockdown, data=data)
  summary <- as.data.frame(cbind(summary(model)$coef, confint(model)))
  summary$model <- "b"
  summary$region <- regions[i]
  model_regional_b_summary <- rbind(model_regional_b_summary, summary)
}

model_regional_b_summary <- model_regional_b_summary[-1,]
model_regional_b_summary
