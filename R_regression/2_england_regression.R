# Regressing R against mobility and vaccination rate


# Date started: 15 August 2022



# Setup -------------------------------------------------------------------

library(tidverse)



# Reading England data ----------------------------------------------------


combined_england <- readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_england.rds")


# Columns in the dataset for reference:
dput(colnames(combined_england))
# c("date", "region", "retail_recreation", "grocery_pharmacy", 
#   "parks", "transit_stations", "workplaces", "residential", "retail_recreation_av", 
#   "grocery_pharmacy_av", "parks_av", "transit_stations_av", "workplaces_av", 
#   "residential_av", "areaType", "areaName", "areaCode", "cumVaccinationFirstDoseUptakeByPublishDatePercentage", 
#   "cumVaccinationSecondDoseUptakeByPublishDatePercentage", "cumVaccinationThirdInjectionUptakeByPublishDatePercentage", 
#   "r", "lb_2.5", "lb_25", "ub_97.5", "ub_75", "prob", "lockdown1", 
#   "lockdown2", "lockdown3")





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


# Creating a **ROUGH** indicator variable for Alpha variant by year

combined_england <- combined_england %>%
     mutate(variant = case_when(date>=as.Date("2021-01-01","%Y-%m-%d")~"Alpha",
                                date< as.Date("2021-01-01","%Y-%m-%d")~"Original"))

 # Playing around with regression ------------------------------------------

# Things to think about: should I be lagging in some way?
# Should I be including date in some way?

model <- lm(r ~  variant + lockdown + workplaces_av + residential_av + retail_recreation_av + grocery_pharmacy_av + transit_stations_av + cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england)
summary(model)$r.squared
summary(model)$adj.r.squared
model_results <- cbind(summary(model)$coef,confint(model))
model_results

# QQplot looks ok-ish?
plot(model,1) # shows homoskedasticity - non-linearity
plot(model,2) # ok - residualas are nomral
plot(model,3) # ok - constant standardised residuals
plot(model,5) 

# Other diagnostics...


model_log <-  lm(log(r) ~ variant + lockdown + workplaces_av + cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england)
summary(model_log)$r.squared
model_log_results <- cbind(summary(model_log)$coef,confint(model_log))
model_log_results
  
plot(model_log,1) # shows homoskedasticity, some improvement compared to without log, but sill non-linear
plot(model_log,2) # ok - residualas are nomral
plot(model_log,3) # ok - constant standardised residuals
# Doesn't look like log is much of an improvement on non-logged

# Log does certainly look slightly better!


# Plot

combined_england$r_pred <- exp(predict(model_log))

ggplot(data=combined_england, aes(x=date))+
  geom_line(aes(y=r), col="purple")+
  geom_line(aes(y=r_pred), col="red", linetype="dashed")+
  theme_bw() +
  theme(legend.position = c(0.5,0.95),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size=22),
        plot.caption.position="panel",
        plot.caption = element_text(hjust = 0.5))

# Can see in the model fit that it is missing things like holiday effects


# Could look separately at each lockdown? Then combined again. Real limitation with lockdown 1 -> missing the first half
# Looks to only fit well Lockdown 2?
