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

# Creating a new var if R>=1 or R<1
# combined_england$r_range <- ifelse(combined_england$r>=1,">=1","<1")
# combined_england$r_range <- as.factor(combined_england$r_range) 
# str(combined_england$r_range)



# Lockdown any variable
# 
# combined_england$lockdown_any <- ifelse(combined_england$lockdown=="No_lockdown",0,1)


 # Playing around with regression ------------------------------------------

# Things to think about: should I be lagging in some way?
# Should I be including date in some way?

model <- lm(r ~  variant + lockdown_any + workplaces_av + residential_av + retail_recreation_av + grocery_pharmacy_av + transit_stations_av + cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england)
summary(model)$r.squared
summary(model)$adj.r.squared
model_results <- cbind(summary(model)$coef,confint(model))
model_results

tbl_regression(model)

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
  geom_line(aes(y=r), col="blue")+
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


## Lockdown 1 only ---------------------------------------------------------

combined_england_lockdown1 <- combined_england %>% filter(lockdown=="Lockdown1")

model_log_l1 <-  lm(log(r) ~ workplaces_av + cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england_lockdown1)
summary(model_log_l1)$r.squared
results_model_log_l1<- cbind(summary(model_log_l1)$coef,confint(model_log_l1))
restuls_model_log_l1

combined_england_lockdown1$r_pred_l1 <- exp(predict(model_log_l1))

ggplot(data=combined_england_lockdown1, aes(x=date))+
  geom_line(aes(y=r), col="blue")+
  geom_line(aes(y=r_pred_l1), col="red", linetype="dashed")+
  theme_bw() +
  theme(legend.position = c(0.5,0.95),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size=22),
        plot.caption.position="panel",
        plot.caption = element_text(hjust = 0.5))


## Lockdown 2 only ---------------------------------------------------------

combined_england_lockdown2 <- combined_england %>% filter(lockdown=="Lockdown2")

model_log_l2 <-  lm(log(r) ~ workplaces_av + cumVaccinationFirstDoseUptakeByPublishDatePercentage, data=combined_england_lockdown2)
summary(model_log_l2)$r.squared
results_model_log_l2<- cbind(summary(model_log_l2)$coef,confint(model_log_l2))
results_model_log_l2

combined_england_lockdown2$r_pred_l2 <- exp(predict(model_log_l2))

ggplot(data=combined_england_lockdown2, aes(x=date))+
  geom_line(aes(y=r), col="blue")+
  geom_line(aes(y=r_pred_l2), col="red", linetype="dashed")+
  theme_bw() +
  theme(legend.position = c(0.5,0.95),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size=22),
        plot.caption.position="panel",
        plot.caption = element_text(hjust = 0.5))

## Lockdown 3 only ---------------------------------------------------------

combined_england_lockdown3 <- combined_england %>% filter(lockdown=="Lockdown3")

model_log_l3 <-  lm(log(r) ~ retail_recreation_av + cumVaccinationFirstDoseUptakeByPublishDatePercentage  + cumVaccinationSecondDoseUptakeByPublishDatePercentage, data=combined_england_lockdown3)
summary(model_log_l3)$r.squared
results_model_log_l3<- cbind(summary(model_log_l3)$coef,confint(model_log_l3))
results_model_log_l3

combined_england_lockdown3$r_pred_l3 <- exp(predict(model_log_l3))

ggplot(data=combined_england_lockdown3, aes(x=date))+
  geom_line(aes(y=r), col="blue")+
  geom_line(aes(y=r_pred_l3), col="red", linetype="dashed")+
  theme_bw() +
  theme(legend.position = c(0.5,0.95),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size=22),
        plot.caption.position="panel",
        plot.caption = element_text(hjust = 0.5))





# Try logistic regression -------------------------------------------------

combined_england



model_logistic <- glm(r_range ~  cumVaccinationFirstDoseUptakeByPublishDatePercentage + workplaces + lockdown_any, data=combined_england, family="binomial")

results_model_logistic<- cbind(summary(model_logistic)$coef,confint(model_logistic))

tbl_regression(model_logistic, exponentiate = TRUE)



## Lockdown 1 only ---------------------------------------------------------


combined_england_lockdown1

model_logistic_l1 <- glm(r_range ~  cumVaccinationFirstDoseUptakeByPublishDatePercentage + workplaces_av, data=combined_england_lockdown1, family="binomial")
tbl_regression(model_logistic_l1, exponentiate = TRUE)



## Lockdown 2 only ---------------------------------------------------------

model_logistic_l2 <- glm(r_range ~  cumVaccinationFirstDoseUptakeByPublishDatePercentage + workplaces, data=combined_england_lockdown2, family="binomial")
tbl_regression(model_logistic_l2, exponentiate = TRUE)


## Lockdown 3 only ---------------------------------------------------------

model_logistic_l3 <- glm(r_range ~  cumVaccinationFirstDoseUptakeByPublishDatePercentage + cumVaccinationSecondDoseUptakeByPublishDatePercentage+ workplaces, data=combined_england_lockdown3, family="binomial")
tbl_regression(model_logistic_l3, exponentiate = TRUE)

# This doesn't work??








# Regional ----------------------------------------------------------------

combined_regional <- readRDS("~/Desktop/Masters/Project/Analysis/R_regression/Data_outputs/combined_regional.rds")

regions_list <- unique(combined_regional$region)
regions_list

regional_summary <- NA

for (i in 1:length(regions_list)){
  region_interest <- regions_list[i]
  data <- combined_regional %>% filter(region == region_interest)
  model <- lm(r ~ workplaces + lockdown_any, data = data)
  summary <- cbind(summary(model)$coef, confint(model))
  summary <- as.data.frame(summary)
  summary$region <- region_interest
  regional_summary <- rbind(regional_summary, summary)
}

regional_summary <- regional_summary[-1,]
regional_summary


rownames(regional_summary)

# Looking just at the workplaces coefficients
regional_summary_workplaces <- regional_summary %>% filter(grepl("workplaces", rownames(regional_summary)))
regional_summary_workplaces
# All significant except for North East and West Midlands

regional_summary_workplaces$region <- as.fac

ggplot(regional_summary_workplaces)+
  geom_bar(aes(y=Estimate))+
  facet_wrap(.~region)
