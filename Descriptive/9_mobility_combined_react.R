# Descriptive for REACT and mobility combined

# Date started: 15th July 2022


library(tidyverse)
library(lubridate)
library(boot)

# England data
google_england_long <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")

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

google_england <- google_england_long %>% filter(region=="ENGLAND", type_mobility %in% cols) #%>% filter(date<=lockdown_3_end)

start_date <- as.Date("2020-02-18","%Y-%m-%d")
end_date <- lockdown_3_end


# Mobility plot -----------------------------------------------------------



plot_baseline <- ggplot(data=google_england, aes(x=date,y=mobility,color=type_mobility)) +
  geom_line() +
  xlab("") +
  ylab ("Mobility compared to baseline (%)") +
  ggtitle("") +
  labs(color = "") + # This means that the title for the legend is blank
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "3 months"),
               minor_breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "1 months"),
               date_labels="%b %Y",limits=c(start_date, end_date)) + #
  scale_color_hue(labels = labels)+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"))+
  geom_hline(yintercept=0, linetype="dashed", size=0.5, col="grey")

plot_baseline


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








# Reproduction (R) plot ---------------------------------------------------

react_reprod <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

R_plot <- ggplot(data=react_reprod) +
          geom_line(aes(x=d_comb,y=r), col="purple")+
          xlim(start_date,end_date)+
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5),legend.position = "none",
                panel.border=element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text.x=element_blank())+
          geom_hline(yintercept=1, linetype="dashed", size=0.5, col="grey")+
          labs(y="Reproduction R(t)",x="")

R_plot

# Prevalence plot ---------------------------------------------------------

# Currently don't have the England level -> should download this!!!

# Use London instead for the moment

prev_london <- readRDS("~/Desktop/Masters/Project/Data/REACT_prevalence/b_splines/reg_LN_6.rds")

prev_plot <- ggplot(data=prev_london) +
            geom_line(aes(x=d_comb,y=inv.logit(p)*100), col="red")+
            xlim(start_date,end_date)+
            theme_light()+
            ylim(0,3)+
            theme(plot.title = element_text(hjust = 0.5),legend.position = "none",
                    panel.border=element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.x=element_blank())+
            labs(y="Prevalence (%)",x="")

prev_plot

# Multi-plot --------------------------------------------------------------

plot_grid(R_plot,
          prev_plot,
          plot_baseline,
          nrow=3, align="v", rel_heights=c(1,1,2))

