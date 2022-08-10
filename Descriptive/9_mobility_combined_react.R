# Descriptive for REACT and mobility combined

# Date started: 15th July 2022


library(tidyverse)
library(lubridate)
library(boot)
library(cowplot)

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


google_england <- google_england %>%
                  mutate(baseline = ifelse((date>=start_date) & (date<=end_date),0,NA))

plot_baseline <- ggplot(data=google_england, aes(x=date)) +
  geom_line(aes(y=mobility,color=type_mobility)) +
  xlab("") +
  ylab ("Mobility compared to baseline (%)") +
  ggtitle("") +
  labs(color = "") + # This means that the title for the legend is blank
  # scale_x_date(breaks = function(x) seq.Date(from = min(x), 
  #                                            to = max(x), 
  #                                            by = "3 months"),
  #              minor_breaks = function(x) seq.Date(from = min(x), 
  #                                                  to = max(x), 
  #                                                  by = "1 months"),
  #              date_labels="%b %Y",limits=c(start_date, end_date)) + #
  scale_color_hue(labels = labels)+
  theme_light() +
  xlim(start_date,end_date)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  geom_line(aes(y=baseline), linetype="dashed")
  #geom_hline(yintercept=0, linetype="dashed", size=0.5, col="grey")

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
  annotate("text",x=lockdown_1_start+days(30), y=132, label="Lockdown 1", hjust=0, color="gray23") +
  annotate("text",x=lockdown_2_start+days(12), y=132, label="Lockdown 2",  hjust=0.5, color="gray23") +
  annotate("text",x=lockdown_3_start+days(40), y=132, label="Lockdown 3",  hjust=0, color="gray23") 
plot_baseline_lockdowns

# Limit until the end of lockdown 3

plot_baseline_lockdowns


# Looking at how much of the drop/rise falls outside of the 3 day period

plot_baseline_lockdowns_3days <- plot_baseline_lockdowns +
                                  geom_vline(xintercept = lockdown_1_start-days(3))+
                                  geom_vline(xintercept = lockdown_1_end+days(3)) +
                                  geom_vline(xintercept = lockdown_2_start-days(3))+
                                  geom_vline(xintercept = lockdown_2_end+days(3)) +
                                  geom_vline(xintercept = lockdown_3_start-days(3))+
                                  geom_vline(xintercept = lockdown_3_end+days(3)) 
                    

plot_baseline_lockdowns_3days




# Reproduction (R) plot ---------------------------------------------------

react_reprod <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

react_reprod_start_date <- as.Date("2020-05-15","%Y-%m-%d")


# Adding positive and negative versions of the variables to indicate when it is >=1 or not
react_reprod <- react_reprod %>%
                mutate(r_pos = ifelse(r>=1,r,NA),
                       r_neg = ifelse(r<1,r,NA),
                       r1_line = ifelse(d_comb>=react_reprod_start_date,1,NA))

#Dates don't line up with d_comb so trying my own date metric
react_reprod$date<-NA

for (i in 1:nrow(react_reprod)){
  react_reprod$date[i] <-as.Date("2020-05-01","%Y-%m-%d")+days(i-1)
}

react_reprod$date <- as.Date(react_reprod$date,"%Y-%m-%d",origin = "1970-01-01")


R_plot <- ggplot(data=react_reprod, aes(x=date)) +
          geom_line(aes(y=r_pos), col="red")+
          geom_line(aes(y=r_neg), col="green")+
          geom_line(aes(y=r1_line), col="black",linetype="dashed")+
          geom_ribbon(aes(ymin  =lb_2.5, ymax = ub_97.5), alpha = 0.1)+
          geom_ribbon(aes(ymin  =lb_25, ymax = ub_75), alpha = 0.2)+
          xlim(start_date,end_date)+
          ylim(0.35,2)+
          # scale_x_date(breaks = function(x) seq.Date(from = min(x), 
          #                                    to = max(x), 
          #                                    by = "3 months"),
          #      minor_breaks = function(x) seq.Date(from = min(x), 
          #                                          to = max(x), 
          #                                          by = "1 months"),
          #      date_labels="%b %Y",limits=c(start_date, end_date)) +
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5),legend.position = "none",
                panel.border=element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text.x=element_blank(),
                plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
          labs(y="Reproduction number",x="")

R_plot

View(react_reprod)

react_reprod$r[which.max(react_reprod$r[react_reprod$d_comb<lockdown_3_end])]
summary(react_reprod$r[react_reprod$d_comb<lockdown_3_end])

# Adding REACT rounds

# Dates
R1_start <- as.Date("2020-05-01","%Y-%m-%d")
R1_end <- as.Date("2020-06-01","%Y-%m-%d")
R2_start <- as.Date("2020-06-19","%Y-%m-%d")
R2_end <- as.Date("2020-07-07","%Y-%m-%d")
R3_start <- as.Date("2020-07-24","%Y-%m-%d")
R3_end <- as.Date("2020-08-11","%Y-%m-%d")
R4_start <- as.Date("2020-08-20","%Y-%m-%d")
R4_end <- as.Date("2020-09-08","%Y-%m-%d")
R5_start <- as.Date("2020-09-18","%Y-%m-%d")
R5_end <- as.Date("2020-10-05","%Y-%m-%d")
R6_start <- as.Date("2020-10-16","%Y-%m-%d")
R6_end <- as.Date("2020-11-02","%Y-%m-%d")
R7_start <- as.Date("2020-11-13","%Y-%m-%d")
R7_end <- as.Date("2020-12-03","%Y-%m-%d")
R8_start <- as.Date("2021-01-06","%Y-%m-%d")
R8_end <- as.Date("2021-01-22","%Y-%m-%d")
R9_start <- as.Date("2021-02-04","%Y-%m-%d")
R9_end <- as.Date("2021-02-23","%Y-%m-%d")
R10_start <- as.Date("2021-03-11","%Y-%m-%d")
R10_end <- as.Date("2021-03-30","%Y-%m-%d")
R11_start <- as.Date("2021-04-15","%Y-%m-%d")
R11_end <- as.Date("2021-05-03","%Y-%m-%d")

# start_dates <- dput(paste0("R",seq(1,11),"_start",sep=""))
# end_dates <- dput(paste0("R",seq(1,11),"_end",sep=""))
round_labels <- dput(paste0("R",seq(1,11),sep=""))

start_dates <- c(R1_start, R2_start,  R3_start, R4_start,  R5_start,  R6_start, R7_start, R8_start, R9_start, R10_start) #R11_start

end_dates <- c(R1_end, R2_end,  R3_end, R4_end,  R5_end,  R6_end, R7_end, R8_end, R9_end, R10_end) #R11_end

#Plot

R_plot_rounds <- R_plot

for (i in 1:length(start_dates)){
  start <- start_dates[i]
  end <- end_dates[i]
  mid <- start + days(round(as.numeric(difftime(end, start))/2))
  date_label <- round_labels[i]
  R_plot_rounds <- R_plot_rounds +
                annotate("segment", x=start, xend=end,y=1.9,yend=1.9) +
                annotate("segment",x=start,xend=start, y=1.85,yend=1.9)+
                annotate("segment",x=end,xend=end, y=1.85,yend=1.9)+
                annotate("text",x=mid,y=2,label=date_label, hjust=0.5)
  
}
R_plot_rounds 

# Prevalence plot ---------------------------------------------------------

# Currently don't have the England level -> should download this!!!

# Use London instead for the moment

prev_eng <- readRDS("~/Desktop/Masters/Project/Data/REACT_prevalence/england_national_prevalence.RDS")

prev_london <- readRDS("~/Desktop/Masters/Project/Data/REACT_prevalence/b_splines/reg_LN_6.rds")

prev_plot <- ggplot(data=prev_eng, aes(x=d_comb)) +
            geom_line(aes(y=inv.logit(p)*100), col="purple")+
            geom_ribbon(aes(ymin = inv.logit(lb_2.5)*100, ymax = inv.logit(ub_97.5)*100), alpha = 0.1)+
            geom_ribbon(aes(ymin = inv.logit(lb_25)*100, ymax = inv.logit(ub_75)*100), alpha = 0.2)+
            xlim(start_date,end_date)+
            # scale_x_date(breaks = function(x) seq.Date(from = min(x), 
            #                                  to = max(x), 
            #                                  by = "3 months"),
            #    minor_breaks = function(x) seq.Date(from = min(x), 
            #                                        to = max(x), 
            #                                        by = "1 months"),
            #    date_labels="%b %Y",limits=c(start_date, end_date)) +
            theme_light()+
            ylim(0,3)+
            theme(plot.title = element_text(hjust = 0.5),
                    legend.position = "none",
                    panel.border=element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.x=element_blank(),
                  plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
            labs(y="Prevalence (%)",x="")

prev_plot

prev_eng$p_inv <- inv.logit(prev_eng$p)*100

prev_eng$d_comb[which.max(prev_eng$p_inv[prev_eng$d_comb<lockdown_3_end])]
max(prev_eng$p_inv[prev_eng$d_comb<lockdown_3_end])


# Multi-plot --------------------------------------------------------------

plot_grid(R_plot_rounds,
          prev_plot,
          plot_baseline_lockdowns,
          nrow=3, align="v", rel_heights=c(1,1,2))

# Note the function used to generate the date tick marks DOES NOT WORK - REPLACE IN OTHER PLOT TOO


setwd("~/Desktop/Masters/Project/Analysis/Descriptive/Ouputs")
ggsave(file="multiplot_england_lockdowns.png", plot_grid(R_plot_rounds,
                                               prev_plot,
                                               plot_baseline_lockdowns,
                                               nrow=3, align="v", rel_heights=c(1,1,2)), 
                                               width=37, height=37, units="cm") 
