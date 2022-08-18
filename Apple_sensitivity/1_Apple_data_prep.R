# Apple data preparation - sensitivity analysis

# Date started: 18th August 2022




# Setup -------------------------------------------------------------------

library(tidyverse)



# Data --------------------------------------------------------------------


apple_mobility <- read_csv("~/Desktop/Masters/Project/Data/Apple/apple_mobility_report.csv")


apple_england <- apple_mobility %>% filter(country=="United Kingdom", sub_region=="England", subregion_and_city=="England")




# Moving average ----------------------------------------------------------

colnames(apple_england)

apple_england <- apple_england %>%
      mutate(driving_av =  rollapply(driving, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center")) %>%
      mutate(transit_av =  rollapply(transit, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center")) %>%
      mutate(walking_av =  rollapply(walking, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center")) 



# Check date format --------------------------------------------------------------

str(apple_england$date)
apple_england$date <- as.Date(apple_england$date, "%d/%m/%Y")
str(apple_england$date)



# Plot to check -----------------------------------------------------------

ggplot(apple_england, aes(x=date))+
  geom_line(aes(y=driving_av), col="blue") +
  geom_line(aes(y=transit_av), col="green")+
  geom_line(aes(y=walking_av), col="red")





# Export the data ---------------------------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/Apple_sensitivity/Outputs/Data")
write_rds(apple_england,"apple_england.rds")





# Long version of data ----------------------------------------------------

library(reshape2)

apple_england_long <- melt(apple_england[,c("driving_av", "transit_av",
                            "walking_av", "date")], id.vars="date")


colnames(apple_england_long)<-c("date","type_mobility","mobility")

# Proper plot -------------------------------------------------------------

min(apple_england_long$date)
summary(apple_england_long$mobility[apple_england_long$date<lockdown_3_end])

start_date <- as.Date("2020-01-13","%Y-%m-%d")
end_date <- lockdown_3_end

labels=c("Driving","Transit",
         "Walking")


  
plot_baseline <- ggplot(data=apple_england_long, aes(x=date)) +
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
  ylim(-90,55)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.text=element_text(size=14),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  geom_hline(yintercept=0, linetype="dashed", size=0.5, col="black")

plot_baseline


# Add labels

plot_baseline_lockdowns <- plot_baseline + annotate("rect", xmin=lockdown_1_start, xmax = lockdown_1_end, ymin=-90, ymax=45, alpha=0.2) +
  annotate("rect", xmin=lockdown_2_start, xmax = lockdown_2_end, ymin=-90, ymax=45, alpha=0.2) +
  annotate("rect", xmin=lockdown_3_start, xmax = lockdown_3_end, ymin=-90, ymax=45, alpha=0.2) +
  annotate("text",x=lockdown_1_start+days(30), y=49, label="Lockdown 1", hjust=0, color="gray23") +
  annotate("text",x=lockdown_2_start+days(12), y=49, label="Lockdown 2",  hjust=0.5, color="gray23") +
  annotate("text",x=lockdown_3_start+days(40), y=49, label="Lockdown 3",  hjust=0, color="gray23") 

plot_baseline_lockdowns


setwd("~/Desktop/Masters/Project/Analysis/Apple_sensitivity/Outputs")
ggsave(file="apple_av_lockdowns.png",plot_baseline_lockdowns ,
       width=37, height=20, units="cm") 

