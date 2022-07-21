# Understanding the lag between mobility and reproduction number (R)

# Date started: 15th July 2022





# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(stringr)
library(dtw)
library(BBmisc)

min_max_normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



# Useful dates ------------------------------------------------------------

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

REACT_start <-as.Date("2020-05-01","%Y-%m-%d")


# Loading mobility tibble ---------------------------------------------------------

mobility_google <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")

# Only want the rolling averages
mobility_av <- mobility_google %>% filter(str_detect(type_mobility, 'av'))

# Defining a list of types of mobility
types_mobility <- c("retail_recreation_av", "grocery_pharmacy_av", "parks_av","transit_stations_av", "workplaces_av","residential_av")

mobility_av <- mobility_av %>% 
  mutate(type_mobility = 
           case_when(type_mobility=="retail_recreation_av"~"Retail & recreation" ,
                     type_mobility=="grocery_pharmacy_av"~ "Grocery & pharmacy",
                     type_mobility=="parks_av"~ "Parks",
                     type_mobility=="transit_stations_av"~ "Transit stations",
                     type_mobility=="workplaces_av"~ "Workplaces",
                     type_mobility=="residential_av"~ "Residential"))


types_mobility <- dput(unique(mobility_av$type_mobility))    
types_mobility

# Load REACT reproduction number data -------------------------------------

react_reprod <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

REACT_reprod_start <- as.Date("2020-05-15","%Y-%m-%d")




# Functions
setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results")
# DTW
dtw_full <- function(query=mobility_av, reference=react_reprod, 
                     mobility_place="Workplaces", start_date, end_date, region="ENGLAND"){
  # Filtering data
  mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
  reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
  # DTW function
  dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
  print("Mean distance between indices is:")
  print(mean(dtw_lag$index1 - dtw_lag$index2))
  # DTW path plot
  dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
  colnames(dtw_data)<- c("mob_index","r_index")
  write.csv(dtw_data,"dtw_indices.csv" )
  dtw_plot <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
                      geom_line(col="blue") + 
                      theme_bw() +
                      labs(x="Mobility series index",
                           y="R(t) series index")+
                      #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
                      geom_abline(slope=1, intercept=0, col="red", linetype="dashed")
  plot_name <- paste0("plot_dtw_",region,"_",start_date,"_",end_date,"_",mobility_place,".png")
  return(dtw_plot)
  ggsave(plot_name, dtw_plot)
  }

# Test
# dtw_full(query=mobility_av, reference=react_reprod, 
#          mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_3_end, region="ENGLAND")


# CCF

ccf_full <- function(query=mobility_av, reference=react_reprod, 
                     mobility_place="Workplaces", start_date, end_date, region="ENGLAND"){
  mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
  reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
  lag_max=100
  ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
  ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
  colnames(ccf_data)[1:2]<-c("ccf","lag")
  print("Max lag is at:")
  print(ccf_data$lag[which.max(ccf_data$ccf)])
  
  n <- nrow(mob)
  #k <- seq(1,lag_max,1)
  
  ccf_data <- ccf_data %>% 
    mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
           lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
  ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")
  write.csv(ccf_data,ccf_data_name )
  
  ccf_plot <- ccf_data %>% filter(lag>=-100) %>%
    ggplot(aes(x=lag))+
    geom_area(aes(y=upper_ci), fill="light grey")+
    geom_area(aes(y=lower_ci), fill="light grey") +
    geom_line(aes(y=ccf), color="royal blue") +
    geom_hline(yintercept=0, color="dark grey") +
    annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
             y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
    annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
             y=max(ccf_data$ccf)+0.02, col="Red") +
    #ylim(-0.2,0.4) +
    labs(x="Lag (days)", y="CCF")+
    #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
    ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region))+
    theme_light()
  ccf_plot_name <-  paste0("plot_ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".png")
  ggsave(ccf_plot_name, ccf_plot)
}

# Test
# ccf_full(query=mobility_av, reference=react_reprod, 
#          mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_3_end, region="ENGLAND")
# 

# Plotting the time series of interest

plot_period<- function(query=mobility_av, reference=react_reprod, 
            mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_3_end, region="ENGLAND"){
  mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
  reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
  
  mob <- mob[c("date" ,"mobility" , "type_mobility")]
  colnames(mob) <- c("date_series" , "series", "type")
  mob$series <- normalize(mob$series)
  write.csv(mob,"mob_series.csv")
  reprod <- reprod[c("r","d_comb")]
  reprod$type <- "R(t)"
  colnames(reprod) <- c("series","date_series","type")
  reprod$series <- normalize(reprod$series)
  write.csv(reprod,"r_series.csv")
  data <- rbind(mob, reprod)
  plot_series <- ggplot(data)+
    geom_line(aes(x=date_series,y=series, col=type))+
    theme_light()+
    labs(y="Normalised time-series", x="")+
    ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region))+
    theme(plot.title = element_text(hjust = 0.5),legend.position = "right",
          panel.border=element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank())
  plot_name <- paste0("plot_series_",region,"_",start_date,"_",end_date,"_",mobility_place,".png")
  ggsave(plot_name, plot_series)
}

# Test
# plot_period(query=mobility_av, reference=react_reprod, 
#          mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_3_end, region="ENGLAND")


# test in full

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results")
lag_combined <- function(query=mobility_av, reference=react_reprod, 
                         mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_3_end, region="ENGLAND"){
  
  dtw_full(query,reference, mobility_place, start_date, end_date, region)
  ccf_full(query,reference, mobility_place, start_date, end_date, region)
  plot_period(query,reference, mobility_place, start_date, end_date, region)
}




# Whole period,  England, workplaces --------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results/Whole_period")
lag_combined(query=mobility_av, reference=react_reprod, 
                      mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_3_end, region="ENGLAND")



## Refining plots ----------------------------------------------------------
query=mobility_av
reference=react_reprod 
mobility_place="Workplaces"
start_date = REACT_reprod_start
end_date = lockdown_3_end
region="ENGLAND"



  mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
  reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
  # DTW function
  dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
  print("Mean distance between indices is:")
  print(mean(dtw_lag$index1 - dtw_lag$index2))
  # DTW path plot
  dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
  colnames(dtw_data)<- c("mob_index","r_index")

  
  dtw_plot_whole_period <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
    geom_line(col="blue") + 
    theme_bw() +
    labs(x="Mobility series index",
         y="R(t) series index")+
    #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
    geom_abline(slope=1, intercept=0, col="red", linetype="dashed")
  
  dtw_plot_whole_period



# Lockdown 1,  England, workplaces ----------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results/Lockdown_1")
lag_combined(query=mobility_av, reference=react_reprod, 
             mobility_place="Workplaces", start_date = REACT_reprod_start, end_date = lockdown_1_end, region="ENGLAND")



# Refining plots ----------------------------------------------------------

query=mobility_av
reference=react_reprod, 
mobility_place="Workplaces"
start_date = REACT_reprod_start
end_date = lockdown_1_end
region="ENGLAND"

# DTW
mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
# DTW function
dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
# DTW path plot
dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
colnames(dtw_data)<- c("mob_index","r_index")


dtw_plot_lockdown_1 <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
  geom_line(col="blue") + 
  theme_bw() +
  labs(x="Mobility series index",
       y="R(t) series index")+
  #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
  geom_abline(slope=1, intercept=0, col="red", linetype="dashed")+
  theme(axis.line = element_line(colour = "black"),panel.border = element_blank())

dtw_plot_lockdown_1

# CCF

lag_max=100
ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])

n <- nrow(mob)
#k <- seq(1,lag_max,1)

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")


ccf_plot_lockdown_1 <- ccf_data %>% filter(lag>=-24,lag<=24) %>% 
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  #annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
  #         y=max(ccf_data$ccf)+0.03, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-1,1))

ccf_plot_lockdown_1

# Lockdown 2,  England, workplaces ----------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results/Lockdown_2")
lag_combined(query=mobility_av, reference=react_reprod, 
             mobility_place="Workplaces", start_date = lockdown_2_start, end_date = lockdown_2_end, region="ENGLAND")


# Refining plots ----------------------------------------------------------

query=mobility_av
reference=react_reprod, 
mobility_place="Workplaces"
start_date = lockdown_2_start
end_date = lockdown_2_end
region="ENGLAND"

mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
# DTW function
dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
# DTW path plot
dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
colnames(dtw_data)<- c("mob_index","r_index")


dtw_plot_lockdown_2 <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
  geom_line(col="blue") + 
  theme_bw() +
  labs(x="Mobility series index",
       y="R(t) series index")+
  #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
  geom_abline(slope=1, intercept=0, col="red", linetype="dashed")+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())

dtw_plot_lockdown_2

# CCF

lag_max=100
ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])

n <- nrow(mob)
#k <- seq(1,lag_max,1)

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")


ccf_plot_lockdown_2 <- ccf_data %>% filter(lag>=-24,lag<=24) %>%
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  #annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
  #         y=max(ccf_data$ccf)+0.03, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-1,1))

ccf_plot_lockdown_2


# Lockdown 3,  England, workplaces ----------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results/Lockdown_3")
lag_combined(query=mobility_av, reference=react_reprod, 
             mobility_place="Workplaces", start_date = lockdown_3_start, end_date = lockdown_3_end, region="ENGLAND")



# Refining plots ----------------------------------------------------------
query=mobility_av
reference=react_reprod, 
mobility_place="Workplaces"
start_date = lockdown_3_start
end_date = lockdown_3_end
region="ENGLAND"

mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
# DTW function
dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
# DTW path plot
dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
colnames(dtw_data)<- c("mob_index","r_index")


dtw_plot_lockdown_3<-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
  geom_line(col="blue") + 
  theme_bw() +
  labs(x="Mobility series index",
       y="R(t) series index")+
  #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
  geom_abline(slope=1, intercept=0, col="red", linetype="dashed")+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())
  

dtw_plot_lockdown_3

# CCF

lag_max=100
ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])

n <- nrow(mob)
#k <- seq(1,lag_max,1)

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")


ccf_plot_lockdown_3 <- ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0,ymax=max(ccf_data$ccf),  col="red", linetype="dashed")+
  #annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
  #         y=max(ccf_data$ccf)+0.03, col="Red") +
  #ylim(-0.2,0.4) +
  annotate("pointrange", x=-37,
           y=ccf_data$ccf[ccf_data$lag==-37],ymin=ccf_data$ccf[ccf_data$lag==-37],ymax=0,  col="red", linetype="dashed")+
  labs(x="Lag (days)", y="CCF")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_x_continuous(limits=c(-50,50))+
  scale_y_continuous(limits=c(-1,1))

ccf_plot_lockdown_3


# Dividing lockdown 3 into 2 ----------------------------------------------


## Prior to 16th Feb -------------------------------------------------------

query=mobility_av
reference=react_reprod
mobility_place="Workplaces"
region="ENGLAND"
start_date = lockdown_3_start
end_date = lockdown_3_start + days(41)
end_date

mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
lag_max=100
ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])

n <- nrow(mob)
#k <- seq(1,lag_max,1)

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")
write.csv(ccf_data,ccf_data_name )

ccf_plot <- ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region))+
  theme_light()

ccf_plot

## After 16th Feb -------------------------------------------------------


query=mobility_av
reference=react_reprod
mobility_place="Workplaces"
region="ENGLAND"
start_date = lockdown_3_start + days(41)
end_date = lockdown_3_end
end_date

mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
lag_max=100
ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])

n <- nrow(mob)
#k <- seq(1,lag_max,1)

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")
write.csv(ccf_data,ccf_data_name )

ccf_plot <- ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region))+
  theme_light()

ccf_plot

# Lockdown 2 to 3, England, workplaces ----------------------------------------

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results/Lockdown_2_3")
lag_combined(query=mobility_av, reference=react_reprod, 
             mobility_place="Workplaces", start_date = lockdown_2_start, end_date = lockdown_3_end, region="ENGLAND")


# Refining plots ----------------------------------------------------------

query=mobility_av
reference=react_reprod 
mobility_place="Workplaces"
start_date = lockdown_2_start
end_date = lockdown_3_end
region="ENGLAND"

mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
# DTW function
dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
# DTW path plot
dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
colnames(dtw_data)<- c("mob_index","r_index")


dtw_plot_lockdown_2_3<-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
  geom_line(col="blue") + 
  theme_bw() +
  labs(x="Mobility series index",
       y="R(t) series index")+
  #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
  geom_abline(slope=1, intercept=0, col="red", linetype="dashed")+
  theme(axis.line = element_line(colour = "black"))

dtw_plot_lockdown_2_3

#CCF
 
lag_max=100
ccf <- ccf(reprod$r,mob$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])

n <- nrow(mob)
#k <- seq(1,lag_max,1)

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))
ccf_data_name <- paste0("ccf_",region,"_",start_date,"_",end_date,"_",mobility_place,".csv")


ccf_plot_lockdown_2_3 <- ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  geom_area(aes(y=upper_ci), fill="light grey")+
  geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0,ymax=max(ccf_data$ccf),  col="red", linetype="dashed")+
  #annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
  #         y=max(ccf_data$ccf)+0.03, col="Red") +
  #ylim(-0.2,0.4) +
  annotate("pointrange", x=-24,
           y=ccf_data$ccf[ccf_data$lag==-24],ymin=ccf_data$ccf[ccf_data$lag==-24],ymax=0,  col="red", linetype="dashed")+
  labs(x="Lag (days)", y="CCF")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_x_continuous(limits=c(-50,50))+
  scale_y_continuous(limits=c(-1,1))

ccf_plot_lockdown_2_3



# Combining plots ---------------------------------------------------------


plot_grid(dtw_plot_lockdown_1,
          dtw_plot_lockdown_2,
          dtw_plot_lockdown_3,
          ccf_plot_lockdown_1,
          ccf_plot_lockdown_2,
          ccf_plot_lockdown_3,
          nrow=2, align="hv")

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/dtw_results")
ggsave(file="ccf_dtw_multiplot.png", plot_grid(dtw_plot_lockdown_1,
                                                              dtw_plot_lockdown_2,
                                                              dtw_plot_lockdown_3,
                                                              ccf_plot_lockdown_1,
                                                              ccf_plot_lockdown_2,
                                                              ccf_plot_lockdown_3,
                                                              nrow=2, align="hv"), width=265, height=154, units="mm") 




# Looking at residential and DTW -----------------------------------------------
 # Exploring as not sure what to expect as this is an inverse relationship, so shapes would be very different...

query=mobility_av
reference=react_reprod
mobility_place="Residential"
start_date=lockdown_3_start
end_date=lockdown_3_end
region="ENGLAND"

  mob <- query %>% filter(date>=start_date, date<=end_date, type_mobility == mobility_place, region=="ENGLAND")
  reprod <- reference %>% filter(d_comb>=start_date, d_comb <= end_date)
  # DTW function
  dtw_lag <- dtw(normalize(mob$mobility), normalize(reprod$r))
  print("Mean distance between indices is:")
  print(mean(dtw_lag$index1 - dtw_lag$index2))
  # DTW path plot
  dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
  colnames(dtw_data)<- c("mob_index","r_index")

  dtw_plot <-   ggplot(data=dtw_data, aes(x=mob_index, y=r_index))+
    geom_line(col="blue") + 
    theme_bw() +
    labs(x="Mobility series index",
         y="R(t) series index")+
    #ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region)) +
    geom_abline(slope=1, intercept=0, col="red", linetype="dashed")
  dtw_plot

  
  mob <- mob[c("date" ,"mobility" , "type_mobility")]
  colnames(mob) <- c("date_series" , "series", "type")
  mob$series <- normalize(mob$series)
  
  reprod <- reprod[c("r","d_comb")]
  reprod$type <- "R(t)"
  colnames(reprod) <- c("series","date_series","type")
  reprod$series <- normalize(reprod$series)
  
  data <- rbind(mob, reprod)
  plot_series <- ggplot(data)+
    geom_line(aes(x=date_series,y=series, col=type))+
    theme_light()+
    labs(y="Normalised time-series", x="")+
    ggtitle(paste0("Mobility: ", mobility_place,", Start date: ", start_date, ", End date:", end_date, ", Region: " , region))+
    theme(plot.title = element_text(hjust = 0.5),legend.position = "right",
          panel.border=element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank())
  plot_series
