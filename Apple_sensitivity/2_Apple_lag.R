# Repeated lag analysis with Apple data

# Date started: 18 August 2022



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

mobility_apple <- readRDS("~/Desktop/Masters/Project/Analysis/Apple_sensitivity/Outputs/Data/apple_england.rds")

# Only want the rolling averages
mobility_av <- mobility_apple %>% select(c(date,driving_av, transit_av,walking_av))

# Defining a list of types of mobility
types_mobility <- c("driving_av","transit_av","walking_av")


# Load REACT reproduction number data -------------------------------------

react_reprod <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

REACT_reprod_start <- as.Date("2020-05-15","%Y-%m-%d")

str(react_reprod)


# CCF ---------------------------------------------------------------------




## CCF with driving --------------------------------------------------------

### Lockdown 1 --------------------------------------------------------------

react_reprod_lockdown1 <- react_reprod %>% filter(d_comb>=REACT_reprod_start, d_comb<=lockdown_1_end)
mobility_av_lockdown1 <- mobility_av %>%  filter(date>=REACT_reprod_start, date<=lockdown_1_end)


lag_max=100
ccf <- ccf(mobility_av_lockdown1$driving_av,react_reprod_lockdown1$r, na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown1)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  theme_light()

ccf_plot


### Lockdown 2 --------------------------------------------------------------

react_reprod_lockdown2 <- react_reprod %>% filter(d_comb>=lockdown_2_start, d_comb<=lockdown_2_end)
mobility_av_lockdown2 <- mobility_av %>%  filter(date>=lockdown_2_start, date<=lockdown_2_end)


lag_max=100
ccf <- ccf(mobility_av_lockdown2$driving_av,react_reprod_lockdown2$r, na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown2)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  theme_light()

ccf_plot


### Lockdown 3 --------------------------------------------------------------

react_reprod_lockdown3 <- react_reprod %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end)
mobility_av_lockdown3 <- mobility_av %>%  filter(date>=lockdown_3_start, date<=lockdown_3_end)


lag_max=100
ccf <- ccf(mobility_av_lockdown3$driving_av, react_reprod_lockdown3$r,na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown3)

ccf_data


ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  xlim(-50,50) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  theme_light()

ccf_plot

# Peaks

ccf_data$lag[which.max(abs(ccf_data$ccf[ccf_data$lag<0]))] # lag at -20

ccf_data$lag[which.max(abs(ccf_data$ccf[ccf_data$lag>0]))]


## CCF with transit --------------------------------------------------------

### Lockdown 1 --------------------------------------------------------------

react_reprod_lockdown1 <- react_reprod %>% filter(d_comb>=REACT_reprod_start, d_comb<=lockdown_1_end)
mobility_av_lockdown1 <- mobility_av %>%  filter(date>=REACT_reprod_start, date<=lockdown_1_end)


lag_max=100
ccf <- ccf(react_reprod_lockdown1$r, mobility_av_lockdown1$transit_av, na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown1)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  theme_light()

ccf_plot


### Lockdown 2 --------------------------------------------------------------

react_reprod_lockdown2 <- react_reprod %>% filter(d_comb>=lockdown_2_start, d_comb<=lockdown_2_end)
mobility_av_lockdown2 <- mobility_av %>%  filter(date>=lockdown_2_start, date<=lockdown_2_end)


lag_max=100
ccf <- ccf(react_reprod_lockdown2$r, mobility_av_lockdown2$transit_av, na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown2)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  theme_light()

ccf_plot


### Lockdown 3 --------------------------------------------------------------

react_reprod_lockdown3 <- react_reprod %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end)
mobility_av_lockdown3 <- mobility_av %>%  filter(date>=lockdown_3_start, date<=lockdown_3_end)


lag_max=100
ccf <- ccf( mobility_av_lockdown3$transit_av,react_reprod_lockdown3$r, na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown3)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  xlim(-50,50) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  theme_light()

ccf_plot


ccf_data$lag[which.max(abs(ccf_data$ccf[ccf_data$lag<0]))] 

## CCF with walking --------------------------------------------------------

### Lockdown 1 --------------------------------------------------------------

react_reprod_lockdown1 <- react_reprod %>% filter(d_comb>=REACT_reprod_start, d_comb<=lockdown_1_end)
mobility_av_lockdown1 <- mobility_av %>%  filter(date>=REACT_reprod_start, date<=lockdown_1_end)


lag_max=100
ccf <- ccf( mobility_av_lockdown1$walking_av, react_reprod_lockdown1$r,na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown1)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  theme_light()

ccf_plot


### Lockdown 2 --------------------------------------------------------------

react_reprod_lockdown2 <- react_reprod %>% filter(d_comb>=lockdown_2_start, d_comb<=lockdown_2_end)
mobility_av_lockdown2 <- mobility_av %>%  filter(date>=lockdown_2_start, date<=lockdown_2_end)


lag_max=100
ccf <- ccf(mobility_av_lockdown2$walking_av, react_reprod_lockdown2$r, na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown2)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  theme_light()

ccf_plot


### Lockdown 3 --------------------------------------------------------------

react_reprod_lockdown3 <- react_reprod %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end)
mobility_av_lockdown3 <- mobility_av %>%  filter(date>=lockdown_3_start, date<=lockdown_3_end)


lag_max=100
ccf <- ccf(mobility_av_lockdown3$walking_av,react_reprod_lockdown3$r,  na.action = na.pass, pl=FALSE, lag=100)
ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")
print("Max lag is at:")
print(ccf_data$lag[which.max(ccf_data$ccf)])
n <- nrow(mobility_av_lockdown3)

ccf_data

ccf_data <- ccf_data %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))


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
  xlim(-50,50) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  theme_light()

ccf_plot





# DTW ---------------------------------------------------------------------



# DTW with driving --------------------------------------------------------


## Lockdown 1 --------------------------------------------------------------


react_reprod_lockdown1 <- react_reprod %>% filter(d_comb>=REACT_reprod_start, d_comb<=lockdown_1_end)
mobility_av_lockdown1 <- mobility_av %>%  filter(date>=REACT_reprod_start, date<=lockdown_1_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown1$driving_av), normalize(react_reprod_lockdown1$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot

## Lockdown 2 --------------------------------------------------------------


react_reprod_lockdown2 <- react_reprod %>% filter(d_comb>=lockdown_2_start, d_comb<=lockdown_2_end)
mobility_av_lockdown2 <- mobility_av %>%  filter(date>=lockdown_2_start, date<=lockdown_2_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown2$driving_av), normalize(react_reprod_lockdown2$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot


## Lockdown 3 --------------------------------------------------------------

react_reprod_lockdown3 <- react_reprod %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end)
mobility_av_lockdown3 <- mobility_av %>%  filter(date>=lockdown_3_start, date<=lockdown_3_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown3$driving_av), normalize(react_reprod_lockdown3$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot



# DTW with transit --------------------------------------------------------


## Lockdown 1 --------------------------------------------------------------


react_reprod_lockdown1 <- react_reprod %>% filter(d_comb>=REACT_reprod_start, d_comb<=lockdown_1_end)
mobility_av_lockdown1 <- mobility_av %>%  filter(date>=REACT_reprod_start, date<=lockdown_1_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown1$transit_av), normalize(react_reprod_lockdown1$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot

## Lockdown 2 --------------------------------------------------------------


react_reprod_lockdown2 <- react_reprod %>% filter(d_comb>=lockdown_2_start, d_comb<=lockdown_2_end)
mobility_av_lockdown2 <- mobility_av %>%  filter(date>=lockdown_2_start, date<=lockdown_2_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown2$transit_av), normalize(react_reprod_lockdown2$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot


## Lockdown 3 --------------------------------------------------------------

react_reprod_lockdown3 <- react_reprod %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end)
mobility_av_lockdown3 <- mobility_av %>%  filter(date>=lockdown_3_start, date<=lockdown_3_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown3$transit_av), normalize(react_reprod_lockdown3$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot



# DTW with walking --------------------------------------------------------


## Lockdown 1 --------------------------------------------------------------


react_reprod_lockdown1 <- react_reprod %>% filter(d_comb>=REACT_reprod_start, d_comb<=lockdown_1_end)
mobility_av_lockdown1 <- mobility_av %>%  filter(date>=REACT_reprod_start, date<=lockdown_1_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown1$walking_av), normalize(react_reprod_lockdown1$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot

## Lockdown 2 --------------------------------------------------------------


react_reprod_lockdown2 <- react_reprod %>% filter(d_comb>=lockdown_2_start, d_comb<=lockdown_2_end)
mobility_av_lockdown2 <- mobility_av %>%  filter(date>=lockdown_2_start, date<=lockdown_2_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown2$walking_av), normalize(react_reprod_lockdown2$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot


## Lockdown 3 --------------------------------------------------------------

react_reprod_lockdown3 <- react_reprod %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end)
mobility_av_lockdown3 <- mobility_av %>%  filter(date>=lockdown_3_start, date<=lockdown_3_end)

dtw_lag <- dtw(normalize(mobility_av_lockdown3$walking_av), normalize(react_reprod_lockdown3$r))
print("Mean distance between indices is:")
print(mean(dtw_lag$index1 - dtw_lag$index2))
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
dtw_plot