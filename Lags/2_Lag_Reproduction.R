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


# Load REACT reproduction number data -------------------------------------

react_reprod <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")

# Plot looks reasonable:
# ggplot(react_reprod, aes(d_comb,r)) +
#   geom_line()

# Note that the start of the REACT data is actually 15th May 2020
REACT_reprod_start <- REACT_start <-as.Date("2020-05-15","%Y-%m-%d")




# CCF  whole period - workplace---------------------------------------------------

lag_max=100

# Testing out with workplace mobility

mobility <- mobility_av %>% 
              filter(type_mobility == "workplaces_av") %>% 
              filter(date >= REACT_reprod_start, date <= lockdown_3_end) %>% 
              filter(region=="ENGLAND") #%>%
              #select(mobility)

reprod <- react_reprod %>% 
          filter(d_comb >= REACT_reprod_start, d_comb <= lockdown_3_end) #%>%
          #select(r)


# Plot min max normalised data so that on the same scale
ggplot()+
  geom_line(data=mobility, aes(x=date,y=min_max_normalise(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")+
  theme_light()

# Plot with z normalisation
ggplot()+
  geom_line(data=mobility, aes(x=date,y=normalize(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=normalize(r)), col="green")+
  theme_light()
 


# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

# Max at lag=0
ccf_data$lag[which.max(ccf_data$ccf)]

# Looking for the second peak
ccf_data$lag[which.max(ccf_data$ccf[ccf_data$lag>25])]

dtw_lag <- dtw(normalize(mobility$mobility), normalize(reprod$r))
dtw_lag$distance
#150.9157 (whatever that means)

dtw_lag$normalizedDistance
#0.220637

plot(dtw_lag$index1, dtw_lag$index2)
abline(a=0, b=1, col="red", lty="dashed")
mean(dtw_lag$index1 - dtw_lag$index2)
# 46.43842


# Min max normalise

dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r))
dtw_lag$distance

# Difference in the indices with min max normalisation is...

plot(dtw_lag$index1, dtw_lag$index2)
abline(a=0, b=1, col="red", lty="dashed")
mean(dtw_lag$index1 - dtw_lag$index2)
# 21.68988 now - odd that it's different -> start of the plot is very different -> have a clearer cross-over with min max normalisation than with z-norm




# CCF  lockdown 3 period - workplace---------------------------------------------------


# Testing out with workplace mobility

mobility <- mobility_av %>% 
  filter(type_mobility == "workplaces_av") %>% 
  filter(date >= lockdown_3_start, date <= lockdown_3_end) %>% 
  filter(region=="ENGLAND") #%>%
#select(mobility)

reprod <- react_reprod %>% 
  filter(d_comb >= lockdown_3_start, d_comb <= lockdown_3_end) #%>%
#select(r)


# Plot min max normalised data so that on the same scale
ggplot()+
  geom_line(data=mobility, aes(x=date,y=min_max_normalise(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green") +
  theme_light()



# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

max(ccf_data$ccf)
ccf_data$lag[which.max(ccf_data$ccf)] # 22 days

dtw_lag <- dtw(normalize(mobility$mobility), normalize(reprod$r))
dtw_lag$distance
# 12 (whatever that means)
dtw_lag$normalizedDistance
# 0.169

mean(dtw_lag$index1 - dtw_lag$index2)
# Average difference in index is only 1.72
plot(dtw_lag$index1, dtw_lag$index2)
abline(a=0, b=1, col="red", lty="dashed")



# Normalised distance is as a proportion of length of the period maybe?
#dtw_lag$normalizedDistance * difftime(lockdown_1_end,REACT_reprod_start) # but this could give 1.7666 days


# EXPERIMENTING TO UNDERSTAND DTW

dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r))
dtw_lag$distance
# 20.6 days # Only seems to work if I also min max normalise too, but reassuring that the same as CCF again


# Experimenting with open begin and end arguments
#dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r), open.begin=TRUE) # this doesn't work as it requires step patterns with N nromalisation
dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r), open.begin=TRUE, open.end=TRUE, step.pattern = asymmetric)
dtw_lag$distance
#8.387524

mean(dtw_lag$index1 - dtw_lag$index2)
# 2.433962

plot(dtw_lag$index1,dtw_lag$index2, ylim=c(0,106), xlim=c(0,106))
abline(a=0, b=1, col="red", lty="dashed")

ggplot()+
  geom_line(data=mobility,aes(x=date,y=normalize(mobility)),col="red")+
  geom_line(data=reprod, aes(x=d_comb,y=normalize(r)),col="blue")


# testing out a wave with an exact movement


# CCF  lockdown 2 period - workplace---------------------------------------------------


# Testing out with workplace mobility

mobility <- mobility_av %>% 
  filter(type_mobility == "workplaces_av") %>% 
  filter(date >= lockdown_2_start, date <= lockdown_2_end) %>% 
  filter(region=="ENGLAND") #%>%
#select(mobility)

reprod <- react_reprod %>% 
  filter(d_comb >= lockdown_2_start, d_comb <= lockdown_2_end) #%>%
#select(r)


# Plot min max normalised data so that on the same scale
ggplot()+
  geom_line(data=mobility, aes(x=date,y=min_max_normalise(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")+
  theme_light()



# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

max(ccf_data$ccf)
ccf_data$lag[which.max(ccf_data$ccf)] # 0 days

# Try CCF with min max normalisation

ccf <- ccf(min_max_normalise(reprod$r),min_max_normalise(mobility$mobility),lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

max(ccf_data$ccf)
ccf_data$lag[which.max(ccf_data$ccf)] # 0 days

# DTW


dtw_lag <- dtw(normalize(mobility$mobility), normalize(reprod$r))
dtw_lag$distance
# 9.61 (whatever that means)
dtw_lag$normalizedDistance
# 0.171717

mean(dtw_lag$index1-dtw_lag$index2)
# -2.47619

plot(dtw_lag$index1,dtw_lag$index2)
abline(a=0,b=1,col="red",lty="dashed")

dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r))
dtw_lag$distance
# 2.48 days # Only seems to work if I also min max normalise too, but reassuring that the same as CCF again


plot(dtw_lag$index1,dtw_lag$index2)
abline(a=0,b=1,col="red",lty="dashed")







# CCF  lockdown 1 period - workplace---------------------------------------------------


# Testing out with workplace mobility

mobility <- mobility_av %>% 
  filter(type_mobility == "workplaces_av") %>% 
  filter(date >= REACT_reprod_start, date <= lockdown_1_end) %>% 
  filter(region=="ENGLAND") #%>%
#select(mobility)

reprod <- react_reprod %>% 
  filter(d_comb >= REACT_reprod_start, d_comb <= lockdown_1_end) #%>%
#select(r)


# Plot min max normalised data so that on the same scale
ggplot()+
  geom_line(data=mobility, aes(x=date,y=min_max_normalise(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")



# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

max(ccf_data$ccf)
ccf_data$lag[which.max(ccf_data$ccf)] # 0 days

dtw_lag <- dtw(normalize(mobility$mobility), normalize(reprod$r))
dtw_lag$distance
# 88.87 (whatever that means)
dtw_lag$normalizedDistance
# 0.419
plot(dtw_lag$index1,dtw_lag$index2)

dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r))
dtw_lag$distance
# 2.48 days 










# CCF  between lockdown 1 and 2------------------------------------------------


# Testing out with workplace mobility

mobility <- mobility_av %>% 
  filter(type_mobility == "workplaces_av") %>% 
  filter(date >= lockdown_1_end, date <= lockdown_2_start) %>% 
  filter(region=="ENGLAND") #%>%
#select(mobility)

reprod <- react_reprod %>% 
  filter(d_comb >= lockdown_1_end, d_comb <= lockdown_2_start) #%>%
#select(r)


# Plot min max normalised data so that on the same scale
ggplot()+
  geom_line(data=mobility, aes(x=date,y=min_max_normalise(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")



# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

max(ccf_data$ccf)
ccf_data$lag[which.max(ccf_data$ccf)] # 0 days

dtw_lag <- dtw(normalize(mobility$mobility), normalize(reprod$r))
dtw_lag$distance
# 88.87 (whatever that means)
dtw_lag$normalizedDistance
# 0.419
plot(dtw_lag$index1,dtw_lag$index2)

dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r))
dtw_lag$distance
# 13.6 days...a bit better

# CCF  between lockdown 2 and 3------------------------------------------------


# Testing out with workplace mobility

mobility <- mobility_av %>% 
  filter(type_mobility == "workplaces_av") %>% 
  filter(date >= lockdown_2_end, date <= lockdown_3_start) %>% 
  filter(region=="ENGLAND") #%>%
#select(mobility)

reprod <- react_reprod %>% 
  filter(d_comb >= lockdown_2_end, d_comb <= lockdown_3_start) #%>%
#select(r)


# Plot min max normalised data so that on the same scale
ggplot()+
  geom_line(data=mobility, aes(x=date,y=min_max_normalise(mobility)), col="blue")+
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")



# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=-100) %>%
  ggplot(aes(x=lag))+
  # geom_area(aes(y=upper_ci), fill="light grey")+
  # geom_area(aes(y=lower_ci), fill="light grey") +
  geom_line(aes(y=ccf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  annotate("pointrange", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf),ymin=0, ymax=max(ccf_data$ccf), col="red", linetype="dashed")+
  annotate("text", label="Max CCF", x=ccf_data$lag[which.max(ccf_data$ccf)],
           y=max(ccf_data$ccf)+0.02, col="Red") +
  #ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 14), expand=c(0,0)) +
  ggtitle("CCF for England")+
  theme_light()

max(ccf_data$ccf)
ccf_data$lag[which.max(ccf_data$ccf)] # 1 days


dtw_lag <- dtw(normalize(mobility$mobility), normalize(reprod$r))
dtw_lag$distance
# 88.87 (whatever that means)
dtw_lag$normalizedDistance
# 0.419
plot(dtw_lag$index1,dtw_lag$index2)

dtw_lag <- dtw(min_max_normalise(mobility$mobility), min_max_normalise(reprod$r))
dtw_lag$distance
# 4 days??

