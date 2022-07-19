# Understanding the lag between mobility and reproduction number (R)

# Date started: 15th July 2022





# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(stringr)

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
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")
 


# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=0) %>%
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
  geom_line(data=reprod, aes(x=d_comb,y=min_max_normalise(r)), col="green")



# Calculate ccf
ccf <- ccf(reprod$r,mobility$mobility,lag.max=lag_max,na.action=na.pass, pl=FALSE)

ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf_data)[1:2]<-c("ccf","lag")


ccf_data %>% filter(lag>=0) %>%
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




