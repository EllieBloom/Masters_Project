# Exploring lag with varying length of period

# As with Network Paper 6 - barometer study

# Date started: 16th June 2022

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
# library(astsa) # Time series package used in datacamp course
library(lubridate) # Using to add months to date
# library(lmtest) # For granger causality test
# library(dtw)

# Functions

min_max_normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Useful dates ------------------------------------------------------------

bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                        "%Y-%m-%d")

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

REACT_start <-as.Date("2020-05-01","%Y-%m-%d")

# Looking at London workplace mobility from the start of REACT

region_of_interest <- "LONDON"
mobility_of_interest <- "workplaces"
mobility_of_interest_av <- "workplaces_av"


# Reading tibbles ---------------------------------------------------------

# Loading tibbles ---------------------------------------------------------

mobility_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/mobility_tibble_raw.rds")
summary(mobility_ts)

cases_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/cases_tibble.rds")
summary(cases_ts)

prev_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/prev_tibble.rds")
summary(prev_ts)

prev_smooth_ts <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Tibbles/prev_smooth_tibble.rds")



## Mobility - workplace ----------------------------------------------------


workplace_ts <- mobility_ts %>% filter(region=="LONDON",type_mobility=="workplaces",date>=REACT_start)

workplace_ts$mobility_normalised <- min_max_normalise(workplace_ts$mobility)
summary(workplace_ts$mobility_normalised)

table(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0])
min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

workplace_ts$mobility_normalised[workplace_ts$mobility_normalised==0]<-min(workplace_ts$mobility_normalised[workplace_ts$mobility_normalised!=0])

## REACT prevalence --------------------------------------------------------

london_prev_smooth_ts <- prev_smooth_ts %>% filter(region=="LONDON", d_comb>=REACT_start)

# Normalisation
london_prev_smooth_ts$prev_normalised <- min_max_normalise(london_prev_smooth_ts$p)
summary(london_prev_smooth_ts$prev_normalised)

london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised==0]<-min(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$prev_normalised!=0])
min(london_cases_ts$cases_normalised) 


# Official cases ----------------------------------------------------------

london_cases_ts <- cases_ts %>% filter(region=="LONDON", date>=REACT_start)

# Normalisation
london_cases_ts$cases_normalised <- min_max_normalise(london_cases_ts$cases)
summary(london_cases_ts$cases_normalised)

min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
london_cases_ts$cases_normalised[london_cases_ts$cases_normalised==0]<-min(london_cases_ts$cases_normalised[london_cases_ts$cases_normalised!=0])
min(london_cases_ts$cases_normalised) # Looks like this worked







# CCF with varying length, but same start date ----------------------------

# Total potential window

difftime(lockdown_2_start, REACT_start) #188 days

# Do it as a loop

lags <-c(60,80,100,120,140,160,180)

n_iter <- length(lags)

ccf_interval_summary <- NA



for (i in 1:n_iter){
lag<-lags[i]
start_date <- REACT_start
end_date <-  REACT_start %m+% days(lag)
prev_series <-min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date]))
prev_series <- prev_series[4:(length(prev_series)-3)]
mob_series  <-rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7)
ccf <- ccf(prev_series, mob_series,
              lag.max=lag,na.action=na.pass)
ccf <- as.data.frame(cbind(ccf$acf,ccf$lag))
colnames(ccf)[1:2]<-c("acf","lag")
ccf$interval <- lag
ccf_interval_summary <- rbind(ccf_interval_summary,ccf)
}

ccf_interval_summary <- ccf_interval_summary[-1,]

ccf_interval_summary$interval <- as.factor(ccf_interval_summary$interval)

library(RColorBrewer)
myColors <- brewer.pal(7,"RdBu")
names(myColors) <- levels(ccf_interval_summary$interval)
colScale <- scale_colour_manual(name = "Interval length (days)",values = myColors)

# Use viridis maybe? https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

plot_ccf_varying_interval <- ccf_interval_summary %>% filter(lag>=0, lag<=100) %>%
  ggplot(aes(x=lag, y=acf, color=interval))+
  geom_line() +
  labs(x="Lag (days)",
       y="CCF",
       subtitle="Workplace mobility vs log(prevalence) - start 1st May 2020")+
  ggtitle("CCF plots in London with varying lengths of time interval")+
  theme_light()+
  theme()

plot_ccf_varying_interval

plot_ccf_varying_interval_final <-plot_ccf_varying_interval + colScale
plot_ccf_varying_interval_final



setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Lags")
write.csv(ccf_interval_summary,"ccf_interval_summary.csv")
ggsave("plot_ccf_varying_interval.pdf",plot_ccf_varying_interval_final)



# DTW with varying length, but same start date ----------------------------


difftime(lockdown_2_start, REACT_start) #188 days

# Do it as a loop

lags <-seq(63,182,7)

n_iter <- length(lags)

dtw_interval_summary <- NA

i=1

for (i in 1:n_iter){
  lag<-lags[i]
  start_date <- REACT_start
  end_date <-  REACT_start %m+% days(lag)
  prev_series <-min_max_normalise(log(london_prev_smooth_ts$prev_normalised[london_prev_smooth_ts$d_comb>=start_date & london_prev_smooth_ts$d_comb<end_date]))
  prev_series <- prev_series[4:(length(prev_series)-3)]
  mob_series  <-rollmean(workplace_ts$mobility_normalised[workplace_ts$date>=start_date & workplace_ts$date<end_date],7)
  dtw <- dtw(mob_series, prev_series)
  distance <- dtw$distance
  summary <- cbind(lag,distance)
  dtw_interval_summary <- rbind(dtw_interval_summary,summary)

}

dtw_interval_summary <- dtw_interval_summary[-1,]

colnames(dtw_interval_summary) <- c("interval_length","dtw_distance")
dtw_interval_summary <- as.data.frame(dtw_interval_summary)


plot_dtw_varying_interval <- ggplot(data=dtw_interval_summary,aes(x=interval_length/7,y=dtw_distance/7))+
                              geom_point() + 
                              labs(x="Interval length (weeks)",
                                   y="DTW lag (weeks)")+
                              theme_light() +
                              ggtitle("DTW lag with varying interval length starting 1st May 2020")+
                              geom_smooth(se=FALSE)


setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/Time_series_analysis/Ouputs/Lags")
write.csv(dtw_interval_summary,"dtw_interval_summary.csv")
ggsave("plot_dtw_varying_interval.pdf",plot_dtw_varying_interval)
